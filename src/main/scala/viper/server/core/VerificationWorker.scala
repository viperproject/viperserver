// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import viper.carbon.CarbonFrontend
import viper.server.vsi.Envelope
import viper.silicon.{Silicon, SiliconFrontend}
import viper.silver.ast._
import viper.silver.frontend.{DefaultStates, SilFrontend}
import viper.silver.reporter.{Reporter, _}
import viper.silver.verifier.{AbstractVerificationError, VerificationResult, _}

import scala.collection.mutable.ListBuffer

class ViperServerException extends Exception
case class ViperServerWrongTypeException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name`."
}
case class ViperServerBackendNotFoundException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be found."
}

case class ViperEnvelope(m: Message) extends Envelope

class VerificationWorker(override val logger: Logger,
                         private val programId: String,
                         private val command: List[String],
                         private val program: Program)
                        (override val executor: VerificationExecutionContext)
  extends MessageReportingTask[Unit] {

  private var backend: ViperBackend = _

  private def resolveCustomBackend(clazzName: String, rep: Reporter): Option[SilFrontend] = {
    (try {
      val constructor = Class.forName(clazzName).getConstructor(
        classOf[viper.silver.reporter.Reporter],
        classOf[ch.qos.logback.classic.Logger])
      Some(constructor.newInstance(rep, logger))
    } catch {
      case _: ClassNotFoundException => None
    })
    match {
      case Some(instance) if instance.isInstanceOf[SilFrontend] =>
        Some(instance.asInstanceOf[SilFrontend])
      case Some(instance) =>
        throw ViperServerWrongTypeException(instance.getClass.getName)
      case _ =>
        throw ViperServerBackendNotFoundException(clazzName)
    }
  }

  def run(): Unit = {
    try {
      command match {
        case "silicon" :: args =>
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend(new SiliconFrontend(new ActorReporter("silicon"), logger), programId, program)
          backend.execute(args)
        case "carbon" :: args =>
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend(new CarbonFrontend(new ActorReporter("carbon"), logger), programId, program)
          backend.execute(args)
        case custom :: args =>
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(resolveCustomBackend(custom, new ActorReporter(custom)).get, programId, program)
          backend.execute(args)
        case args =>
          logger.error(s"invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
      }
    } catch {
      case _: InterruptedException =>
      case _: java.nio.channels.ClosedByInterruptException =>
      case e: Throwable =>
        enqueueMessage(ExceptionReport(e))
        logger.trace(s"Creation/Execution of the verification backend " +
          s"${if (backend == null) "<null>" else backend.toString} resulted in an exception.", e)
    } finally {
      try {
        backend.stop()
      } catch {
        case e: Throwable =>
          logger.trace(s"Stopping the verification backend ${if (backend == null) "<null>" else backend.toString} resulted in exception.", e)
      }
    }
    if (backend != null) {
      logger.info(s"The command `${command.mkString(" ")}` has been executed.")
      registerTaskEnd(true)
    } else {
      logger.error(s"The command `${command.mkString(" ")}` did not result in initialization of verification backend.")
      registerTaskEnd(false)
    }
  }

  override def call(): Unit = run()
}

class ViperBackend(private val _frontend: SilFrontend, private val programId: String, private val _ast: Program) {

  def backendName: String = _frontend.verifier.getClass.getName

  override def toString: String = {
    if ( _frontend.verifier == null )
      s"ViperBackend( ${_frontend.getClass.getName} /with uninitialized verifier/ )"
    else
      s"ViperBackend( ${_frontend.verifier.name} )"
  }

  /** Run the backend verification functionality
    * */
  def execute(args: Seq[String]): Unit = {
    // --ignoreFile is not enough as Silicon still tries to parse the provided filepath unless
    // the following dummy file is used:
    val argsWithDummyFilename = args ++ Seq("--ignoreFile", Silicon.dummyInputFilename)
    _frontend.setStartTime()
    _frontend.setVerifier( _frontend.createVerifier(argsWithDummyFilename.mkString(" ")) )

    if (!_frontend.prepare(argsWithDummyFilename)) return
    _frontend.init( _frontend.verifier )

    val temp_result: Option[VerificationResult] = if (_frontend.config.disableCaching()) {
      _frontend.logger.info("Verification with caching disabled")
      Some(_frontend.verifier.verify(_ast))
    } else {
      _frontend.logger.info("Verification with caching enabled")
      doCachedVerification(_ast)
      _frontend.getVerificationResult
    }

    temp_result match {
      case Some(Success) =>
        _frontend.logger.debug(s"Verification successful (members: ${_ast.members.map(_.name).mkString(", ")})")
        _frontend.reporter report OverallSuccessMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime)
      // TODO: Think again about where to detect and trigger SymbExLogging
      case Some(f@Failure(_)) =>
        // Cached errors will be reported as soon as they are retrieved from the cache.
        _frontend.logger.debug(s"Verification has failed (errors: ${f.errors.map(_.readableMessage).mkString("\n")}; members: ${_ast.members.map(_.name).mkString(", ")})")
        _frontend.reporter report OverallFailureMessage(_frontend.getVerifierName,
                                                        System.currentTimeMillis() - _frontend.startTime,
                                                        Failure(f.errors.filter { e => !e.cached }))
      case None =>
    }
    _frontend.verifier.stop()
  }

  private def doCachedVerification(real_program: Program): Unit = {
    /** Top level branch is here for the same reason as in
      * {{{viper.silver.frontend.DefaultFrontend.verification()}}} */

    val (transformed_prog, cached_results) = ViperCache.applyCache(backendName, programId, real_program)

    // collect and report errors
    val all_cached_errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
    cached_results.foreach((result: CacheResult) => {
      val cached_errors = result.verification_errors
      if (cached_errors.isEmpty) {
        _frontend.reporter report
          CachedEntityMessage(_frontend.getVerifierName, result.method, Success)
      } else {
        all_cached_errors ++= cached_errors
        _frontend.reporter report
          CachedEntityMessage(_frontend.getVerifierName, result.method, Failure(cached_errors))
      }
    })

    val methodsToVerify: Seq[Method] = transformed_prog.methods.filter(_.body.isDefined)

    _frontend.logger.debug(
      s"Retrieved data from cache..." +
        s" cachedErrors: ${all_cached_errors.map(_.loggableMessage)};" +
        s" cachedMethods: ${cached_results.map(_.method.name)};" +
        s" methodsToVerify: ${methodsToVerify.map(_.name)}.")
    _frontend.logger.trace(s"The cached program is equivalent to: \n${transformed_prog.toString()}")

    val ver_result: VerificationResult = _frontend.verifier.verify(transformed_prog)
    _frontend.setVerificationResult(ver_result)
    _frontend.setState(DefaultStates.Verification)

    _frontend.logger.debug(s"Latest verification result: $ver_result")

    val meth_to_err_map: Seq[(Method, Option[List[AbstractVerificationError]])] = methodsToVerify.map((m: Method) => {
      // Results come back irrespective of program Member.
      val cacheable_errors: Option[List[AbstractVerificationError]] = for {
        verRes <- _frontend.getVerificationResult
        cache_errs <- verRes match {
          case Failure(errs) =>
            val r = getMethodSpecificErrors(m, errs)
            _frontend.logger.debug(s"getMethodSpecificErrors returned $r")
            r
          case Success =>
            Some(Nil)
        }
      } yield cache_errs

      (m, cacheable_errors)
    })

    // Check that the mapping from errors to methods is not messed up
    // (otherwise it is unsafe to cache the results)
    val update_cache_criterion: Boolean = {
      val all_errors_in_file = meth_to_err_map.flatMap(_._2).flatten
      _frontend.getVerificationResult.get match {
        case Success =>
          all_errors_in_file.isEmpty
        case Failure(errors) =>
          // FIXME find a better sorting criterion
          errors.sortBy(ae => ae.hashCode()) == all_errors_in_file.sortBy(ae => ae.hashCode())
      }
    }

    if (update_cache_criterion) {
      // update cache
      meth_to_err_map.foreach { case (m: Method, cacheable_errors: Option[List[AbstractVerificationError]]) =>
        _frontend.logger.debug(s"Obtained cacheable errors: $cacheable_errors")

        if (cacheable_errors.isDefined) {
          ViperCache.update(backendName, programId, m, transformed_prog, cacheable_errors.get) match {
            case e :: es =>
              _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}): $e. Other entries for this method: ($es)")
            case Nil =>
              _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}) FAILED.")
          }
        }
      }
    } else {
      _frontend.logger.debug(s"Inconsistent error splitting; no cache update for this verification attempt with ProgramID $programId.")
    }

    // combine errors:
    if (all_cached_errors.nonEmpty) {
      _frontend.getVerificationResult.get match {
        case Failure(errorList) =>
          _frontend.setVerificationResult(Failure(errorList ++ all_cached_errors))
        case Success =>
          _frontend.setVerificationResult(Failure(all_cached_errors.toSeq))
      }
    }
  }

  /**
    * Tries to determine which of the given errors are caused by the given method.
    * If an error's scope field is set, this information is used.
    * Otherwise, if both the given method and the error have line/column position information, we calculate if the error's
    * position is inside the method.
    * If at least one error has no scope set and no position, or the method's position is not set, we cannot determine
    * if the error belongs to the method and return None.
    */
  private def getMethodSpecificErrors(m: Method, errors: Seq[AbstractError]): Option[List[AbstractVerificationError]] = {
    val methodPos: Option[(Int, Int)] = m.pos match {
      case sp: SourcePosition =>
        /** Only the line component matters (not the column) since,
          * in Viper, each method must be declared on a new line. */
        Some(sp.start.line, sp.end.get.line)
      case _ =>
        None
    }
    val result = scala.collection.mutable.ListBuffer[AbstractVerificationError]()

    errors.foreach {
      case e: AbstractVerificationError if e.scope.isDefined =>
        if (e.scope.get == m)
          result += e
      case e: AbstractVerificationError =>
        e.pos match {
          case pos: HasLineColumn =>
            val errorPos = pos.line
            if (methodPos.isEmpty) {
              return None
            }
            // The position of the error is used to determine to which Method it belongs.
            if (methodPos.get._1 <= errorPos && errorPos <= methodPos.get._2) {
              result += e
            }
          case _ =>
            return None
        }
      case AbortedExceptionally(cause) =>
        _frontend.logger.debug(s"Backend aborted exceptionally - this error is not attributed to any program member", cause)
        return None
      case e =>
        throw new Exception("Error with unexpected type found: " + e)
    }
    Some(result.toList)
  }

  def stop(): Unit = _frontend.verifier.stop()
}