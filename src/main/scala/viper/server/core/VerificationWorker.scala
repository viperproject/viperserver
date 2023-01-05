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
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.{Reporter, _}
import viper.silver.verifier.{AbstractVerificationError, VerificationResult, _}

import java.nio.file.Path
import scala.collection.mutable.ListBuffer

class ViperServerException extends Exception
case class ViperServerWrongTypeException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name`."
}
case class ViperServerBackendNotFoundException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be found."
}

case class ViperEnvelope(m: Message) extends Envelope

class VerificationWorker(private val command: List[String],
                         private val programId: String,
                         private val program: Program,
                         override val logger: Logger)
                        (override val executor: VerificationExecutionContext)
  extends MessageReportingTask[Unit] {

  private var frontend: Option[VerificationOnlySilFrontendWithCaching] = None

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
    var success: Boolean = false
    try {
      command match {
        case "silicon" :: args =>
          /*
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend("silicon", new SiliconFrontend(new ActorReporter("silicon"), logger), programId, program)
          backend.execute(args)
          success = true
          */

          logger.info("Creating new Silicon verification backend.")
          frontend = Some(new VerificationOnlySiliconFrontendWithCaching(new ActorReporter("silicon"), logger, "silicon", programId, program))
          frontend.get.execute(args ++ Seq("--ignoreFile", Silicon.dummyInputFilename))
          success = true
        case "carbon" :: args =>
          /*
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend("carbon", new CarbonFrontend(new ActorReporter("carbon"), logger), programId, program)
          backend.execute(args)
          success = true
          */
          logger.info("Creating new Carbon verification backend.")
          frontend = Some(new VerificationOnlyCarbonFrontendWithCaching(new ActorReporter("carbon"), logger, "carbon", programId, program))
          frontend.get.execute(args ++ Seq("--ignoreFile", Silicon.dummyInputFilename))
          success = true
          // TODO add support for custom backends
          /*
        case "custom" :: custom :: args =>
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(custom, resolveCustomBackend(custom, new ActorReporter(custom)).get, programId, program)
          backend.execute(args)
          success = true
        case args =>
          logger.error(s"invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
            */
      }
    } catch {
      case _: InterruptedException =>
      case _: java.nio.channels.ClosedByInterruptException =>
      case e: Throwable =>
        enqueueMessage(ExceptionReport(e))
        val stackTraceElements = e.getStackTrace
        for (stackTrace <- stackTraceElements) {
          logger.error(stackTrace.getClassName + "  " + stackTrace.getMethodName + " " + stackTrace.getLineNumber)
        }
        logger.error(s"Creation/Execution of the verification backend " +
          s"${frontend.map(_.backendName).getOrElse("<none>")} resulted in an exception.", e)
    }
    if (success) {
      logger.info(s"The command `${command.mkString(" ")}` has been executed.")
      registerTaskEnd(true)
    } else {
      logger.error(s"The command `${command.mkString(" ")}` resulted in an exception.")
      registerTaskEnd(false)
    }
  }

  override def mapEntityVerificationResult(entity: Entity, result: VerificationResult): VerificationResult = {
    frontend
      .getOrElse(throw new IllegalStateException("frontend has not been initialized yet"))
      .mapEntityVerificationResult(entity, result)
  }

  override def call(): Unit = run()
}

class VerificationOnlySiliconFrontendWithCaching(override val reporter: Reporter,
                                                 override implicit val logger: Logger = ViperStdOutLogger("VerificationOnlySiliconFrontendWithCaching", "INFO").get,
                                                 override val backendName: String, override protected val programId: String, override protected val _ast: Program)
  extends SiliconFrontend(reporter, logger) with VerificationOnlySilFrontendWithCaching

class VerificationOnlyCarbonFrontendWithCaching(override val reporter: Reporter,
                                                 override implicit val logger: Logger = ViperStdOutLogger("VerificationOnlyCarbonFrontendWithCaching", "INFO").get,
                                                 override val backendName: String, override protected val programId: String, override protected val _ast: Program)
  extends CarbonFrontend(reporter, logger) with VerificationOnlySilFrontendWithCaching

/**
  * special frontend that expects an AST (i.e. Parsing, Semantic Analysis, Translation, and Consistency Check have already happened;
  * e.g. by the ViperAstProvider) and supports caching (if enabled via the config)
  */
trait VerificationOnlySilFrontendWithCaching extends SilFrontend {

  val backendName: String
  protected val programId: String
  protected val _ast: Program

  val Caching: Phase = Phase("Caching", caching _)
  val PostCaching: Phase = Phase("PostCaching", postCaching _)

  // only perform caching and verification
  override val phases: Seq[Phase] = Seq(Caching, Verification, PostCaching)

  var _astAfterApplyingCache: Program = _ast // note that this can deviate from _program since plugins might transform the AST further
  var _all_cached_errors: Seq[VerificationError] = Seq.empty

  /** Transform the Viper AST to filter out cached members */
  def caching(): Unit = {
    if (state == DefaultStates.ConsistencyCheck && _errors.isEmpty && !config.disableCaching()) {
      val (transformed_prog, cached_results) = ViperCache.applyCache(backendName, programId, _ast)

      // collect and report errors
      val all_cached_errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
      cached_results.foreach((result: CacheResult) => {
        val cached_errors = result.verification_errors
        if (cached_errors.isEmpty) {
          reporter report
            CachedEntityMessage(backendName, result.method, Success)
        } else {
          all_cached_errors ++= cached_errors
          reporter report
            CachedEntityMessage(backendName, result.method, Failure(cached_errors))
        }
      })

      val methodsToVerify: Seq[Method] = transformed_prog.methods.filter(_.body.isDefined)
      logger.debug(
        s"Retrieved data from cache..." +
          s" cachedErrors: ${all_cached_errors.map(_.loggableMessage)};" +
          s" cachedMethods: ${cached_results.map(_.method.name)};" +
          s" methodsToVerify: ${methodsToVerify.map(_.name)}.")
      logger.trace(s"The cached program is equivalent to: \n${transformed_prog.toString()}")

      _program = Some(transformed_prog)
      _astAfterApplyingCache = transformed_prog
      _all_cached_errors = all_cached_errors.toSeq
    }
  }

  /** merges cached and new verification results */
  def postCaching(): Unit = {
    val methodsToVerify: Seq[Method] = _astAfterApplyingCache.methods.filter(_.body.isDefined)

    val meth_to_err_map: Seq[(Method, Option[List[AbstractVerificationError]])] = methodsToVerify.map((m: Method) => {
      // Results come back irrespective of program Member.
      val cacheable_errors: Option[List[AbstractVerificationError]] = for {
        cache_errs <- result match {
          case Failure(errs) =>
            val r = getMethodSpecificErrors(m, errs)
            logger.debug(s"getMethodSpecificErrors returned $r")
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
      result match {
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
        logger.debug(s"Obtained cacheable errors: $cacheable_errors")

        if (cacheable_errors.isDefined) {
          ViperCache.update(backendName, programId, m, _astAfterApplyingCache, cacheable_errors.get) match {
            case e :: es =>
              logger.trace(s"Storing new entry in cache for method '${m.name}' and backend '$backendName': $e. Other entries for this method: ($es)")
            case Nil =>
              logger.trace(s"Storing new entry in cache for method '${m.name}' and backend '$backendName' FAILED.")
          }
        }
      }
    } else {
      logger.warn(s"Inconsistent error splitting; no cache update for this verification attempt with ProgramID $programId.")
    }

    // Write cache to file at the end of a verification run
    ViperCache.writeToFile()

    // combine errors:
    if (_all_cached_errors.nonEmpty) { // in the other case, _verificationResult remains unchanged as the cached errors (which are none) do not change the outcome
      _verificationResult.get match {
        case Failure(errorList) =>
          _verificationResult = Some(Failure(errorList ++ _all_cached_errors))
        case Success =>
          _verificationResult = Some(Failure(_all_cached_errors))
      }
    }
  }

  // the following implementation is identical to the one provided in DefaultFrontend except that `input` is ignored
  override def reset(input: Path): Unit = {
    if (state < DefaultStates.Initialized) sys.error("The translator has not been initialized.")
    _state = DefaultStates.ConsistencyCheck
    _inputFile = None
    _input = None
    _errors = Seq()
    _parsingResult = None
    _semanticAnalysisResult = None
    _verificationResult = None
    _program = Some(_ast)
    resetMessages()
  }

  def mapEntityVerificationResult(entity: Entity, verificationResult: VerificationResult): VerificationResult = {
    plugins.mapEntityVerificationResult(entity, verificationResult)
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
        logger.debug(s"Backend aborted exceptionally - this error is not attributed to any program member", cause)
        return None
      case e =>
        throw new Exception("Error with unexpected type found: " + e)
    }
    Some(result.toList)
  }
}
