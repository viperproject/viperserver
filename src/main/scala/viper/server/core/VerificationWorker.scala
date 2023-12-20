// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2023 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import viper.carbon.CarbonFrontend
import viper.server.ViperConfig
import viper.server.vsi.Envelope
import viper.silicon.{Silicon, SiliconFrontend}
import viper.silver.ast._
import viper.silver.frontend.SilFrontend
import viper.silver.reporter.{Reporter, _}
import viper.silver.utility.ViperProgramSubmitter
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

class VerificationWorker(private val command: List[String],
                         private val programId: String,
                         private val program: Program,
                         override val logger: Logger,
                         private val config: ViperConfig)
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
    var success: Boolean = false
    try {
      command match {
        case "silicon" :: args =>
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend("silicon", new SiliconFrontend(new ActorReporter("silicon"), logger), programId, program, disablePlugins = config.disablePlugins())
          backend.execute(args)
          success = true
        case "carbon" :: args =>
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend("carbon", new CarbonFrontend(new ActorReporter("carbon"), logger), programId, program, disablePlugins = config.disablePlugins())
          backend.execute(args)
          success = true
        case "custom" :: custom :: args =>
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(custom, resolveCustomBackend(custom, new ActorReporter(custom)).get, programId, program, disablePlugins = config.disablePlugins())
          backend.execute(args)
          success = true
        case args =>
          logger.error(s"invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
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
        logger.error(s"Creation or execution of the verification backend " +
          s"${if (backend == null) "<null>" else backend.toString} resulted in an exception.", e)
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
    backend.mapEntityVerificationResult(entity, result)
  }

  override def call(): Unit = run()
}

/**
  * ViperBackend that verifies `_ast` using `_frontend` when calling `execute`.
  * Note that we wrap `_frontend` in this way to support custom backends (which are instances of `SilFrontend`).
  */
class ViperBackend(val backendName: String, private val _frontend: SilFrontend, private val programId: String, private val _ast: Program, private val disablePlugins: Boolean = false) {

  // ProgramSubmitter that sends program to viper-data-collection API if _frontend.config.submitForEvaluation enabled
  private val submitter: ViperProgramSubmitter = initSubmitter
  override def toString: String = {
    if ( _frontend.verifier == null )
      s"ViperBackend( ${_frontend.getClass.getName} /with uninitialized verifier/ )"
    else
      s"ViperBackend( ${_frontend.verifier.name} )"
  }

  /** Run the backend verification functionality */
  def execute(args: Seq[String]): Unit = {
    initialize(args)

    /**
      * the architecture is as follows:
      * First, plugins can transform the AST (via their `beforeVerify` methods).
      * Then, caching operates on the AST that would be sent to the verifier for verification. Caching turns methods
      * for which a cache hit exists into abstract methods. The resulting AST is then sent to the verifier.
      * Afterwards, the caching postprocessing happens: The verification results reported by the verifier are added to
      * the cache and are combined with the verification results obtained from the cache for cached methods.
      * As the last step, the combined verification results are handed to the plugins (as `mapVerificationResult`) to
      * back-translate the errors according to the transformation they performed in `beforeVerify`.
      *
      * Caching is thus transparent to clients of ViperServer and in particular plugins. Additionally, plugins creating
      * multiple methods benefit from partial caching: Imagine a termination plugin that creates additional methods to
      * check termination. For small modifications to the input programs, a method might have changed (and thus lead to
      * a cache miss) but as long long as the method checking for termination stays the same, the termination check might
      * still be cached.
      *
      * Note however that the `cached` flag for verification errors is best effort: Since caching is (almost) transparent
      * to plugins, it might sometimes not be possible for a plugin to correctly set the cache flag. E.g., the refute
      * plugin can see the cached flag for assertion failures when mapping the verification results. Since it's not
      * aware which errors belong to which member, the plugin cannot set the `cached` flag for refute errors, i.e.
      * verification errors that were expected but did not occur.
      */
    val overallResult = try {
      val res = for {
        filteredProgram <- filter(_ast)
        innerProgram <- beforeVerify(filteredProgram)
        cachingResult = caching(innerProgram)
        verificationResult <- verification(cachingResult.transformedProgram)
        combinedVerificationResult = postCaching(cachingResult, verificationResult)
        mappedVerificationResult = mapVerificationResult(innerProgram, combinedVerificationResult)
      } yield mappedVerificationResult
      turnEitherIntoVerificationResult(res)
    } finally {
      stop()
      submitter.submit()
    }

    finish(overallResult)
  }

  /** initializes frontend such that the associated verifier is ready for verification */
  private def initialize(args: Seq[String]): Unit = {
    // --ignoreFile is not enough as Silicon still tries to parse the provided filepath unless
    // the following dummy file is used instead (see Silicon issue #552):
    val argsWithDummyFilename = args ++ Seq("--ignoreFile", Silicon.dummyInputFilename)
    _frontend.setStartTime()
    _frontend.setVerifier( _frontend.createVerifier(argsWithDummyFilename.mkString(" ")) )

    if (!_frontend.prepare(argsWithDummyFilename)) {
      return
    }
    // Initialize plugins based on the configuration that was just created from the passed arguments.
    _frontend.resetPlugins()
    _frontend.init( _frontend.verifier )

    submitter.setArgs(args.toArray)
  }

  private def filter(input: Program): Either[Seq[AbstractError], Program] = {
    if (disablePlugins) Right(input)
    else _frontend.plugins.beforeMethodFilter(input) match {
      case Some(inputPlugin) =>
        // Filter methods according to command-line arguments.
        val verifyMethods =
          if (_frontend.config != null && _frontend.config.methods() != ":all") Seq("methods", _frontend.config.methods())
          else inputPlugin.methods map (_.name)
        val methods = inputPlugin.methods filter (m => verifyMethods.contains(m.name))
        Right(Program(inputPlugin.domains, inputPlugin.fields, inputPlugin.functions, inputPlugin.predicates, methods, inputPlugin.extensions)(inputPlugin.pos, inputPlugin.info, inputPlugin.errT))

      case None => Left(_frontend.plugins.errors)
    }
  }

  case class CachingResult(transformedProgram: Program, cachedErrors: Seq[VerificationError])

  private def caching(input: Program): CachingResult = {
    if (_frontend.config.disableCaching()) {
      // NOP
      return CachingResult(input, Seq.empty)
    }

    val (transformed_prog, cached_results) = ViperCache.applyCache(backendName, programId, input)

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

    CachingResult(transformed_prog, all_cached_errors.toSeq)
  }

  private def beforeVerify(input: Program): Either[Seq[AbstractError], Program] = {
    if (disablePlugins) Right(input)
    else _frontend.plugins.beforeVerify(input) match {
      case Some(programPlugin) => Right(programPlugin)
      case None => Left(_frontend.plugins.errors)
    }
  }

  private def verification(input: Program): Either[Seq[AbstractError], VerificationResult] =
    Right(_frontend.verifier.verify(input))

  def mapEntityVerificationResult(entity: Entity, verificationResult: VerificationResult): VerificationResult = {
    if (disablePlugins) verificationResult
    else _frontend.plugins.mapEntityVerificationResult(entity, verificationResult)
  }

  private def mapVerificationResult(input: Program, result: VerificationResult): VerificationResult =
    if (disablePlugins) result
    else _frontend.plugins.mapVerificationResult(input, result)

  private def postCaching(cachingResult: CachingResult, verificationResult: VerificationResult): VerificationResult = {
    if (_frontend.config.disableCaching()) {
      // NOP
      return verificationResult
    }

    val astAfterApplyingCache = cachingResult.transformedProgram
    val methodsToVerify: Seq[Method] = astAfterApplyingCache.methods.filter(_.body.isDefined)

    val meth_to_err_map: Seq[(Method, Option[List[AbstractVerificationError]])] = methodsToVerify.map((m: Method) => {
      // Results come back irrespective of program Member.
      val cacheable_errors: Option[List[AbstractVerificationError]] = for {
        cache_errs <- verificationResult match {
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
      verificationResult match {
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
          ViperCache.update(backendName, programId, m, astAfterApplyingCache, cacheable_errors.get) match {
            case e :: es =>
              _frontend.logger.trace(s"Storing new entry in cache for method '${m.name}' and backend '$backendName': $e. Other entries for this method: ($es)")
            case Nil =>
              _frontend.logger.trace(s"Storing new entry in cache for method '${m.name}' and backend '$backendName' FAILED.")
          }
        }
      }
    } else {
      _frontend.logger.warn(s"Inconsistent error splitting; no cache update for this verification attempt with ProgramID $programId.")
    }

    // Write cache to file at the end of a verification run
    ViperCache.writeToFile()

    // combine errors:
    if (cachingResult.cachedErrors.isEmpty) {
      verificationResult // verificationResult remains unchanged as the cached errors (which are none) do not change the outcome
    } else {
      verificationResult match {
        case Failure(errorList) =>
          Failure(errorList ++ cachingResult.cachedErrors)
        case Success =>
          Failure(cachingResult.cachedErrors)
      }
    }
  }

  /**
    * LA Jan 5 2023: unclear whether this is necessary at all. SilFrontend neither calls start nor stop on the verifier and ViperBackend used to call only stop in the past.
    */
  private def stop(): Unit = {
    _frontend.verifier.stop()
  }

  private def finish(verificationResult: VerificationResult): Unit = {
    val finishedResult = if (disablePlugins) verificationResult else _frontend.plugins.beforeFinish(verificationResult)
    finishedResult match {
      case Success =>
        _frontend.logger.debug(s"Verification successful (members: ${_ast.members.map(_.name).mkString(", ")})")
        _frontend.reporter report OverallSuccessMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime)
        // TODO: Think again about where to detect and trigger SymbExLogging
      case f: Failure =>
        // Cached errors will be reported as soon as they are retrieved from the cache.
        _frontend.logger.debug(s"Verification has failed (errors: ${f.errors.map(_.readableMessage).mkString("\n")}; members: ${_ast.members.map(_.name).mkString(", ")})")
        _frontend.reporter report OverallFailureMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime, f)
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

  private def turnEitherIntoVerificationResult(input: Either[Seq[AbstractError], VerificationResult]): VerificationResult = {
    input match {
      case Left(errs) => Failure(errs)
      case Right(res) => res
    }
  }

  private def initSubmitter: ViperProgramSubmitter = {
    val s = new ViperProgramSubmitter(_frontend) {
      override def originalFrontend: String = "ViperServer"
    }
    s.setName(programId.split("/").last)
    s.setProgram(_ast)
    s
  }
}
