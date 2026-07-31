// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import scala.language.postfixOps
import akka.actor.{PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import ch.qos.logback.classic.Logger
import viper.server.ViperConfig
import viper.server.core.{SiliconConfig, VerificationExecutionContext, ViperBackendConfig, ViperCoreServer}
import viper.server.frontends.lsp.debug.DebugDrainActor
import viper.server.utility.ReformatterAstGenerator
import viper.server.utility.Helpers.{getArgListFromArgString, validateViperFile}
import viper.server.utility.Helpers.validateViperFile
import viper.server.vsi.VerificationProtocol.{StopAstConstruction, StopVerification}
import viper.server.vsi.{AstJobId, DefaultVerificationServerStart, VerHandle, VerJobId}
import viper.silicon.Silicon
import viper.silicon.debugger.SiliconDebugSession
import viper.silver.parser.ReformatPrettyPrinter
import viper.silver.ast.{AbstractSourcePosition, Method, Program}
import viper.silver.ast.utility.FileLoader

import scala.concurrent.Future
import scala.concurrent.duration._

class ViperServerService(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with DefaultVerificationServerStart {

  def constructAst(file: String, backend: ViperBackendConfig, localLogger: Option[Logger] = None, loader: Option[FileLoader]): AstJobId = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to start new job...")

    if (!validateViperFile(file)) {
      logger.debug(s"file not found: $file")
      return AstJobId(-1)
    }

    requestAst(file, backend, localLogger, loader)
  }

  def verifyAst(astJob: AstJobId, file: String, backend: ViperBackendConfig, localLogger: Option[Logger] = None): VerJobId = {
    if (astJob.id < 0) {
      return VerJobId(-1)
    }
    val logger = combineLoggers(localLogger)

    val ver_id = verifyWithAstJob(file, astJob, backend, localLogger)
    if (ver_id.id >= 0) {
      logger.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  def reformatFile(file: String, localLogger: Option[Logger] = None): Option[String] = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to create a reformatted file.");

    val ast_generator = new ReformatterAstGenerator(logger);
    val parse_ast = ast_generator.generateViperParseAst(file);
    parse_ast match {
      case Some(p) => Some(ReformatPrettyPrinter.showProgram(p))
      case _ => {
        logger.error("Failed to generate parse AST for reformatting the program.")
        None
      }
    }
  }

  def startStreaming(jid: VerJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    logger.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor, include_ast = true).map(_.map(_ => ()))
  }
  def startStreamingAst(jid: AstJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    val relay_actor = system.actorOf(relayActor_props)
    logger.debug(s"Sending ast construct request to ViperServer... (${relay_actor.toString()})")
    streamMessages(jid, relay_actor).map(_.map(_ => ()))
  }
  def startStreamingVer(jid: VerJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    val relay_actor = system.actorOf(relayActor_props)
    logger.debug(s"Sending verification request to ViperServer... (${relay_actor.toString()})")
    streamMessages(jid, relay_actor, include_ast = false).map(_.map(_ => {
      logger.debug("Done verification request to ViperServer...")
      ()
    }))
  }

  def stopVerification(jid: VerJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val logger = combineLoggers(localLogger)
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        // Free the ver slot so new jobs can be added immediately
        ver_jobs.discardJob(jid)
        handle_future.flatMap(handle => {
          // Stop ast construction
          handle.prev_job_id.foreach(astJobId => stopAstConstruction(astJobId, localLogger))
          stopOnlyVerification(handle, logger)
            .map(verResult => {
              logger.info(s"verification stopped for job #$jid")
              verResult
            })
        })
      case _ =>
        // Did not find a job with this jid.
        logger.warn(s"stopVerification - The verification job #$jid does not exist and can thus not be stopped.")
        Future.successful(false)
    }
  }

  private def stopOnlyVerification(handle: VerHandle, combinedLogger: Logger): Future[Boolean] = {
    handle match {
      // If AST construction failed, a verification handle will be returned where the actor field is null.
      case VerHandle(null, _, _, _) => Future.successful(false)
      case _ => {
        implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
        val interrupt: Future[String] = (handle.job_actor ? StopVerification).mapTo[String]
        handle.job_actor ! PoisonPill // the actor played its part.
        interrupt.map(msg => {
          combinedLogger.info(msg)
          true
        })
      }
    }
  }

  // Discards an AST job if it exists, the job will keep running but frees up a slot in the allowed number of jobs.
  def discardAstJobLookup(jid: AstJobId): Unit = {
    ast_jobs.lookupJob(jid).map({job =>
      ast_jobs.discardJob(jid)
      job.map(astHandle => astHandle.queue.watchCompletion().onComplete(_ => {
        astHandle.job_actor ! PoisonPill
      }))
    })
  }

  def stopAstConstruction(jid: AstJobId, localLogger: Option[Logger] = None): Unit = {
    stopOnlyAstConstruction(jid, localLogger).map { found =>
      if (found) discardAstJob(jid)
    }
  }

  def stopOnlyAstConstruction(jid: AstJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val combinedLogger = combineLoggers(localLogger)
    ast_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.map { handle =>
          handle.job_actor ! StopAstConstruction
          handle.job_actor ! PoisonPill // the actor played its part.
          combinedLogger.info(s"ast construction stopped for job #$jid")
          true
        }
      case _ =>
        // Did not find a job with this jid.
        combinedLogger.warn(s"stopVerification - The AST construction job #$jid does not exist and can thus not be stopped.")
        Future.successful(false)
    }
  }

  /**
    * Runs a verification with debugging enabled and returns the resulting debug session together with the
    * Silicon instance that produced it. The caller owns both and must stop the Silicon instance when done
    * (see [[viper.server.frontends.lsp.debug.ServerDebugSession]]).
    *
    * Since debugging requires the whole symbolic execution to be tracked, this is a separate verification run
    * rather than a reuse of the one whose diagnostics the user clicked on. Caching is disabled for it, because
    * a cache hit would replace the failing method by an abstract one and no debug information would be
    * produced.
    *
    * @param selectMemberAt if given, a (1-based) position; the member containing it is verified on its own,
    *                       which is much faster. If that run does not reproduce a debuggable failure, the
    *                       whole file is verified instead.
    */
  def startDebugVerification(file: String,
                             customArgs: String,
                             selectMemberAt: Option[(Int, Int)],
                             withCounterexample: Boolean,
                             loader: Option[FileLoader],
                             onProgress: String => Unit,
                             localLogger: Option[Logger] = None): Future[Either[String, (Silicon, SiliconDebugSession)]] = {
    val logger = combineLoggers(localLogger)
    val baseArgs = getArgListFromArgString(customArgs)

    onProgress("Constructing the AST...")
    val astJob = constructAst(file, SiliconConfig(baseArgs), localLogger, loader)
    if (astJob.id < 0) {
      return Future.successful(Left("Could not start AST construction; too many jobs are running."))
    }
    startStreamingAst(astJob, DebugDrainActor.props(onProgress), localLogger)

    val programFut: Future[Option[Program]] = ast_jobs.lookupJob(astJob) match {
      case Some(handleFut) => handleFut.flatMap(_.artifact)
      case None => Future.successful(None)
    }

    programFut.flatMap { programOpt =>
      discardAstJobLookup(astJob)
      programOpt match {
        case None =>
          Future.successful(Left("The file could not be parsed or type-checked."))
        case Some(program) =>
          val member = selectMemberAt.flatMap(pos => memberAt(program, pos))
          runDebugVerification(file, baseArgs, program, member, withCounterexample, onProgress, localLogger).flatMap {
            case Left(err) if member.isDefined =>
              logger.info(s"Debugging only member '${member.get}' did not work ($err); verifying the whole file.")
              onProgress(s"Verifying only ${member.get} did not reproduce the error; verifying the whole file...")
              runDebugVerification(file, baseArgs, program, None, withCounterexample, onProgress, localLogger)
            case other =>
              Future.successful(other)
          }
      }
    }
  }

  private def runDebugVerification(file: String,
                                   baseArgs: List[String],
                                   program: Program,
                                   member: Option[String],
                                   withCounterexample: Boolean,
                                   onProgress: String => Unit,
                                   localLogger: Option[Logger]): Future[Either[String, (Silicon, SiliconDebugSession)]] = {
    val args = debugArgs(baseArgs, member, withCounterexample)
    combineLoggers(localLogger).info(s"Starting a debug verification: silicon ${args.mkString(" ")}")
    onProgress(member match {
      case Some(m) => s"Verifying $m with debugging enabled..."
      case None => "Verifying with debugging enabled..."
    })

    val (verId, siliconFut) = verifyForDebugging(file, SiliconConfig(args), program, localLogger)
    if (verId.id < 0) {
      return Future.successful(Left("Could not start a verification process; too many jobs are running."))
    }
    startStreamingVer(verId, DebugDrainActor.props(onProgress), localLogger)

    siliconFut.map {
      case Left(err) => Left(err)
      case Right(silicon) =>
        silicon.debugSession.filter(_.hasDebuggableFailure) match {
          case Some(session) =>
            Right((silicon, session))
          case None =>
            val reason = silicon.debugSession match {
              case Some(s) if s.failures.nonEmpty =>
                s"the verification reported ${s.failures.size} error(s), but none of them carry debug information"
              case Some(_) => "the verification succeeded this time, so there is nothing to debug"
              case None => "the verification did not run with debugging enabled"
            }
            silicon.stop()
            Left(s"No proof obligation is available: $reason.")
        }
    }
  }

  /**
    * The command line of a debug run.
    *
    * The IDE's custom arguments end with the file to verify, and Silicon's argument parser ignores options
    * that follow that positional argument — so the flags of the debug run have to come first. They also must
    * not be passed twice, hence the flags we set ourselves are removed from the user's arguments.
    */
  private def debugArgs(baseArgs: List[String], member: Option[String], withCounterexample: Boolean): List[String] = {
    val ownFlags = Set("--enableDebugging", "--disableCaching")
    val cleaned = List("--select", "--counterexample", "--exhaleMode")
      .foldLeft(baseArgs.filterNot(ownFlags.contains))(withoutOption)
    // Counterexamples need the prover to keep its models, and are far more informative with the more complete
    // exhale mode, which keeps the permissions of the heap around.
    val counterexampleArgs =
      if (withCounterexample) List("--counterexample", "mapped", "--exhaleMode", "1") else Nil
    List("--enableDebugging", "--disableCaching") ++
      counterexampleArgs ++
      member.map(m => List("--select", m)).getOrElse(Nil) ++
      cleaned
  }

  /** Drops every occurrence of the given option together with its value. */
  private def withoutOption(args: List[String], name: String): List[String] = args match {
    case `name` :: _ :: rest => withoutOption(rest, name)
    case arg :: rest => arg :: withoutOption(rest, name)
    case Nil => Nil
  }

  /** The name of the method that contains the given (1-based) position, if any. */
  private def memberAt(program: Program, pos: (Int, Int)): Option[String] = {
    val (line, _) = pos
    program.methods.collectFirst {
      case m if m.body.isDefined && containsLine(m, line) => m.name
    }
  }

  private def containsLine(m: Method, line: Int): Boolean = m.pos match {
    case sp: AbstractSourcePosition => sp.start.line <= line && line <= sp.end.getOrElse(sp.start).line
    case _ => false
  }

  def isSupportedType(t: String): Boolean = {
    if (t == null) {
      return false
    }
    t.toLowerCase() == "carbon" || t.toLowerCase() == "silicon" || t.toLowerCase() == "other"
  }

  def supportedTypes(): String = {
    "'carbon', 'silicon', 'other'"
  }
}
