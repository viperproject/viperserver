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
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig, ViperCoreServer}
import viper.server.utility.Helpers.validateViperFile
import viper.server.vsi.VerificationProtocol.{StopAstConstruction, StopVerification}
import viper.server.vsi.{AstJobId, DefaultVerificationServerStart, VerHandle, VerJobId}
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
        handle_future.flatMap(handle => {
          // first stop ast construction:
          val astFuture = handle.prev_job_id.map(astJobId => stopOnlyAstConstruction(astJobId, localLogger)).getOrElse(Future.successful(true))
          astFuture.flatMap(astResult => {
            stopOnlyVerification(handle, logger)
              .map(verResult => {
                logger.info(s"verification stopped for job #$jid")
                astResult && verResult
              })
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
