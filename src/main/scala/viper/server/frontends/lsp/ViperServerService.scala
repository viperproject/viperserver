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
import viper.server.utility.Helpers.{getArgListFromArgString, validateViperFile}
import viper.server.vsi.VerificationProtocol.{StopAstConstruction, StopVerification}
import viper.server.vsi.{AstJobId, DefaultVerificationServerStart, VerHandle, VerJobId}

import scala.concurrent.Future
import scala.concurrent.duration._

class ViperServerService(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with DefaultVerificationServerStart {

  def verifyWithCommand(command: String, localLogger: Option[Logger] = None, verifyTarget: Option[String]): VerJobId = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to start new job...")

    val arg_list = getArgListFromArgString(command)
    val file: String = arg_list.last
    val arg_list_partial = arg_list.dropRight(1)

    if (!validateViperFile(file)) {
      logger.debug(s"file not found: $file")
      return VerJobId(-1)
    }

    val ast_id = requestAst(arg_list, localLogger)

    val backend = try {
      ViperBackendConfig(arg_list_partial)
    } catch {
      case _: IllegalArgumentException =>
        logger.info(s"Invalid arguments: $command " +
          s"You need to specify the verification backend, e.g., `silicon [args]`")
        return VerJobId(-1)
    }

    val ver_id = verifyWithAstJob(file, ast_id, backend, localLogger, verifyTarget)
    if (ver_id.id >= 0) {
      logger.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  def startStreaming(jid: VerJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Unit = {
    val logger = combineLoggers(localLogger)
    logger.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor)
  }

  def stopVerification(jid: VerJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val logger = combineLoggers(localLogger)
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          // first stop ast construction:
          val astFuture = handle.prev_job_id.map(astJobId => stopAstConstruction(astJobId, logger)).getOrElse(Future.successful(true))
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
    implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
    val interrupt: Future[String] = (handle.job_actor ? StopVerification).mapTo[String]
    handle.job_actor ! PoisonPill // the actor played its part.
    interrupt.map(msg => {
      combinedLogger.info(msg)
      true
    })
  }

  private def stopAstConstruction(jid: AstJobId, combinedLogger: Logger): Future[Boolean] = {
    ast_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.job_actor ? StopAstConstruction).mapTo[String]
          handle.job_actor ! PoisonPill // the actor played its part.
          interrupt
        }).map(msg => {
          combinedLogger.info(msg)
          combinedLogger.info(s"ast construction stopped for job #$jid")
          true
        })
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
