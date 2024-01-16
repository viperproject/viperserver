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
import viper.silver.ast.utility.FileLoader

import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.ActorRef

class ViperServerService(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with DefaultVerificationServerStart {

  def constructAst(file: String, localLogger: Option[Logger] = None, loader: Option[FileLoader]): AstJobId = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to start new job...")

    val arg_list = getArgListFromArgString(file)
    if (!validateViperFile(file)) {
      logger.debug(s"file not found: $file")
      return AstJobId(-1)
    }

    requestAst(arg_list, localLogger, loader)
  }

  // def discardAstOnCompletion(jid: AstJobId, jobActor: ActorRef): Unit = discardAstJobOnCompletion(jid, jobActor)

  def verifyAst(astJob: AstJobId, command: String, localLogger: Option[Logger] = None): VerJobId = {
    if (astJob.id < 0) {
      return VerJobId(-1)
    }
    val arg_list = getArgListFromArgString(command)
    val file: String = arg_list.last
    val arg_list_partial = arg_list.dropRight(1)
    val logger = combineLoggers(localLogger)
    val backend = try {
      ViperBackendConfig(arg_list_partial)
    } catch {
      case _: IllegalArgumentException =>
        logger.info(s"Invalid arguments: $command " +
          s"You need to specify the verification backend, e.g., `silicon [args]`")
        return VerJobId(-1)
    }

    val ver_id = verifyWithAstJob(file, astJob, backend, localLogger)
    if (ver_id.id >= 0) {
      logger.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  def verifyFull(file: String, command: String, localLogger: Option[Logger] = None): VerJobId = {
    val ast_id = constructAst(file, localLogger, None)
    verifyAst(ast_id, command, localLogger)
  }

  def startStreaming(jid: VerJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    logger.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor, true).map(_.map(_ => ()))
  }
  def startStreamingAst(jid: AstJobId, relayActor_props: Props, localLogger: Option[Logger] = None): (Option[Future[Unit]], ActorRef) = {
    val logger = combineLoggers(localLogger)
    val relay_actor = system.actorOf(relayActor_props)
    logger.debug(s"Sending ast construct request to ViperServer... (${relay_actor.toString()})")
    (streamMessages(jid, relay_actor).map(_.map(_ => ())), relay_actor)
  }
  def startStreamingVer(jid: VerJobId, relayActor_props: Props, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    val relay_actor = system.actorOf(relayActor_props)
    logger.debug(s"Sending verification request to ViperServer... (${relay_actor.toString()})")
    streamMessages(jid, relay_actor, false).map(_.map(_ => {
      logger.debug("Done verification request to ViperServer...")
      ()
    }))
  }
  // def startStreamingVer(jid: VerJobId, relay_actor: ActorRef, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
  //   val logger = combineLoggers(localLogger)
  //   logger.debug("Sending verification only request to ViperServer...")
  //   streamMessages(jid, relay_actor, false).map(_.map(_ => ()))
  // }

  def stopVerification(jid: VerJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val logger = combineLoggers(localLogger)
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          // first stop ast construction:
          val astFuture = handle.prev_job_id.map(astJobId => stopAstConstruction(astJobId, localLogger)).getOrElse(Future.successful(true))
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

  def stopAstConstruction(jid: AstJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val combinedLogger = combineLoggers(localLogger)
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
