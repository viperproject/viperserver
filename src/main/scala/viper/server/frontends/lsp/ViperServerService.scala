// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import scala.language.postfixOps
import java.util.concurrent.{CompletableFuture => CFuture}
import akka.actor.{PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import ch.qos.logback.classic.Logger
import viper.server.ViperConfig
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.server.utility.Helpers.{getArgListFromArgString, validateViperFile}
import viper.server.vsi.VerificationProtocol.StopVerification
import viper.server.vsi.{VerJobId, VerificationServer}

import scala.compat.java8.FutureConverters._
import scala.concurrent.Future
import scala.concurrent.duration._

class ViperServerService(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with VerificationServer {

  def verifyWithCommand(command: String, localLogger: Option[Logger] = None): VerJobId = {
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

    val ver_id = verifyWithAstJob(file, ast_id, backend, localLogger)
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

  def stopVerification(jid: VerJobId, localLogger: Option[Logger] = None): CFuture[Boolean] = {
    val logger = combineLoggers(localLogger)
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.job_actor ? StopVerification).mapTo[String]
          handle.job_actor ! PoisonPill // the actor played its part.
          interrupt
        }).toJava.toCompletableFuture.thenApply(msg => {
          logger.info(msg)
          true
        })
      case _ =>
        // Did not find a job with this jid.
        CFuture.completedFuture({throw new Throwable(s"The verification job #$jid does not exist.")})
    }
  }

  /** if a file path is provided, only the cache for the particular file is flushed */
  def flushCachePartially(backendAndFilePath: Option[(String, String)], localLogger: Option[Logger] = None): Boolean = {
    val logger = combineLoggers(localLogger)
    backendAndFilePath match {
      case Some((backend, file)) =>
        logger.info(s"Requesting ViperServer to flush the cache for backend $backend and file $file...")
        val flushed_file_opt = ViperCache.forgetFile(backend, file)

        if (flushed_file_opt.isDefined) {
          logger.debug(s"ViperServer has confirmed that the cache for backend $backend and file $file has been flushed.")
        } else {
          logger.debug(s"Error while requesting ViperServer to flush the cache for backend $backend and $file: not found.")
        }
        flushed_file_opt.isDefined
      case None =>
        logger.info("Flush entire cache...")
        super.flushCache(localLogger)
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
