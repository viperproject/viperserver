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
import viper.server.core.{ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.server.frontends.lsp.VerificationState.Stopped
import viper.server.utility.AstGenerator
import viper.server.utility.Helpers.getArgListFromArgString
import viper.server.vsi.VerificationProtocol.StopVerification
import viper.server.vsi.{VerJobId, VerificationServer}
import viper.silver.ast.Program

import scala.compat.java8.FutureConverters._
import scala.concurrent.Future
import scala.concurrent.duration._

class ViperServerService(args: Array[String]) extends ViperCoreServer(args) with VerificationServer {

  protected var timeout: Int = _

  def isRunningServer: Boolean = isRunning

  var is_ready = false

  def setReady(backend: BackendProperties): Unit = {
    Coordinator.backend = backend
    start()
    is_ready = true
    val param = BackendReadyParams("Silicon", false, true)
    Coordinator.client.notifyBackendReady(param)
    Log.info("The backend is ready for verification")
  }

  def setStopping(): Unit = {
    Log.debug("Set Stopping... ")
    if(isRunning){
      isRunning = false
      val params = StateChangeParams(Stopped.id)
      Coordinator.sendStateChangeNotification(params, None)
    } else {
      Log.debug("Server stopped")
    }
  }
  def setStopped(): Unit = {
    Log.debug("Set Stopped. ")
    if(isRunning){
      isRunning = false
      val params = StateChangeParams(Stopped.id)
      Coordinator.sendStateChangeNotification(params, None)
    } else {
      Log.debug("Server stopped")
    }
  }

  def verify(command: String): VerJobId = {
    Log.debug("Requesting ViperServer to start new job...")

    val arg_list = getArgListFromArgString(command)
    val file: String = arg_list.last
    val arg_list_partial = arg_list.dropRight(1)

    // Parse file
    val astGen = new AstGenerator(_logger.get)
    var ast_option: Option[Program] = None
    try {
      // TODO use AstWorker instead
      ast_option = astGen.generateViperAst(file)
    } catch {
      case _: java.nio.file.NoSuchFileException =>
        Log.debug("The file for which verification has been requested was not found.")
        return VerJobId(-1)
    }
    val ast = ast_option.getOrElse({
      Log.debug("The file for which verification has been requested contained syntax errors.")
      return VerJobId(-1)
    })

    // prepare backend config
    val backend = try {
      ViperBackendConfig(arg_list_partial)
    } catch {
      case _: IllegalArgumentException =>
        logger.get.error(s"Invalid arguments: ${command} " +
          s"You need to specify the verification backend, e.g., `silicon [args]`")
        return VerJobId(-1)
    }

    val jid: VerJobId = verify(file, backend, ast)
    if (jid.id >= 0) {
      Log.info(s"Verification process #${jid.id} has successfully started.")
    } else {
      Log.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    jid
  }

  def startStreaming(jid: VerJobId, relayActor_props: Props): Unit = {
    Log.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor)
  }

  def stopVerification(jid: VerJobId): CFuture[Boolean] = {
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.job_actor ? StopVerification).mapTo[String]
          handle.job_actor ! PoisonPill // the actor played its part.
          interrupt
        }).toJava.toCompletableFuture.thenApply(msg => {
          Log.info(msg)
          true
        })
      case _ =>
        // Did not find a job with this jid.
        CFuture.completedFuture({throw new Throwable(s"The verification job #$jid does not exist.")})
    }
  }

  def flushCache(filePath: Option[String]): Unit = {
    filePath match {
      case Some(file) =>
        Log.info(s"Requesting ViperServer to flush the cache for $file...")
        val flushed_file_opt = ViperCache.forgetFile("silicon", file)

        if (flushed_file_opt.isDefined) {
          Log.debug(s"ViperServer has confirmed that the cache for $file has been flushed.")
        } else {
          Log.debug(s"Error while requesting ViperServer to flush the cache for $file: File not found.")
        }
      case None =>
        Log.info("Flush entire cache...")
        super.flushCache()
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