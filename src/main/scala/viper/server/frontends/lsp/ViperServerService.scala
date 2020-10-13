/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2020 ETH Zurich.
  */

package viper.server.frontends.lsp

import java.util.concurrent.{CompletableFuture => CFuture}

import akka.actor.{PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.core.{ViperCache, ViperCoreServer}
import viper.server.frontends.lsp.VerificationState.Stopped
import viper.server.utility.AstGenerator
import viper.server.vsi.VerificationProtocol.Stop
import viper.server.vsi.{JobID, VerificationProtocol, VerificationServer}
import viper.silver.ast.Program
import viper.silver.reporter.PongMessage

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

  def swapBackend(newBackend: BackendProperties): Unit = {
    is_ready = true
    Coordinator.backend = newBackend
    val param = BackendReadyParams("Silicon", false, true)
    Coordinator.client.notifyBackendReady(param)
    Log.info("The backend has been swapped and is now ready for verification")
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

  private def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

  def verify(command: String): JobID = {
    Log.debug("Requesting ViperServer to start new job...")

    val arg_list = getArgListFromArgString(command)
    val file: String = arg_list.last
    val arg_list_partial = arg_list.dropRight(1)

    // Parse file
    val astGen = new AstGenerator(logger)
    var ast_option: Option[Program] = None
    try {
      ast_option = astGen.generateViperAst(file)
    } catch {
      case _: java.nio.file.NoSuchFileException =>
        Log.debug("The file for which verification has been requested was not found.")
        return JobID(-1)
    }
    val ast = ast_option.getOrElse({
      Log.debug("The file for which verification has been requested contained syntax errors.")
      return JobID(-1)
    })

    // prepare backend config
    val backend = arg_list_partial match {
      case "silicon" :: args => SiliconConfig(args)
      case "carbon" :: args => CarbonConfig(args)
      case "custom" :: args => CustomConfig(args)
    }

    val jid: JobID = verify(file, backend, ast)
    if (jid.id >= 0) {
      Log.info(s"Verification process #${jid.id} has successfully started.")
    } else {
      Log.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS}).")
    }
    jid
  }

  def startStreaming(jid: JobID, relayActor_props: Props): Unit = {
    Log.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor)
  }

  def stopVerification(jid: JobID): CFuture[Boolean] = {
    jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.job_actor ? Stop()).mapTo[String]
          handle.job_actor ! PoisonPill // the actor played its part.
          interrupt
        }).toJava.toCompletableFuture.thenApply(msg => {
          Log.info(msg)
          true
        })
      case _ =>
        // Did not find a job with this jid.
        CFuture.failedFuture(new Throwable(s"The verification job #$jid does not exist."))
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