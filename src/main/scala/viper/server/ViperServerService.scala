package viper.server

import java.util.concurrent.{CompletableFuture => CFuture}

import akka.actor.{PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import viper.server.VerificationState._
import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.core.{VerificationJobHandler, ViperCache, ViperCoreServer}
import viper.server.protocol.ViperServerProtocol.Stop
import viper.server.utility.AstGenerator
import viper.silver.ast.Program

import scala.compat.java8.FutureConverters._
import scala.concurrent.Future
import scala.concurrent.duration._

class ViperServerService(args: Array[String]) extends ViperCoreServer(args) {

  protected var timeout: Int = _

  def isReady: Boolean = isRunning

  def setReady(backend: BackendProperties): Unit = {
    Coordinator.backend = backend
    start()
    Coordinator.startingOrRestarting = true
    val param = BackendReadyParams("Silicon", false, true)
    Coordinator.client.notifyBackendReady(param)
    Log.info("The backend is ready for verification")
  }

  def swapBackend(newBackend: BackendProperties): Unit = {
    Coordinator.backend = newBackend
    Coordinator.startingOrRestarting = false
    val param = BackendReadyParams("Silicon", false, true)
    Coordinator.client.notifyBackendReady(param)
    Log.info("The backend has been swapped and is now ready for verification")
  }

//  def setStopping(): Unit = {
//    Log.debug("Set Stopping... ")
//    isRunning = false
//    Coordinator.startingOrRestarting = false
//    Coordinator.sendStateChangeNotification(StateChangeParams(Stopping.id), None)
//  }
//
//  def setStopped(): Unit = {
//    Log.debug("Set Stopped. ")
//    isRunning = false
//    Coordinator.startingOrRestarting = false
//    Coordinator.sendStateChangeNotification(StateChangeParams(Stopped.id), None)
//  }

  private def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

  def verify(command: String): Int = {
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
        return -1
    }
    val ast = ast_option.getOrElse({
      Log.debug("The file for which verification has been requested contained syntax errors.")
      return -1
    })

    // prepare backend config
    val backend = arg_list_partial match {
      case "silicon" :: args => SiliconConfig(args)
      case "carbon" :: args => CarbonConfig(args)
      case "custom" :: args => CustomConfig(args)
    }

    val jid: VerificationJobHandler = verify(file, backend, ast)

    if (jid.id >= 0) {
      logger.get.info(s"Verification process #${jid.id} has successfully started.")
    } else {
      logger.get.error(s"Could not start verification process. " +
        s"The maximum number of active verification jobs are currently running (${MAX_ACTIVE_JOBS}).")
      Log.debug(s"the maximum number of active verification jobs are currently running (${MAX_ACTIVE_JOBS}).")
    }
    jid.id
  }

  def startStreaming(jid: Int, relayActor_props: Props): Unit = {
    Log.debug("Sending verification request to ViperServer...")
    val relay_actor = system.actorOf(relayActor_props)
    streamMessages(jid, relay_actor)
  }

  def stopVerification(jid: Int): CFuture[Boolean] = {
    lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
          handle.controller_actor ! PoisonPill // the actor played its part.
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