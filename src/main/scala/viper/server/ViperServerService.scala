package viper.server

import java.util.concurrent.{CompletableFuture => CFuture}

import akka.actor.{Actor, PoisonPill, Props}

import scala.compat.java8.FutureConverters._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
import viper.server.core.{VerificationJobHandler, ViperCoreServer}
import viper.server.protocol.ViperServerProtocol.Stop
import viper.server.VerificationState._
import viper.server.VerificationSuccess._
import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.utility.AstGenerator
import viper.silver.ast.{Method, Program}
import viper.silver.reporter.{Message, ProgramOutlineReport}

class ViperServerService(args: Array[String]) extends ViperCoreServer(args) {
  var instanceCount: Int = 0
  var isSessionRunning: Boolean = false

  private var _ready: Boolean = false

  protected var timeout: Int = _

  private var _server_logfile: String = _

  // the JID that ViperServer assigned to the current verification job.
  private var _job_id: Int = _

  def isReady: Boolean = _ready;

  def setReady(backend: Backend): Unit = {
    _ready = true
    Coordinator.backend = backend
    Coordinator.startingOrRestarting = false
    Log.info("The backend is ready for verification")
    Coordinator.client.notifyBackendReady(S2C_Commands.BackendReady)
  }

  def startStageProcess(
        stage: Stage, fileToVerify: String,
        onData: String => Unit,
        onError: String => Unit,
        onClose: Int => Unit): Unit = {

    try {
      Log.lowLevel("Start Stage Process")

      isSessionRunning = true
      val command = getStageCommand(fileToVerify, stage);
      startVerifyStream(command)
    } catch {
      case e: Throwable => Log.debug("Error starting stage process: " + e);
    }
  }

   def getStageCommand(fileToVerify: String, stage: Stage): String = {
       val args: String = getViperBackendClassName(stage) + " " + stage.customArguments
       val command = Settings.expandCustomArguments(args, stage, fileToVerify, Coordinator.backend)
       Log.debug(command)
       command
   }

   def getViperBackendClassName(stage: Stage): String = {
     Coordinator.backend.backend_type match {
       case "silicon" => "silicon"
       case "carbon" => "carbon"
       case "other" => stage.mainMethod
       case _ => throw new Error(s"Invalid verification backend value. " +
                                    s"Possible values are [silicon | carbon | other] " +
                                    s"but found ${Coordinator.backend}")
     }
   }

  def swapBackend(newBackend: Backend): Unit = {
    setReady(newBackend)
  }

  def stopVerification(): CFuture[Boolean] = {
    lookupJob(_job_id) match {
      case Some(handle_future) =>
        handle_future.flatMap(handle => {
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          val interrupt: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
          handle.controller_actor ! PoisonPill // the actor played its part.
          interrupt
        }).toJava.toCompletableFuture.thenApply(msg => {
          Log.info(msg);
          true
        })
      case _ =>
        // Did not find a job with this jid.
        CFuture.failedFuture(new Throwable(s"The verification job #$_job_id does not exist."))
    }
  }

  def setStopping(): Unit = {
    Log.debug("Set Stopping... ")
    _ready = false
    Coordinator.startingOrRestarting = false
    Coordinator.sendStateChangeNotification(StateChangeParams(Stopping), None)
  }

  def setStopped(): Unit = {
    Log.debug("Set Stopped. ")
    _ready = false
    Coordinator.startingOrRestarting = false
    Coordinator.sendStateChangeNotification(StateChangeParams(Stopped), None)
  }

  private def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

  def startVerify(command: String): Int = {
    Log.debug("Requesting ViperServer to start new job...")
    // Todo start verification in VCS

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
        return
    }
    val ast = ast_option.getOrElse(return Log.debug("The file for which verification has been requested contained syntax errors."))

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

  // This method should start a verification and, after getting back the corresponding jobId, should
  // immmediately ask for messages.
  private def startVerifyStream(command: String): Unit = {
    Log.debug("Sending verification request to ViperServer...")
    _job_id = startVerify(command)
    val relay_actor = system.actorOf(RelayActor.props())
    val termination_status = streamMessages(_job_id, relay_actor)
  }

  private object RelayActor {
    case object Result
    def props(): Props = Props(new RelayActor())
  }

  def flushCache(filePath?: String): CFuture[String] = {
    return new Promise((resolve, reject) => {
    val url = this._url + ':' + this._port + '/cache/flush'
    if (filePath) {
    Log.log(`Requesting ViperServer to flush the cache for (` + filePath + `)...`, LogLevel.Info)

    val options = {
    url: url,
    headers: {'content-type': 'application/json'},
    body: JSON.stringify({ backend: Coordinator.backend.name, file: filePath })
  }

    request.post(options).on('error', (error) => {
    Log.log(`error while requesting ViperServer to flush the cache for (` + filePath + `).` +
    ` Request URL: ${url}\n` +
    ` Error message: ${error}`, LogLevel.Default)
    reject(error)

  }).on('data', (data) => {
    val response = JSON.parse(data.toString())
    if ( !response.msg ) {
    Log.log(`ViperServer did not complain about the way we requested it to flush the cache for (` + filePath + `).` +
    ` However, it also did not provide the expected bye-bye message.` +
    ` It said: ${data.toString}`, LogLevel.Debug)
    resolve(response)
  } else {
    Log.log(`ViperServer has confirmed that the cache for (` + filePath + `) has been flushed.`, LogLevel.Debug)
    resolve(response.msg)
  }
  })

  } else {
    Log.log(`Requesting ViperServer to flush the entire cache...`, LogLevel.Info)

    request.get(url).on('error', (error) => {
    Log.log(`error while requesting ViperServer to flush the entire cache.` +
    ` Request URL: ${url}\n` +
    ` Error message: ${error}`, LogLevel.Default)
    reject(error)

  }).on('data', (data) => {
    val response = JSON.parse(data.toString())
    if ( !response.msg ) {
    Log.log(`ViperServer did not complain about the way we requested it to flush the entire cache.` +
    ` However, it also did not provide the expected bye-bye message.` +
    ` It said: ${data.toString}`, LogLevel.Debug)
    resolve(response)
  } else {
    Log.log(`ViperServer has confirmed that the entire cache has been flushed.`, LogLevel.Debug)
    resolve(response.msg)
  }
  })
  }
  })
  }

  private def sendStopRequest(): CFuture[Boolean] = {
    return new Promise((resolve, reject) => {
    Log.log(`Requesting ViperServer to exit...`, LogLevel.Debug)
    val url = this._url + ':' + this._port + '/exit'
    request.get(url).on('error', (err) => {
    Log.log(`error while requesting ViperServer to stop.` +
    ` Request URL: ${url}\n` +
    ` Error message: ${err}`, LogLevel.Default)
    reject(err)
  }).on('data', (data) => {
    val response = JSON.parse(data.toString())
    if ( !response.msg ) {
    Log.log(`ViperServer did not complain about the way we requested it to exit.` +
    ` However, it also did not provide the expected bye-bye message.` +
    ` It said: ${data.toString}`, LogLevel.Debug)
    resolve(true)
  } else if ( response.msg !== 'shutting down...' ) {
    Log.log(`ViperServer responded with an unexpected bye-bye message: ${response.msg}`,
    LogLevel.Debug)
    resolve(true)
  } else {
    Log.log(`ViperServer has exited properly.`, LogLevel.Debug)
    resolve(true)
  }
  })
  })
  }

  def isSupportedType(t: String) = {
    if (!t) {
      return false
    }
    return t.toLowerCase() == 'carbon' || t.toLowerCase() == 'silicon' || t.toLowerCase() == 'other'
  }

  def supportedTypes(): String = {
    "'carbon', 'silicon', 'other'"
  }
}