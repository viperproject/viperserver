package viper.server.core

import java.util.NoSuchElementException

import akka.actor.ActorRef
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Sink, Source}
import viper.server.ViperConfig
import viper.server.core.ViperBackendConfigs._
import viper.server.vsi.VerificationServerInterface
import viper.silver.ast
import viper.silver.logger.ViperLogger

import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success}


class ViperCoreServer(private var _config: ViperConfig) extends VerificationServerInterface {

  // --- VCS : Configuration ---
  var isRunning: Boolean = true
  final def config: ViperConfig = _config
  override def getServerConfig(): ViperConfig = config


  private var _logger: ViperLogger = _
  final def logger: ViperLogger = _logger
  override def getServerLogger(): ViperLogger = logger


  /** Configures an instance of ViperCoreServer.
    *
    * This function should be called before any other.
    * */
  def start(): Unit = {
    init(None)
  }

  /** Configures an instance of ViperCoreServer.
    *
    * This method replaces 'start()' when running ViperCoreServer in HTTP mode. It should therefore be called before any other.
    * */
  protected def init(routes: Option[ViperLogger => Route]): Unit = {
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    ViperCache.initialize(logger.get, config.backendSpecificCache())

    routes match {
      case Some(routes) => {
        val port = config.port()
        val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes(logger), "localhost", port)

        _termActor = system.actorOf(Terminator.props(bindingFuture), "terminator")
        println(s"ViperServer online at http://localhost:$port")
      }
      case None => {
        _termActor = system.actorOf(Terminator.props(), "terminator")
        println(s"ViperServer online in CoreServer mode")
      }
    }
  }

  /** Verifies a Viper AST using the specified backend.
    * */
  def verify(programID: String, config: ViperBackendConfig, program: ast.Program): VerificationJobHandler = {
    val args: List[String] = config match {
      case _ : SiliconConfig => "silicon" :: config.partialCommandLine
      case _ : CarbonConfig => "carbon" :: config.partialCommandLine
      case _ : CustomConfig => "DummyFrontend" :: config.partialCommandLine
    }
    createJobHandle(args :+ programID, program)
  }

  /** Verifies a Viper AST using the specified backend.
    *
    * This method replaces 'verify()' when running ViperCoreServer in HTTP mode. As such it provides the possibility
    * to directly pass arguments specified by the client.
    * */
  protected def createJobHandle(args: List[String], program: ast.Program): VerificationJobHandler = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }

    def someFun(qa: ActorRef): Thread = {
      val task_backend = new VerificationWorker(getServerConfig(), logger.get, args, program)
      task_backend.setReporterActor(qa)
      new Thread(task_backend)
    }
    initializeVerificationProcess(someFun)
    //    if (newJobsAllowed) {
    //      val (id, jobHandle) = bookNewJob((new_jid: Int) => {
    //        implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
    //        val main_actor = system.actorOf(JobActor.props(new_jid, logger), s"main_actor_$new_jid")
    //        val answer = main_actor ? ViperServerProtocol.Verify(args, program)
    //        val new_job_handle: Future[JobHandle] = answer.mapTo[JobHandle]
    //        new_job_handle
    //      })
    //      VerificationJobHandler(id)
    //    } else {
    //      println(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS).")
    //      VerificationJobHandler(-1) // Not able to create a new JobHandle
    //    }
  }

  /** Stops an instance of ViperCoreServer from running.
    *
    * As such it should be the ultimate method called. Calling any other function after 'stop()' will result in an
    * IllegalStateException.
    * */
  def stop(): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    isRunning = false

    println(s"Stopping ViperCoreServer")

    getInterruptFutureList() onComplete {
      case Success(_) =>
        _termActor ! Terminator.Exit
        println(s"shutting down...")
      case Failure(err_msg) =>
        println(s"Interrupting one of the verification threads timed out: $err_msg")
        _termActor ! Terminator.Exit
        println(s"forcibly shutting down...")
    }
  }

  def flushCache(): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    ViperCache.resetCache()
    println(s"The cache has been flushed successfully.")
  }
  // NOT Relevant -- Caching is not something we're dealing with, yet.

  /** Stream all messages generated by the backend to some actor.

    * Deletes the jobhandle on completion.
    */
  def streamMessages(jid: Int, clientActor: ActorRef): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.onComplete({
          case Success(handle) =>
            Source.fromPublisher(handle.publisher).runWith(Sink.actorRef(clientActor, Success))
            _termActor ! Terminator.WatchJobQueue(jid, handle)
          case Failure(e) => clientActor ! e
        })
      case None =>
        clientActor ! new NoSuchElementException(s"The verification job #$jid does not exist.")
    }
  }

}

