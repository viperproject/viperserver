package viper.server

import org.reactivestreams.{Publisher, Subscriber}

import scala.language.postfixOps
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import java.util.NoSuchElementException

import akka.{Done, NotUsed}
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.CacheDirectives.public
import akka.http.scaladsl.server.Directives._
import viper.silver.reporter.{EntityFailureMessage, ExceptionReport, Message, PongMessage, Reporter, SimpleMessage}
import viper.silver.ast
import viper.silver.logger.ViperLogger
import viper.silver.verifier.{AbstractError, VerificationError, VerificationResult, Failure => VerificationFailure, Success => VerificationSuccess}
import viper.server.ViperServerProtocol._
import viper.server.ViperBackendConfigs._
import viper.server.ViperIDEProtocol.VerificationRequestReject
import viper.silver.logger.ViperStdOutLogger

// We can potentially have more than one verification task at the same time.
// A verification task is distinguished via the corresponding ActorRef,
//  as well as its unique job_id.
case class JobHandle(controller_actor: ActorRef,
                     queue: SourceQueueWithComplete[Message],
                     publisher: Publisher[Message])


case class VerificationJobHandler(id: Int)

class ViperCoreServer(private var _config: ViperConfig) {
  // --- VCS : Configuration ---
  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val executionContext = ExecutionContext.global

  final def config: ViperConfig = _config

  private var _logger: ViperLogger = _
  final def logger: ViperLogger = _logger

  implicit val materializer: ActorMaterializer = ActorMaterializer()


  // --- VCS : Jobs ---
  var _job_handles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _next_job_id: Int = 0
  val MAX_ACTIVE_JOBS: Int = 3

  private def newJobsAllowed = _job_handles.size < MAX_ACTIVE_JOBS

  private def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _next_job_id
    _job_handles(new_jid) = job_executor(new_jid)
    _next_job_id = _next_job_id + 1
    (new_jid, _job_handles(new_jid))
  }

  private def discardJob(jid: Int): mutable.Map[Int, Future[JobHandle]] = {
    _job_handles -= jid
  }

  /** If the Option is resolved to None, the job does not exist.
    * If the Option is resolved to Some(_),
    *   a) The Future is not yet completed ==> verification in progress.
    *   b) The Future is already completed ==> job done.
    */
  protected def lookupJob(jid: Int): Option[ Future[JobHandle] ] = {
    _job_handles.get(jid)
  }


  // --- Actor: Terminator ---

  protected var _term_actor: ActorRef = _

  object Terminator {
    case object Exit
    case class WatchJobQueue(jid: Int, handle: JobHandle)

    def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(Some(bindingFuture)))
    def props(): Props = Props(new Terminator(None))
  }

  class Terminator(bindingFuture: Option[Future[Http.ServerBinding]]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case Terminator.Exit =>
        bindingFuture match {
          case Some(future) =>
            future
            .flatMap(_.unbind()) // trigger unbinidng from the port
            .onComplete(_ => system.terminate()) // and shutdown when done

          case None =>
            system.terminate() // shutdown
        }
      case Terminator.WatchJobQueue(jid, handle) =>
        val queue_completion_future: Future[Done] = handle.queue.watchCompletion()
        queue_completion_future.onComplete( {
          case Failure(e) =>
            println(s"Terminator detected failure in job #$jid: $e")
            throw e
          case Success(_) =>
            discardJob(jid)
            println(s"Terminator deleted job #$jid")
        })
    }
  }


  // --- Actor: MainActor ---

  object MainActor {
    def props(id: Int, logger: ViperLogger): Props = Props(new MainActor(id, logger))
  }

  // (See model description in ViperServerProtocol.scala)
  class MainActor(private val id: Int, private val logger: ViperLogger) extends Actor {

    private var _verificationTask: Thread = _

    // blocking
    private def interrupt: Boolean = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
        println(s"Job #$id has been successfully interrupted.")
        return true
      }
      false
    }

    private def resetVerificationTask(): Unit = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
      }
      _verificationTask = null
    }

    override def receive: PartialFunction[Any, Unit] = {

      case Stop(call_back_needed) =>
        val did_I_interrupt = interrupt
        if (call_back_needed) {
          // If a callback is expected, then the caller must decide when to kill the actor.
          if (did_I_interrupt) {
            sender ! s"Job #$id has been successfully interrupted."
          } else {
            sender ! s"Job #$id has already been finalized."
          }
        }

      case Verify(args, program) =>
        resetVerificationTask()
        sender ! verify(args, program)

      case msg =>
        throw new Exception("Main Actor: unexpected message received: " + msg)
    }

    private def verify(args: List[String], program: ast.Program): JobHandle = {
      val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
                                     .toMat(Sink.asPublisher(false))(Keep.both)
                                     .run()
      val reportingActor = system.actorOf(QueueActor.props(id, queue), s"reporter_$id")

      _verificationTask = new Thread(new VerificationWorker(reportingActor, logger.get, args, program))
      _verificationTask.start()

      println(s"Starting job #$id...")

      JobHandle(self, queue, publisher)
    }
  }


  // --- Actor: QueueActor ---

  object QueueActor {
    def props(jid: Int, queue: SourceQueueWithComplete[Message]): Props = Props(new QueueActor(jid, queue))
  }

  class QueueActor(jid: Int, queue: SourceQueueWithComplete[Message]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case ReporterProtocol.ClientRequest =>
      case ReporterProtocol.ServerReport(msg) =>
        val offer_future = queue.offer(msg)
      case ReporterProtocol.FinalServerReport(success) =>
        queue.complete()
        if ( success )
          println(s"Job #$jid has been completed successfully.")
        else
          println(s"Job #$jid has been completed ERRONEOUSLY.")
        self ! PoisonPill
      case _ =>
    }
  }


  // --- VCS : Core Functions ---

  def start(): Unit = {
    init(None)
  }

  protected def init(routes: Option[ViperLogger => Route]): Unit = {
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, "ALL")
    println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    ViperCache.initialize(logger.get, config.backendSpecificCache())

    routes match {
      case Some(routes) => {
        val port = config.port()
        val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes(logger), "localhost", port)

        _term_actor = system.actorOf(Terminator.props(bindingFuture), "terminator")

        println(s"ViperServer online at http://localhost:$port")
      }
      case None => {
        _term_actor = system.actorOf(Terminator.props(), "terminator")

        println(s"ViperServer online in CoreServer mode")
      }
    }
  }

  def verify(programID: String, config: ViperBackendConfig, program: ast.Program): VerificationJobHandler = {
    val args: List[String] = config match {
      case _ : SiliconConfig => "silicon" :: config.partialCommandLine
      case _ : CarbonConfig => "carbon" :: config.partialCommandLine
      case _ : CustomConfig => "DummyFrontend" :: config.partialCommandLine
    }
    createJobHandle(args :+ programID, program)
  }

  protected def createJobHandle(args: List[String], program: ast.Program): VerificationJobHandler = {
    if (newJobsAllowed) {
      val (id, jobHandle) = bookNewJob((new_jid: Int) => {
        implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
        val main_actor = system.actorOf(MainActor.props(new_jid, logger), s"main_actor_$new_jid")
        val new_job_handle: Future[JobHandle] = (main_actor ? ViperServerProtocol.Verify(args, program)).mapTo[JobHandle]
        new_job_handle
      })
      VerificationJobHandler(id)
    } else {
      println(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS).")
      VerificationJobHandler(-1) // Not able to create a new JobHandle
    }
  }

  def stop(): Unit = {
    println(s"Stopping ViperCoreServer")

    getInterruptFutureList() onComplete {
      case Success(_) =>
        _term_actor ! Terminator.Exit
        println(s"shutting down...")
      case Failure(err_msg) =>
        println(s"Interrupting one of the verification threads timed out: $err_msg")
        _term_actor ! Terminator.Exit
        println(s"forcibly shutting down...")
    }
  }


  // --- VCS : Auxiliary Functions ---

  protected def getInterruptFutureList(): Future[List[String]] = {
    val interrupt_future_list: List[Future[String]] = _job_handles map { case (jid, handle_future) =>
      handle_future.flatMap {
        case JobHandle(actor, _, _) =>
          implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
          (actor ? Stop(true)).mapTo[String]
      }
    } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }

  def flushCache(): Unit = {
    ViperCache.resetCache()
    println(s"The cache has been flushed successfully.")
  }

  def streamMessages(jid: Int, clientActor: ActorRef): Unit = {
    lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.onComplete({
          case Success(handle) =>
            Source.fromPublisher(handle.publisher).runWith(Sink.actorRef(clientActor, Success))
            _term_actor ! Terminator.WatchJobQueue(jid, handle)
          case Failure(e) => clientActor ! e
        })
      case None =>
        clientActor ! new NoSuchElementException(s"The verification job #$jid does not exist.")
    }
  }

  /**
    * Returns a future containing a sequence of all the messages generated by the verification backend.
    * Deletes the jobhandle on completion.
    */
  def getMessagesFuture(jid: Int): Future[Seq[Message]] = {
    lookupJob(jid) match {
      case Some(handle_future) =>
        val result_future = handle_future.flatMap(handle => {
          val messages_future: Future[Seq[Message]] = Source.fromPublisher(handle.publisher).runWith(Sink.seq)
          _term_actor ! Terminator.WatchJobQueue(jid, handle)
          messages_future
        })
        result_future
      case None =>
        val promise = Promise[Seq[Message]]()
        promise failure (new NoSuchElementException(s"The verification job #$jid does not exist."))
        promise.future
    }
  }

}