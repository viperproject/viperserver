package viper.server.vsi

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import org.reactivestreams.Publisher
import viper.server.ViperConfig
import viper.server.protocol.ReporterProtocol
import viper.silver.logger._
import viper.silver.reporter.Message

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}




trait VerificationServerInterface {

  case class VerificationJobHandler(id: Int)
  case class JobHandle(controller_actor: ActorRef,
                     queue: SourceQueueWithComplete[Message],
                     publisher: Publisher[Message])

  def getServerConfig(): ViperConfig
  def getServerLogger(): ViperLogger

  def initializeVerificationProcess(taskCreator: ActorRef => Thread) = {
    if (newJobsAllowed) {
      def createJob(new_jid: Int): Future[JobHandle] = {
        implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
        val job_actor = system.actorOf(JobActor.props(new_jid, getServerLogger()), s"job_actor_$new_jid")
        val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
                                       .toMat(Sink.asPublisher(false))(Keep.both)
                                       .run()
        val reportingActor = system.actorOf(QueueActor.props(new_jid, queue), s"queue_actor_$new_jid")
        val backendTask = taskCreator(reportingActor)
        val answer = job_actor ? VerificationProtocol.Verify(backendTask, queue, publisher)
        val new_job_handle: Future[JobHandle] = answer.mapTo[JobHandle]
        new_job_handle
      }
      val (id, jobHandle) = bookNewJob(createJob)
      VerificationJobHandler(id)
    } else {
      println(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS).")
      VerificationJobHandler(-1) // Not able to create a new JobHandle
    }
  }

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val executionContext = ExecutionContext.global
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  var _jobHandles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _nextJobId: Int = 0
  val MAX_ACTIVE_JOBS: Int = 3

  protected def newJobsAllowed = _jobHandles.size < MAX_ACTIVE_JOBS

  /** Creates a Future of a JobHandle, representing the new job
    *
    * For the next available job ID the function job_executor will set up a JobActor that will start
    * a verification process and thereby eventually produce the JobHandle that's returned as a Future.
    * */
  protected def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _nextJobId
    _jobHandles(new_jid) = job_executor(new_jid)
    _nextJobId = _nextJobId + 1
    (new_jid, _jobHandles(new_jid))
  }

  protected def discardJob(jid: Int): mutable.Map[Int, Future[JobHandle]] = {
    _jobHandles -= jid
  }

  protected def lookupJob(jid: Int): Option[ Future[JobHandle] ] = {
    _jobHandles.get(jid)
  }

  protected var _termActor: ActorRef = _

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
              .onComplete(_ => {
                system.terminate() // and shutdown when done
              })
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

  object JobActor {
    def props(id: Int, logger: ViperLogger): Props = Props(new JobActor(id, logger))
  }

  class JobActor(private val id: Int, private val logger: ViperLogger) extends Actor {

    //Agnostic
    private var _verificationTask: Thread = _

    // blocking
    //Agnostic
    private def interrupt: Boolean = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
        println(s"Job #$id has been successfully interrupted.")
        return true
      }
      false
    }
    //Agnostic
    private def resetVerificationTask(): Unit = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
      }
      _verificationTask = null
    }

    override def receive: PartialFunction[Any, Unit] = {
      //Agnostic
      case VerificationProtocol.Stop(call_back_needed) =>
        val did_I_interrupt = interrupt
        if (call_back_needed) {
          // If a callback is expected, then the caller must decide when to kill the actor.
          if (did_I_interrupt) {
            sender ! s"Job #$id has been successfully interrupted."
          } else {
            sender ! s"Job #$id has already been finalized."
          }
        }
      case VerificationProtocol.Verify(task, queue, publisher) =>
        resetVerificationTask()
        sender ! startJob(task, queue, publisher)
      case msg =>
        throw new Exception("Main Actor: unexpected message received: " + msg)
    }

    private def startJob(task: Thread, queue: SourceQueueWithComplete[Message], publisher: Publisher[Message]): JobHandle = {
      _verificationTask = task
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
        val offer_status = queue.offer(msg)
        sender() ! offer_status
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

  protected def getInterruptFutureList(): Future[List[String]] = {
    val interrupt_future_list: List[Future[String]] = _jobHandles map { case (jid, handle_future) =>
      handle_future.flatMap {
        case JobHandle(actor, _, _) =>
          implicit val askTimeout: Timeout = Timeout(1000 milliseconds)
          (actor ? VerificationProtocol.Stop(true)).mapTo[String]
      }
    } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }
}

object VerificationProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(task: Thread, queue: SourceQueueWithComplete[Message], publisher: Publisher[Message])

  // Main Actor requests Verification with AST Program
  // case class VerifyAst(config: List[String], reporter: viper.silver.reporter.Reporter, program: viper.silver.ast.Program)

  // VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  // Verification interrupt request to Main Actor
  case class Stop(call_me_back: Boolean)
}