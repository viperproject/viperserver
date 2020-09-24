// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.{Done, NotUsed}
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult}
import akka.util.Timeout
import org.reactivestreams.Publisher

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


class VerificationServerException extends Exception
case class JobNotFoundException() extends VerificationServerException

case class JobID(id: Int)
case class JobHandle(job_actor: ActorRef,
                     queue: SourceQueueWithComplete[Envelope],
                     publisher: Publisher[Envelope])

/** This class manages the verification jobs the server receives.
  */
class JobPool(val MAX_ACTIVE_JOBS: Int = 3) {
  var jobHandles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _nextJobId: Int = 0

  def newJobsAllowed = jobHandles.size < MAX_ACTIVE_JOBS

  /** Creates a Future of a JobHandle
    *
    * For the next available job ID the function job_executor will set up a JobActor. That actor will
    * start a verification process and produce a Future JobHandle. The Future will successfully complete
    * as soon as the verification process was started successfully.
    * */
  def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _nextJobId
    jobHandles(new_jid) = job_executor(new_jid)
    _nextJobId = _nextJobId + 1
    (new_jid, jobHandles(new_jid))
  }

  /** Discards the JobHandle for the given JobID
    * */
  def discardJob(jid: JobID): mutable.Map[Int, Future[JobHandle]] = {
    jobHandles -= jid.id
  }

  def lookupJob(jid: JobID): Option[ Future[JobHandle] ] = {
    jobHandles.get(jid.id)
  }
}

/** This trait provides state and process management functionality for verification servers.
  *
  * The server runs on Akka's actor system. This means that the entire server's state
  * and process management are run by actors. The 3 actors in charge are:
  *
  *   1) Job Actor
  *   2) Queue Actor
  *   3) Terminator Actor
  *
  *  The first two manage individual verification processes. I.e., on initializeVerificationProcess()
  *  and instance of each actor is created. The JobActor launches the actual VerificationTask, while
  *  the QueueActor acts as a middleman for communication between a VerificationTask's backend and the
  *  server. The Terminator Actor is in charge of terminating both individual processes and the server.
  */
trait VerificationServer extends Unpacker {

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val executionContext = ExecutionContext.global
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  protected var jobs: JobPool = _
  var isRunning: Boolean = false

  /** Sets up the server.
    *
    * This method must be called by before any other method in this interface.
    * */
  def start(active_jobs: Int): Unit = {
    jobs = new JobPool(active_jobs)
    _termActor = system.actorOf(Terminator.props(), "terminator")
    isRunning = true
  }

  // --- Actor: Terminator ---

  protected var _termActor: ActorRef = _

  object Terminator {
    case object Exit
    case class WatchJobQueue(jid: JobID, handle: JobHandle)

    def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(Some(bindingFuture)))
    def props(): Props = Props(new Terminator(None))
  }

  class Terminator(bindingFuture: Option[Future[Http.ServerBinding]]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case Terminator.Exit =>
        bindingFuture match {
          case Some(future) =>
            future
              .flatMap(_.unbind()) // trigger unbinding from the port
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
            throw e
          case Success(_) =>
            jobs.discardJob(jid)
        })
    }
  }


  // --- Actor: JobActor ---

  object JobActor {
    def props(id: Int): Props = Props(new JobActor(id))
  }

  class JobActor(private val id: Int) extends Actor {

    private var _verificationTask: Thread = _

    private def interrupt: Boolean = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
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
      case VerificationProtocol.Stop =>
        val did_I_interrupt = interrupt
        if (did_I_interrupt) {
          sender ! s"Job #$id has been successfully interrupted."
        } else {
          sender ! s"Job #$id has already been finalized."
        }
      case VerificationProtocol.Verify(task, queue, publisher) =>
        resetVerificationTask()
        sender ! startJob(task, queue, publisher)
      case msg =>
        throw new Exception("Main Actor: unexpected message received: " + msg)
    }

    private def startJob(task: Thread, queue: SourceQueueWithComplete[Envelope], publisher: Publisher[Envelope]): JobHandle = {
      _verificationTask = task
      _verificationTask.start()
      JobHandle(self, queue, publisher)
    }
  }


  // --- Actor: MessageActor ---

  object QueueActor {
    def props(jid: Int, queue: SourceQueueWithComplete[Envelope]): Props = Props(new QueueActor(jid, queue))
  }

  class QueueActor(jid: Int, queue: SourceQueueWithComplete[Envelope]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case TaskProtocol.BackendReport(msg) =>
        val offer_status = queue.offer(msg)
        sender() ! offer_status
      case TaskProtocol.FinalBackendReport(_) =>
        queue.complete()
        self ! PoisonPill
      case _ =>
    }
  }

  /** This method starts an individual verification process.
    *
    * As such, it accepts an instance of a VerificationTask.
    */
  protected def initializeVerificationProcess(task:VerificationTask): JobID = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }

    if (jobs.newJobsAllowed) {
      def createJob(new_jid: Int): Future[JobHandle] = {

        implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
        val job_actor = system.actorOf(JobActor.props(new_jid), s"job_actor_$new_jid")
        val (queue, publisher) = Source.queue[Envelope](10000, OverflowStrategy.backpressure)
                                       .toMat(Sink.asPublisher(false))(Keep.both)
                                       .run()
        val message_actor = system.actorOf(QueueActor.props(new_jid, queue), s"queue_actor_$new_jid")
        task.setQueueActor(message_actor)
        val task_with_actor = new Thread(task)
        val answer = job_actor ? VerificationProtocol.Verify(task_with_actor, queue, publisher)
        val new_job_handle: Future[JobHandle] = answer.mapTo[JobHandle]
        new_job_handle
      }
      val (id, _) = jobs.bookNewJob(createJob)
      JobID(id)
    } else {
      JobID(-1) // Process Management running  at max capacity.
    }
  }

  /** A verification process ends after the results are retrieved.
    *
    * This should be done providing an actor that can receive the envelopes stored in the Queue actor's source queue
    */
  protected def streamMessages(jid: JobID, clientActor: ActorRef): Option[Future[Unit]] = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }

    jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        def mapHandle(handle: JobHandle): Future[Unit] = {
          val src_envelope: Source[Envelope, NotUsed] = Source.fromPublisher((handle.publisher))
          val src_msg: Source[A , NotUsed] = src_envelope.map(e => unpack(e))
          src_msg.runWith(Sink.actorRef(clientActor, Success))
          _termActor ! Terminator.WatchJobQueue(jid, handle)
          handle.queue.watchCompletion().map(_ => ())
        }
        Some(handle_future.flatMap(mapHandle))
      case None =>
        clientActor ! JobNotFoundException()
        None
    }
  }

  /** This method shuts the server down.
    *
    * After calling stop, no other method may be called.
    * */
  def stop(): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    isRunning = false

    getInterruptFutureList() onComplete {
      case Success(_) =>
        _termActor ! Terminator.Exit
        println(s"shutting down...")
      case Failure(err_msg) =>
        _termActor ! Terminator.Exit
        println(s"forcibly shutting down...")
    }
  }

  /** This method interrupts active jobs upon termination of the server.
    */
  protected def getInterruptFutureList(): Future[List[String]] = {
    val interrupt_future_list: List[Future[String]] = jobs.jobHandles map { case (jid, handle_future) =>
      handle_future.flatMap {
        case JobHandle(actor, _, _) =>
          implicit val askTimeout: Timeout = Timeout(1000 milliseconds)
          (actor ? VerificationProtocol.Stop).mapTo[String]
      }
    } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }
}


/** This class is a generic wrapper for a any sort of verification a VerificationServer might
  * work on.
  *
  * It has the following properties:
  *  - implements runnable
  *  - provides a reference to a queue actor.
  *
  *  The first serves the purpose of running the process concurrently. The second allows to
  *  communicate from the verification process to the server.
  * */
abstract class VerificationTask()(implicit val executionContext: ExecutionContext) extends Runnable with Packer {

  private var q_actor: ActorRef = _

  final def setQueueActor(actor: ActorRef): Unit = {
    q_actor = actor
  }

  /** Sends massage to the attached actor.
    *
    * The actor receiving this message offers it to a queue. This offering returns a Future, which will eventually
    * indicate whether or not the offer was successful. This method is blocking, as it waits for the successful
    * completion of such an offer.
    * */
  protected def enqueueMessages(msg: A): Unit = {
    implicit val askTimeout: Timeout = Timeout(5000 milliseconds)

    var current_offer: Future[QueueOfferResult] = null
    val answer = q_actor ? TaskProtocol.BackendReport(pack(msg))
    current_offer = answer.flatMap({
      case res: Future[QueueOfferResult] => res
    })
    while(current_offer == null || !current_offer.isCompleted){
      Thread.sleep(10)
    }
  }

  /** Notify the queue actor that the task has come to an end
    *
    * The actor receiving this message will close the queue.
    *
    * @param success indicates whether or not the task has ended as successfully.
    * */
  protected def registerTaskEnd(success: Boolean): Unit = {
    q_actor ! TaskProtocol.FinalBackendReport(success)
  }
}
