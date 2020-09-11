// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import java.util.NoSuchElementException

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import org.reactivestreams.Publisher
import viper.server.protocol.ViperServerProtocol._
import viper.server.core.ViperBackendConfigs._
import viper.server.protocol.{ReporterProtocol, ViperServerProtocol}
import viper.server.ViperConfig
import viper.silver.ast
import viper.silver.logger.ViperLogger
import viper.silver.reporter.Message

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

case class VerificationJobHandler(id: Int)

// We can potentially have more than one verification task at the same time.
// A verification task is distinguished via the corresponding ActorRef,
//  as well as its unique job_id.
case class JobHandle(jobActor: ActorRef,
                     queue: SourceQueueWithComplete[Message],
                     publisher: Publisher[Message])

/** This class manages the verification jobs the server receives.
  */
class JobPool(val MAX_ACTIVE_JOBS: Int = 3){
  var jobHandles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _nextJobId: Int = 0

  def newJobsAllowed = jobHandles.size < MAX_ACTIVE_JOBS

  /** Creates a Future of a JobHandle, representing the new job
    *
    * For the next available job ID the function job_executor will set up a JobActor that will start
    * a verification process and thereby eventually produce the JobHandle that's returned as a Future.
    * */
  def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _nextJobId
    jobHandles(new_jid) = job_executor(new_jid)
    _nextJobId = _nextJobId + 1
    (new_jid, jobHandles(new_jid))
  }

  def discardJob(jid: Int): mutable.Map[Int, Future[JobHandle]] = {
    jobHandles -= jid
  }

  /** Returns the JobHandle corresponding to the given JID
    *
    * If the Option is resolved to None, the job does not (or no longer) exist.
    * If the Option is resolved to Some(_) and the contained Future is
    *   not completed, the Job is in the process of being setup
    *   completed, the Job has been successfully set up and started.
    */
  def lookupJob(jid: Int): Option[ Future[JobHandle] ] = {
    jobHandles.get(jid)
  }
}


class ViperCoreServer(private val _args: Array[String]) {

  // --- VCS : Configuration ---
  var isRunning: Boolean = true

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val executionContext  = ExecutionContext.global
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  private var _config: ViperConfig = _
  final def config: ViperConfig = _config

  private var _logger: ViperLogger = _
  final def logger: ViperLogger = _logger

  protected var jobs: JobPool = _

  // --- Actor: Terminator ---

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
            .flatMap(_.unbind()) // trigger unbinding from the port
            .onComplete(_ => {
              system.terminate() // and shutdown when done
            })
          case None =>
            system.terminate() // shutdown
        }
      case Terminator.WatchJobQueue(jid, handle) =>
        // Terminating a job means removing its handle from the map _jobHandles. However, before the
        // terminator can delete the jobHandle, it has to wait for all the messages to be consumed.
        // This is indicated by the completion of the queue_completion_future.
        val queue_completion_future: Future[Done] = handle.queue.watchCompletion()
        queue_completion_future.onComplete({
          case Success(_) =>
            println(s"Terminator deleted job #$jid")
            jobs.discardJob(jid)
          case Failure(e) =>
            println(s"Terminator detected failure in job #$jid: $e")
            throw e
        })
    }
  }


  // --- Actor: MainActor ---

  object JobActor {
    def props(id: Int, logger: ViperLogger): Props = Props(new JobActor(id, logger))
  }

  // (See model description in ViperServerProtocol.scala)
  class JobActor(private val id: Int, private val logger: ViperLogger) extends Actor {

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

      case Stop() =>
        val did_I_interrupt = interrupt
        if (did_I_interrupt) {
          sender ! s"Job #$id has been successfully interrupted."
        } else {
          sender ! s"Job #$id has already been finalized."
        }

      case Verify(args, program) =>
        resetVerificationTask()
        sender ! verify(args, program)

      case msg =>
        throw new Exception("Job Actor: unexpected message received: " + msg)
    }

    private def verify(args: List[String], program: ast.Program): JobHandle = {
      val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
                                     .toMat(Sink.asPublisher(false))(Keep.both)
                                     .run()
      val queueActor = system.actorOf(QueueActor.props(id, queue), s"reporter_$id")

      _verificationTask = new Thread(new VerificationWorker(queueActor, config, logger.get, args, program))
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


  // --- VCS : Core Functions ---

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
    _config = new ViperConfig(_args)
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    jobs = new JobPool(config.maximumActiveJobs())

    ViperCache.initialize(logger.get, config.backendSpecificCache())

    routes match {
      case Some(routes) =>
        val port = config.port()
        val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes(logger), "localhost", port)

        _termActor = system.actorOf(Terminator.props(bindingFuture), "terminator")
        println(s"ViperServer online at http://localhost:$port")
      case None =>
        _termActor = system.actorOf(Terminator.props(), "terminator")
        println(s"ViperServer online in CoreServer mode")
    }
  }

  /** Verifies a Viper AST using the specified backend.
    *
    * Returns an VerificationJobHandler whose ID uniquely identifies the verification job.
    * Returns a VerificationJobHandler whose ID is -1 if the job failed to start (e.g., because
    * there are too many active jobs).
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

    if (jobs.newJobsAllowed) {
      val (id, _) = jobs.bookNewJob((new_jid: Int) => {
        implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
        val jobActor = system.actorOf(JobActor.props(new_jid, logger), s"main_actor_$new_jid")
        val answer = jobActor ? ViperServerProtocol.Verify(args, program)
        val new_job_handle: Future[JobHandle] = answer.mapTo[JobHandle]
        new_job_handle
      })
      VerificationJobHandler(id)
    } else {
      println(s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS}).")
      VerificationJobHandler(-1) // Not able to create a new JobHandle
    }
  }

  /** Stream all messages generated by the backend to some actor.
    *
    * This method deletes the jobhandle on completion.
    *
    * If the method returns
    *
    *  - None, the verification job does not exist
    *  - Some(completionFuture), where completionFuture indicates that the verification is in progress
    *    as long as the future is not resolved.
    */
  def streamMessages(jid: Int, clientActor: ActorRef): Option[Future[Unit]] = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        def mapHandle(handle: JobHandle): Future[Unit] = {
          Source.fromPublisher(handle.publisher).runWith(Sink.actorRef(clientActor, Success))
          // As soon as messages start being consumed, the terminator actor is triggered.
          // See Terminator.receive for more information
          _termActor ! Terminator.WatchJobQueue(jid, handle)
          handle.queue.watchCompletion().map(_ => ())
        }
        Some(handle_future.flatMap(mapHandle))
      case None =>
        clientActor ! new NoSuchElementException(s"The verification job #$jid does not exist.")
        None
    }
  }

  def flushCache(): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    ViperCache.resetCache()
    println(s"The cache has been flushed successfully.")
  }

  /** Stops an instance of ViperCoreServer from running.
    *
    * As such it should be the lasts method called. Calling any other function after 'stop()' will result in an
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


  // --- VCS : Auxiliary Functions ---

  protected def getInterruptFutureList(): Future[List[String]] = {
    val interrupt_future_list: List[Future[String]] = jobs.jobHandles map { case (_, handle_future) =>
      handle_future.flatMap {
        case JobHandle(actor, _, _) =>
          implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
          (actor ? Stop()).mapTo[String]
      }
    } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }
}