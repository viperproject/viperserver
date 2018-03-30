/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server


import org.reactivestreams.Publisher

import scala.language.postfixOps
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.collection.mutable
import scala.util.{Failure, Success}
import akka.{Done, NotUsed}
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import ch.qos.logback.classic.Logger
import viper.server.ViperServerProtocol._
import viper.server.ViperIDEProtocol._
import viper.silver.reporter
import viper.silver.reporter._

import scala.util.Try


object ViperServerRunner {

  import ViperRequests._

  private var _config: ViperConfig = _
  final def config: ViperConfig = _config
  private var _logger: Logger = _
  private def logger = _logger

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val materializer: ActorMaterializer = ActorMaterializer()


  // --- Actor: Terminator ---

  private var _term_actor: ActorRef = _

  object Terminator {
    case object Exit
    case class WatchJob(jid: Int)

    def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(bindingFuture))
  }

  class Terminator(bindingFuture: Future[Http.ServerBinding]) extends Actor {
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    override def receive: PartialFunction[Any, Unit] = {
      case Terminator.Exit =>
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate()) // and shutdown when done
      case Terminator.WatchJob(jid) =>
        _job_handles.get(jid) match {
          case Some(handle@ JobHandle(_, queue, _)) =>
            val queue_completion_future: Future[Done] = queue.watchCompletion()
            queue_completion_future.onSuccess({ case _ =>
              _job_handles -= jid
              println(s"Terminator deleted job #$jid")
            })
            queue_completion_future.onFailure({ case e =>
              println(s"Terminator detected failure in job #$jid: $e")
              throw e
            })
          case _ =>
            val e = s"Terminator: job #$jid does not exist."
            println(e)
            throw new Exception(e)
        }
    }
  }


  // --- Actor: MainActor ---

  // We can potentially have more than one verification task at the same time.
  // A verification task is distinguished via the corresponding ActorRef,
  //  as well as its unique job_id.

  case class JobHandle(controller_actor: ActorRef,
                       queue: SourceQueueWithComplete[Message],
                       publisher: Publisher[Message])

  private var _job_handles = mutable.Map[Int, JobHandle]()
  private var _next_job_id: Int = 0
  private val _max_active_jobs: Int = 3

  private def new_jobs_allowed = _job_handles.size < _max_active_jobs

  // (See model description in ViperServerProtocol.scala)

  object MainActor {
    def props(id: Int): Props = Props(new MainActor(id))
  }

  class MainActor(private val id: Int) extends Actor {

    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    private var _verificationTask: Thread = _
    private var _args: List[String] = _

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
      case Verify(args) =>
        if (_verificationTask != null && _verificationTask.isAlive) {
          _args = args
          _verificationTask.interrupt()
          _verificationTask.join()
        }
        _verificationTask = null
        verify(args)
      case msg =>
        throw new Exception("Main Actor: unexpected message received: " + msg)
    }

    private def verify(args: List[String]): Unit = {
      assert(_verificationTask == null)

      // TODO: reimplement with [[SourceQueue]]s and backpressure strategies.

      // The maximum number of messages in the reporter's message buffer is 10000.
      val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure).toMat(Sink.asPublisher(false))(Keep.both).run()

      val my_reporter = system.actorOf(ReporterActor.props(id, queue), s"reporter_$id")

      _verificationTask = new Thread(new VerificationWorker(my_reporter, _logger, args))
      _verificationTask.start()

      assert(_job_handles.get(id).isEmpty)
      _job_handles(id) = JobHandle(self, queue, publisher)
      _next_job_id = _next_job_id + 1

      println(s"Starting job #$id...")
    }
  }


  // --- Actor: ReporterActor ---

  object ReporterActor {
    case object ClientRequest
    case class ServerReport(msg: reporter.Message)
    case class FinalServerReport(success: Boolean)

    def props(jid: Int, queue: SourceQueueWithComplete[Message]): Props = Props(new ReporterActor(jid, queue))
  }

  class ReporterActor(jid: Int, queue: SourceQueueWithComplete[Message]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case ReporterActor.ClientRequest =>
      case ReporterActor.ServerReport(msg) =>
        //println(msg)
        queue.offer(msg)
      case ReporterActor.FinalServerReport(success) =>
        queue.complete()
        if ( success )
          println(s"Job #$jid has been completed successfully.")
        else
          println(s"Job #$jid has been completed ERRONEOUSLY.")
        self ! PoisonPill
      case _ =>
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    try {
      parseCommandLine(args)

    } catch { case e: Throwable =>
      println(s"Cannot parse CMD arguments: $e")
      sys.exit(1)
    }

    _logger = new ViperServerLogger(config.canonizedLogFile(), config.logLevel()).get
    println(s"Writing [level ${config.logLevel()}] logs to ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${config.canonizedLogFile()}")
    ViperCache.initialize(config.backendSpecificCache())

    val routes = {
      /**
        * Send GET request to "/exit".
        *
        * This will do the following:
        * 1. Map all existing jobs from [[_job_handles]] to a list of futures based on the responses of instances of
        *    [[MainActor]] from [[ViperServerProtocol.Stop]] messages (with a 5 second timeout)
        * 2. Merge the list into an overall future
        * 3. Wait for the overall future to complete or timeout
        * 4. Send [[Terminator.Exit]] message to the [[Terminator]] actor
        * 5. Complete the request with an appropriate message
        *
        * Some use cases:
        * - The IDE is being shut down
        * - ViperServer receives a deadly signal and needs to kill the running verification jobs
        *
        * */
      path("exit") {
        get {
          val interrupt_future_list: List[Future[String]] = _job_handles map { case (jid, handle@JobHandle(actor, _, _)) =>
            implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
            (actor ? Stop(true)).mapTo[String]
          } toList
          val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)

          onComplete(overall_interrupt_future) { (err: Try[List[String]]) =>
            err match {
              case Success(_) =>
                _term_actor ! Terminator.Exit
                complete( ServerStopConfirmed("shutting down...") )
              case Failure(err_msg) =>
                println(s"Interrupting one of the verification threads timed out: $err_msg")
                _term_actor ! Terminator.Exit
                complete( ServerStopConfirmed("forcibly shutting down...") )
            }
          }
        }
      }
    } ~ path("verify") {
      /**
        * Send POST request to "/verify".
        *
        * This will do the following:
        * 1. If the limit for allowed active verification jobs has been reached, complete with an appropriate reject message
        * 2. Otherwise, create an actor with a fresh ID and pass the arguments provided by the client
        * 3. Send [[ViperServerProtocol.Verify]] message to the newly created instance of [[MainActor]]
        *   (the actor will add itself as an entry to the [[_job_handles]] collection)
        * 4. Complete request with accepting message with the ID of the new verification job
        *
        * Use case:
        * - Send a request to verify a specific Viper file from the IDE
        * - Send a request to verify a specific Viper file from any other Viper client implementation,
        *   such as <a href="https://bitbucket.org/viperproject/viper_client">viper_client</a> (written in Python)
        */
      post {
        entity(as[VerificationRequest]) { r =>
          if (new_jobs_allowed) {
            val id = _next_job_id
            val main_actor = system.actorOf(MainActor.props(id), s"main_actor_$id")
            var arg_list = getArgListFromArgString(r.arg)
            main_actor ! ViperServerProtocol.Verify(arg_list)
            complete( VerificationRequestAccept(id) )

          } else {
            complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${_max_active_jobs}).") )
          }
        }
      }
    } ~ path("verify" / IntNumber) { jid =>

      /**
        * Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
        * <jid> must be an ID of an existing verification job.
        *
        * This will do the following:
        * 1. If no job handle with ID equal to <jid> exists in [[_job_handles]], complete with an appropriate reject message
        * 2. Otherwise:
        *   - Create a [[Source]] <src> full of [[viper.silver.reporter.Message]]s
        *   - Send [[Terminator.WatchJob]] message to the [[Terminator]] actor, awaiting
        *     [[SourceQueueWithComplete.watchCompletion]] future before removing current job handle from [[_job_handles]]
        *   - Complete request with <src>
        *
        * Use case:
        * - Ask ViperServer to begin streaming the results corresponding to the verification job with provided <jid>
        *
        */
      get {
        _job_handles.get(jid) match {
          case Some(handle) =>
            // Found a job with this jid.
            val src: Source[Message, NotUsed] = Source.fromPublisher(handle.publisher)
            // We do not remove the current entry from [[_job_handles]] because the handle is
            //  needed in order to terminate the job before streaming is completed.
            //  The Terminator actor will delete the entry upon completion of the stream.
            _term_actor ! Terminator.WatchJob(jid)
            complete(src)
          case _ =>
            // Did not find a job with this jid.
            complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
        }
      }
    } ~ path("discard" / IntNumber) { jid =>

      /**
        * Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
        * <jid> must be an ID of an existing verification job.
        *
        * This will do the following:
        * 1. If no job handle with ID equal to <jid> exists in [[_job_handles]], complete with an appropriate reject message
        * 2. Otherwise:
        *   - Create a new future based on response from a the current instance of [[MainActor]] to a
        *   [[ViperServerProtocol.Stop]] message (with a 5 second timeout)
        *   - Send a [[PoisonPill]] message to the current instance of [[MainActor]]
        *   - Complete request with accepting message
        *
        *  Use case:
        *  - Client decided to kill a verification job it no linger cares about
        */
      get {
        _job_handles.get(jid) match {
          case Some(handle) =>
            implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
            val interrupt_done: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
            onSuccess(interrupt_done) { msg =>
                handle.controller_actor ! PoisonPill // the actor played its part.
                complete( JobDiscardAccept(msg) )
            }
          case _ =>
            // Did not find a job with this jid.
            complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
        }
      }
    }

    val port = viper.server.utility.Sockets.findFreePort
    val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes, "localhost", port)

    println(s"ViperServer online at http://localhost:$port")

    _term_actor = system.actorOf(Terminator.props(bindingFuture), "terminator")

  } // method main

  def parseCommandLine(args: Seq[String]) {
    _config = new ViperConfig(args)
    _config.verify()
  }

  private def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

} // object ViperServerRunner

