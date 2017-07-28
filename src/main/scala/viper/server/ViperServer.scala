/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import akka.http.scaladsl.Http
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import akka.util.Timeout

import scala.concurrent.Future
import akka.stream.scaladsl.{Sink, Source, SourceQueue, SourceQueueWithComplete}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import viper.server.ViperIDEProtocol._
import viper.server.ViperServerProtocol._

import scala.concurrent.duration._
import scala.language.postfixOps
import viper.silver.reporter
import viper.silver.reporter.{Message, PongMessage, SuccessMessage}

import scala.collection.mutable



object ViperServerRunner {

  import ViperRequests._

  private var _config: ViperConfig = _
  final def config: ViperConfig = _config

  implicit val system = ActorSystem("Main")
  implicit val materializer = ActorMaterializer()


  // --- Actor: Terminator ---

  private var _term_actor: ActorRef = _

  object Terminator {
    case object Exit

    def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(bindingFuture))
  }

  class Terminator(bindingFuture: Future[Http.ServerBinding]) extends Actor {
    implicit val executionContext = system.dispatcher

    override def receive = {
      case Terminator.Exit =>
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate()) // and shutdown when done
    }
  }


  // --- Actor: MainActor ---

  // We can potentially have more than one verification task at the same time.
  // A verification task is distinguished via the corresponding ActorRef,
  //  as well as its unique job_id.

  case class JobHandle(job_id: Int,
                       queueSource: Source[reporter.Message, SourceQueueWithComplete[reporter.Message]],
                       futureQueue: Future[SourceQueueWithComplete[reporter.Message]])

  private var _main_actor = mutable.Map[ActorRef, JobHandle]()
  private var _next_job_id: Int = 0
  private val _max_active_jobs: Int = 3

  def new_jobs_allowed = _main_actor.size < _max_active_jobs

  def find_job(jid: Int): Option[JobHandle] = _main_actor.find( _._2.job_id == jid ) match {
    case Some((a_ref: ActorRef, j_handle: JobHandle)) =>
      Some(j_handle)
    case _ =>
      None
  }

  // (See model description in ViperServerProtocol.scala)

  object MainActor {
    def props(id: Int): Props = Props(new MainActor(id))
  }



  class MainActor(private val id: Int) extends Actor {

    implicit val executionContext = system.dispatcher

    private var _verificationTask: Thread = null
    private var _args: List[String] = null

    // Set a timeout for the main actor. Expect the client to request results within this time.
    private val _timeout = system.scheduler.scheduleOnce(500.seconds, self, Stop)

    def receive = {
      case Stop =>
        _timeout.cancel()

        // Can be sent wither from the client for terminating the job,
        // or by the server (indicating that the job has been completed).
        if (_verificationTask != null && _verificationTask.isAlive) {
          _verificationTask.interrupt()
          _verificationTask.join()
        } else {
          // The backend has already stopped.
          //TODO: send appropriate message to client.
        }
        _main_actor -= self
        self ! PoisonPill
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

    def verify(args: List[String]): Unit = {
      assert(_verificationTask == null)

      // The maximum messages in the reporter's message queue is 10.
      // TODO: parametrize?
      // TODO: choose the right strategy?
      // FIXME: the message type is not supposed t be String.

      val (queueSource, futureQueue) = utility.Futures.peekMatValue(Source.queue[reporter.Message](10, OverflowStrategy.fail))

      futureQueue.map { queue =>
        val my_reporter = system.actorOf(ReporterActor.props(queue), s"reporter_$id")

        _verificationTask = new Thread(new VerificationWorker(self, my_reporter, args))
        _verificationTask.start()

        queue.watchCompletion().map { done =>
          println(s"Client #$id disconnected")
          //tickSchedule.cancel
        }
      }

      _main_actor(self) = JobHandle(id, queueSource, futureQueue)
      _next_job_id = _next_job_id + 1
    }
  }


  // --- Actor: ReporterActor ---

  object ReporterActor {
    case object ClientRequest
    case class ServerRequest(msg: reporter.Message)
    case object FinalServerRequest

    def props(bindingFuture: SourceQueueWithComplete[reporter.Message]): Props = Props(new ReporterActor(bindingFuture))
  }

  class ReporterActor(queue: SourceQueueWithComplete[reporter.Message]) extends Actor {

    def receive = {
      case ReporterActor.ClientRequest =>
      case ReporterActor.ServerRequest(msg) =>
        queue.offer(msg)
      case ReporterActor.FinalServerRequest =>
        queue.offer(reporter.PongMessage("Done"))
        queue.complete()
        self ! PoisonPill
      case _ =>
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val executionContext = system.dispatcher

    import spray.json._

    try {
      println("This is the Viper Server.")
      parseCommandLine(args)

    } catch { case e: Throwable =>
      println(s"Cannot parse CMD arguments: $e")
      sys.exit(1)
    }

    ViperCache.initialize(config.backendSpecificCache())

    val routes = {
      path("exit") {
        get {
          _term_actor ! Terminator.Exit
          complete( ServerStopConfirmed("shutting down...") )
        }
      }
    } ~ path("verify") {
      post {
        entity(as[VerificationRequest]) { r =>
          if ( new_jobs_allowed ) {
            val id = _next_job_id
            val main_actor = system.actorOf(MainActor.props(id), s"main_actor_$id")

            main_actor ! ViperServerProtocol.Verify(r.arg.split("\\s+").toList)

            complete( VerificationRequestAccept(id) )

          } else {
            complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${_max_active_jobs}).") )
          }
        }
      }
    } ~ path("verify" / IntNumber) { jid =>
      get {
        find_job(jid) match {
          case Some(handle) =>
            // Found a job with this jid.
            val src = handle.queueSource.asInstanceOf[ Source[PongMessage, NotUsed] ]
            complete( src )
          case _ =>
            // Did not find a job with this jid.
            complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
        }
      }
    }

    //FIXME: unhardcode the port number!
    val port = 50424 //viper.server.utility.Sockets.findFreePort
    val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes, "localhost", port)
    println(s"Server online at http://localhost:$port")

    _term_actor = system.actorOf(Terminator.props(bindingFuture), "terminator")



  } // method main

  def parseCommandLine(args: Seq[String]) {
    _config = new ViperConfig(args)
    _config.verify()
  }

} // object ViperServerRunner