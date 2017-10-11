/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server


import org.reactivestreams.Publisher

import scala.language.postfixOps
import scala.concurrent.Future
import scala.collection.mutable

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}

import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._

import viper.server.ViperServerProtocol._
import viper.server.ViperIDEProtocol._

import viper.silver.reporter
import viper.silver.reporter._


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

  case class JobHandle(controller_actor: ActorRef,
                       queue: SourceQueueWithComplete[Message],
                       publisher: Publisher[Message])

  private var _job_handles = mutable.Map[Int, JobHandle]()
  private var _next_job_id: Int = 0
  private val _max_active_jobs: Int = 3

  def new_jobs_allowed = _job_handles.size < _max_active_jobs

  /*
  def find_job(jid: Int): Option[JobHandle] = _job_handles.find( _._2.job_id == jid ) match {
    case Some((a_ref: ActorRef, j_handle: JobHandle)) =>
      Some(j_handle)
    case _ =>
      None
  }*/

  // (See model description in ViperServerProtocol.scala)

  object MainActor {
    def props(id: Int): Props = Props(new MainActor(id))
  }

  class MainActor(private val id: Int) extends Actor {

    implicit val executionContext = system.dispatcher

    private var _verificationTask: Thread = null
    private var _args: List[String] = null


    def receive = {
      case Stop =>
        // Can be sent whether from the client for terminating the job,
        // or by the server (indicating that the job has been completed).
        if (_verificationTask != null && _verificationTask.isAlive) {
          _verificationTask.interrupt()
          _verificationTask.join()
        } else {
          // The backend has already stopped.
          //TODO: send appropriate message to client.
        }
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

      // TODO: reimplement with [[SourceQueue]]s and backpressure strategies.

      // The maximum number of messages in the reporter's message buffer is 1000.
      val (queue, publisher) = Source.queue[Message](1000, OverflowStrategy.backpressure).toMat(Sink.asPublisher(false))(Keep.both).run()

      val my_reporter = system.actorOf(ReporterActor.props(queue), s"reporter_$id")

      _verificationTask = new Thread(new VerificationWorker(self, my_reporter, args))
      _verificationTask.start()

      //println(s"Client #$id disconnected")

      _job_handles(id) = JobHandle(self, queue, publisher)
      _next_job_id = _next_job_id + 1
    }
  }


  // --- Actor: ReporterActor ---

  object ReporterActor {
    case object ClientRequest
    case class ServerRequest(msg: reporter.Message)
    case object FinalServerRequest

    def props(queue: SourceQueueWithComplete[Message]): Props = Props(new ReporterActor(queue))
  }

  class ReporterActor(queue: SourceQueueWithComplete[Message]) extends Actor {

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

    try {
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
      get {
        _job_handles.get(jid) match {
          case Some(handle) =>
            //Found a job with this jid.
            val src: Source[Message, NotUsed] = Source.fromPublisher(handle.publisher)
            _job_handles -= jid
            complete(src)
          case _ =>
            // Did not find a job with this jid.
            complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
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

