/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.typesafe.scalalogging.LazyLogging
import org.rogach.scallop.{ScallopConf, ScallopOption, singleArgConverter}
import org.slf4j.LoggerFactory
import viper.carbon.CarbonVerifier
import viper.server.ViperServerProtocol._
import viper.silicon.Silicon
import viper.silver.verifier.Verifier

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import akka.NotUsed
import akka.actor.FSM.Failure
import akka.actor.Status.Success
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import akka.util.Timeout

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import akka.stream.scaladsl.{Sink, Source, SourceQueue, SourceQueueWithComplete}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.typesafe.scalalogging.LazyLogging
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsObject, JsString, JsValue, RootJsonFormat}
import viper.carbon.CarbonVerifier
import viper.server.ViperServerProtocol._
import viper.silicon.Silicon
import viper.silver.verifier.{AbstractError, AbstractVerificationError, VerificationResult, Verifier}
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn, SourcePosition}
import java.nio.file.{Path, Paths}

import scala.language.postfixOps
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import viper.silver.reporter
import viper.silver.reporter.Message

import scala.collection.mutable

//import scala.concurrent.ExecutionContext.Implicits.global


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
                       queueSource: Source[String, SourceQueueWithComplete[String]],
                       futureQueue: Future[SourceQueueWithComplete[String]])

  private var _main_actor = mutable.Map[ActorRef, JobHandle]()

  private var _next_job_id: Int = 0
  private val _max_active_jobs: Int = 1

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

    // The maximum messages in the reporter's message queue is 10.
    // TODO: parametrize?
    // TODO: choose the right strategy?
    // FIXME: the message type is not supposed t be String.
    //                                            utility.Futures.peekMatValue(Source.queue[String](10, OverflowStrategy.fail))

    private val src: Source[String, SourceQueueWithComplete[String]] = Source.queue[String](10, OverflowStrategy.fail)

    private val (_queueSource, _futureQueue: Future[SourceQueueWithComplete[String]]) = {
      val p = Promise[SourceQueueWithComplete[String]]()
      val s = src.mapMaterializedValue { m =>
        p.trySuccess(m)
        m
      }
      (s, p.future)
    }

    private var _reporter: ActorRef = null

    println("A")
    _futureQueue.map { queue =>
      _reporter = system.actorOf(ReporterActor.props(queue), s"reporter_$id")
      val tickSchedule = system.scheduler.schedule(
          0 milliseconds, 1 second,
          _reporter,
          ReporterActor.ServerRequest(reporter.PongMessage(s"pong_from_$id")))

      println("B")

      queue.watchCompletion().map { done =>
        println(s"Client #$id disconnected")
        tickSchedule.cancel
        println(s"Scheduler for job $id canceled")
      }

      println("C")
    }

    assert(_reporter != null)

    _main_actor(self) = JobHandle(id, _queueSource, _futureQueue)

    private var _verificationTask: Thread = null
    private var _args: List[String] = null

    def receive = {
      case Stop =>
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
      _verificationTask = new Thread(new VerificationWorker(self, _reporter, args))
      _verificationTask.start()
    }
  }


  // --- Actor: ReporterActor ---

  object ReporterActor {
    case object ClientRequest
    case class ServerRequest(msg: reporter.Message)

    def props(bindingFuture: SourceQueue[String]): Props = Props(new ReporterActor(bindingFuture))
  }

  class ReporterActor(queue: SourceQueue[String]) extends Actor {

    def receive = {
      case ReporterActor.ClientRequest =>
      case ReporterActor.ServerRequest(msg) =>
        queue.offer(msg.toString)
      case _ =>
    }
  }

  def main(args: Array[String]): Unit = {

    try {
      println("This is the Viper Server.")
      parseCommandLine(args)

    } catch { case e: Throwable =>
      println(s"Cannot parse CMD arguments: $e")
      sys.exit(1)
    }

    ViperCache.initialize(config.backendSpecificCache())

    // Normally, the actors can be initialized here.
    // Only the terminator actor needs to be initialized after the Http handler has been created.



    /*
    _futureQueue.map { queue =>

      _reporter_actor = system.actorOf(ReporterActor.props(queue), "reporter")
      val tickSchedule =
        system.scheduler.schedule(0.milliseconds,
          1 second,
          _reporter_actor,
          ReporterActor.ServerRequest(reporter.PongMessage("pong")))

      queue.watchCompletion().map{ done =>
        println("Client disconnected")
        tickSchedule.cancel
        println("Scheduler canceled")
      }
    }
*/


    val routes = {
      path("exit") {
        get {
          _term_actor ! Terminator.Exit
          complete((StatusCodes.Accepted, "shutting down..."))
        }
      }
    } ~ path("verify" / IntNumber) { jid =>
      post {
        entity(as[VerificationRequest]) { r =>
          if ( new_jobs_allowed ) {

            implicit val executionContext = system.dispatcher

            val id = _next_job_id
            val main_actor = system.actorOf(MainActor.props(id), s"main_actor_$id")

            main_actor ! ViperServerProtocol.Verify(r.arg.split("\\s+").toList)

            println("[Main App] main_actor = ", main_actor)

            // Activate the future queue for the new job.
            //assert(_main_actor.contains(main_actor))

            complete((StatusCodes.Accepted, s"submitted verification request to main actor #$id: `${r.arg}'."))

          } else {
            complete((StatusCodes.Accepted, s"the maximum number of active verification jobs are currently running (${_max_active_jobs})."))
          }
        }
      } ~
      get {
        find_job(jid) match {
          case Some(handle) =>
            // Found a job with this jid.
            println(s"[Main App] job_handle(jid=$jid) = ", handle)
            complete(HttpEntity(
              ContentTypes.`application/json`,
              handle.queueSource.map{ e => ByteString(s"$e\n") }
          ))
          case _ =>
            // Did not find a job with this jid.
            complete((StatusCodes.NotFound, s"The verification job #$jid does not exist."))
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