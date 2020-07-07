package viper.server.vsi

import akka.actor.PoisonPill
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.NotUsed
import akka.pattern.ask
import akka.stream.scaladsl.Source
import akka.util.Timeout

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import viper.server.vsi.VerificationProtocol.Stop

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


/** This expands a VerifiationServer (VerSer) by providing common HTTP functionality.
  *  (E.g.: Stopping a VerSer, submitting Verification Requests and requesting their results)
  *
  * The VerSer- specific functionality will need to be implemented for each individual server.
  * In particular, this means providing a protocol that returns the VerSer's responses as type
  * [[ToResponseMarshallable]].
  * */
trait HttpVerificationServerInterface extends VerificationServerInterface
                                          with CustomizableHttpServer {

  /** Implement VerificationServer- specific handling of server shutdown
    * (Response should depend on interruption state.)
    * */
  def serverStopConfirmation(interruption_future: Try[List[String]]): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of VerificationRequests
    * */
  def onVerifyPost(vr: Requests.VerificationRequest): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a request for streaming results
    * */
  def unpackMessages(s: Source[Envelope, NotUsed]): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a failed request for streaming results
    * */
  def verificationRequestRejection(jid: Int, e: Throwable): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a sucessful request for discarding a job
    * */
  def discardJObConfirmation(jid: Int, msg: String): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a failed request for for discarding a job
    * */
  def discardJobRejection(jid: Int): ToResponseMarshallable

  override def routes(): Route = {
    /** Send GET request to "/exit".
      */
    path("exit") {
      get {
        onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
          err match {
            case Success(_) =>
              _termActor ! Terminator.Exit
              complete( serverStopConfirmation(err) )
            case Failure(err_msg) =>
              println(s"Interrupting one of the verification threads timed out: $err_msg")
              _termActor ! Terminator.Exit
              complete( serverStopConfirmation(err) )
          }
        }
      }
    }
  } ~ path("verify") {
    /** Send POST request to "/verify".
      */
    post {
      entity(as[Requests.VerificationRequest]) { r =>
        complete(onVerifyPost(r))
      }
    }
  } ~ path("verify" / IntNumber) { jid =>
    /** Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    get {
      jobs.lookupJob(jid) match {
        case Some(handle_future) =>
          // Found a job with this jid.
          onComplete(handle_future) {
            case Success(handle) =>
              _termActor ! Terminator.WatchJobQueue(jid, handle)
              val s: Source[Envelope, NotUsed] = Source.fromPublisher((handle.publisher))
              complete(unpackMessages(s))
            case Failure(error) =>
              complete(verificationRequestRejection(jid, error))
          }
        case _ =>
          complete(verificationRequestRejection(jid, new JobNotFoundException()))
      }
    }
  } ~ path("discard" / IntNumber) { jid =>
    /** Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    get {
      jobs.lookupJob(jid) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
              val interrupt_done: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
              onSuccess(interrupt_done) { msg =>
                handle.controller_actor ! PoisonPill // the actor played its part.
                complete(discardJObConfirmation(jid, msg))
              }
            case Failure(_) =>
              complete(discardJobRejection(jid))
          }
        case _ =>
          complete(discardJobRejection(jid))
      }
    }
  }
}

object Requests extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  case class VerificationRequest(arg: String)
  implicit val VerificationRequest_format: RootJsonFormat[VerificationRequest] = jsonFormat1(VerificationRequest.apply)

  case class CacheResetRequest(backend: String, file: String)
  implicit val CacheResetRequest_format: RootJsonFormat[CacheResetRequest] = jsonFormat2(CacheResetRequest.apply)
}