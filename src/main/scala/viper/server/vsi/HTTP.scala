// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.NotUsed
import akka.actor.PoisonPill
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.scaladsl.Source
import akka.util.Timeout
import viper.server.vsi.VerificationProtocol.StopVerification

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** This trait contains the bear essentials required for an HTTP server.
  * */
sealed trait BasicHttp {

  /** Specifies the port through which the clients may access this server instance.
    *
    * The default port number tells the system to automatically pick an available port
    * (depends on the implementation of the underlying socket library)
    * */
  var port: Int = _

  /** Must be implemented to return the routes defined for this server.
    * */
  def routes(): Route
}

/** This trait provides the functionality to add additional routes to an HTTP Server.
  * */
sealed trait CustomizableHttp extends BasicHttp {

  /** This method merges two given routes into one.
    * */
  def addRoute(existingRoute: Route, newRoute: Route): Route = {
    existingRoute ~ newRoute
  }
}

/** This trait extends a VerificationServer by providing common HTTP functionality.
  *
  *  Examples for such functionality are:
  *
  *     - Stopping a VerifiationServer.
  *     - Submitting verification requests.
  *     - Requesting results for verification requests.
  *
  * The VerificationServer-specific functionality will need to be implemented for each individual
  * server. In particular, this means providing a protocol that returns the VerificationServer's
  * responses as type [[ToResponseMarshallable]].
  * */
trait VerificationServerHttp extends VerificationServer with CustomizableHttp {

  def setRoutes(): Route

  var bindingFuture: Future[Http.ServerBinding] = _

  override def start(active_jobs: Int): Unit = {
    ast_jobs = new JobPool("AST-pool", active_jobs)
    ver_jobs = new JobPool("Verification-pool", active_jobs)
    bindingFuture = Http().newServerAt("localhost", port).bindFlow(setRoutes())
    _termActor = system.actorOf(Terminator.props(ast_jobs, ver_jobs, Some(bindingFuture)), "terminator")
    isRunning = true
  }

  /** Implement VerificationServer- specific handling of server shutdown.
    * (Response should depend on interruption state.)
    * */
  def serverStopConfirmation(interruption_future: Try[List[String]]): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of VerificationRequests.
    * */
  def onVerifyPost(vr: Requests.VerificationRequest): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a request for streaming results.
    * */
  def unpackMessages(s: Source[Envelope, NotUsed]): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a failed request for streaming results.
    * */
  def verificationRequestRejection(jid: Int, e: Throwable): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a successful request for discarding a job.
    * */
  def discardJobConfirmation(jid: Int, msg: String): ToResponseMarshallable

  /** Implement VerificationServer- specific handling of a failed request for for discarding a job.
    * */
  def discardJobRejection(jid: Int): ToResponseMarshallable

  override final def routes(): Route = {
    /** Send GET request to "/exit".
      */
    path("exit") {
      get {
        onComplete(stop()) { err: Try[List[String]] =>
          complete( serverStopConfirmation(err) )
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
  } ~ path("ast" / IntNumber) { id =>
    val ast_id = AstJobId(id)
    get {
      ast_jobs.lookupJob(ast_id) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              val s: Source[Envelope, NotUsed] = Source.fromPublisher(handle.publisher)
              complete(unpackMessages(s))
            case Failure(error) =>
              // TODO use AST-specific response
              complete(verificationRequestRejection(id, error))
          }
        case None =>
          // TODO use AST-specific response
          complete(verificationRequestRejection(id, JobNotFoundException))
      }
    }
  } ~ path("verify" / IntNumber) { id =>
    /** Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    val ver_id = VerJobId(id)
    get {
      ver_jobs.lookupJob(ver_id) match {
        case None =>
          /** Verification job with this ID is not found. */
          complete(verificationRequestRejection(id, JobNotFoundException))

        case Some(handle_future) =>
          /** Combine the future AST and the future verification results. */
          onComplete(handle_future.flatMap((ver_handle: VerHandle) => {
            /** If there exists a verification job, there should have existed
              * (or should still exist) a corresponding AST construction job. */
            val ast_id: AstJobId = ver_handle.prev_job_id.get

            /** The AST construction job may have been cleaned up
              * (if all of its messages were already consumed) */
            ast_jobs.lookupJob(ast_id) match {
              case Some(ast_handle_fut) =>
                ast_handle_fut.map(ast_handle => (Some(ast_handle), ver_handle))
              case None =>
                Future.successful((None, ver_handle))
            }
          })) {
            case Success((ast_handle_maybe, ver_handle)) =>
              val ver_source = ver_handle match {
                case VerHandle(null, null, null, _) =>
                  /** There were no messages produced during verification. */
                  Source.empty[Envelope]
                case _ =>
                  Source.fromPublisher(ver_handle.publisher)
              }
              val ast_source = ast_handle_maybe match {
                case None =>
                  /** The AST messages were already consumed. */
                  Source.empty[Envelope]
                case Some(ast_handle) =>
                  Source.fromPublisher(ast_handle.publisher)
              }
              val resulting_source = ver_source.prepend(ast_source)
              complete(unpackMessages(resulting_source))
            case Failure(error) =>
              complete(verificationRequestRejection(id, error))
          }
      }
    }
  } ~ path("discard" / IntNumber) { id =>
    /** Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    val ver_id = VerJobId(id)
    get {
      ver_jobs.lookupJob(ver_id) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
              val interrupt_done: Future[String] = (handle.job_actor ? StopVerification).mapTo[String]
              onSuccess(interrupt_done) { msg =>
                handle.job_actor ! PoisonPill // the actor played its part.
                complete(discardJobConfirmation(id, msg))
              }
            case Failure(_) =>
              complete(discardJobRejection(id))
          }
        case _ =>
          complete(discardJobRejection(id))
      }
    }
  }
}