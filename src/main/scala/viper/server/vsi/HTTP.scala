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
import viper.server.vsi.VerificationProtocol.Stop

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** This trait contains the bear essentials required for an HTTP server.
  * */
sealed trait BasicHttp {

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
trait VerificationServerHTTP extends VerificationServer with CustomizableHttp {

  def setRoutes(): Route

  var bindingFuture: Future[Http.ServerBinding] = _

  override def start(active_jobs: Int): Unit = {
    jobs = new JobPool(active_jobs)
    bindingFuture = Http().bindAndHandle(setRoutes, "localhost", port)
    _termActor = system.actorOf(Terminator.props(bindingFuture), "terminator")
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
        onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
          _termActor ! Terminator.Exit
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
  } ~ path("verify" / IntNumber) { jid =>
    /** Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    get {
      jobs.lookupJob(JobID(jid)) match {
        case Some(handle_future) =>
          // Found a job with this jid.
          onComplete(handle_future) {
            case Success(handle) =>
              val s: Source[Envelope, NotUsed] = Source.fromPublisher((handle.publisher))
              _termActor ! Terminator.WatchJobQueue(JobID(jid), handle)
              complete(unpackMessages(s))
            case Failure(error) =>
              complete(verificationRequestRejection(jid, error))
          }
        case _ =>
          complete(verificationRequestRejection(jid, JobNotFoundException()))
      }
    }
  } ~ path("discard" / IntNumber) { jid =>
    /** Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      */
    get {
      jobs.lookupJob(JobID(jid)) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
              val interrupt_done: Future[String] = (handle.job_actor ? Stop).mapTo[String]
              onSuccess(interrupt_done) { msg =>
                handle.job_actor ! PoisonPill // the actor played its part.
                complete(discardJobConfirmation(jid, msg))
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