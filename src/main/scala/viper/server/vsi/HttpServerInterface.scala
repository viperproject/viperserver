package viper.server.vsi

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route}

/** At it's core every HTTP server should implement the routes function that describes the HTTP paths.
  * */
trait HttpServerInterface {
  def routes(): Route
}

// Example
//trait BasicHttpServer extends HttpServerInterface {
//
//  def onExit(): Route
//  def onPostRequest(): Route
//  def onGetRequest(jid: Int): Route
//
//  override def routes(): Route = {
//    /**
//      * Send GET request to "/exit".
//      */
//    path("exit") {
//      get {
//        onExit()
//      }
//    }
//  } ~ path("verify") {
//    /**
//      * Send POST request to "/verify".
//      */
//    post {
//      onPostRequest()
//    }
//  } ~ path("verify" / IntNumber) { jid =>
//
//    /**
//      * Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
//      * <jid> must be an ID of an existing verification job.
//      */
//    get {
//      onGetRequest(jid)
//    }
//  }
//}
//
//trait StandardHttpServer extends HttpServerInterface {
//
//  def onExit(): Route
//  def onPostRequest(r: Requests.VerificationRequest): Route
//  def onGetRequest(jid: Int): Route
//
//  def onDiscard(jid: Int): Route
//
//  def onFlushGet(): Route
//  def onFlushPost(r: Requests.CacheResetRequest): Route
//
//  override def routes(): Route = {
//    /**
//      * Send GET request to "/exit".
//      */
//    path("exit") {
//      get {
//        onExit()
//      }
//    }
//  } ~ path("verify") {
//    /**
//      * Send POST request to "/verify".
//      */
//    post {
//      entity(as[Requests.VerificationRequest]){ r =>
//        onPostRequest(r)
//      }
//    }
//  } ~ path("verify" / IntNumber) { jid =>
//
//    /**
//      * Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
//      * <jid> must be an ID of an existing verification job.
//      */
//    get {
//      onGetRequest(jid)
//    }
//  } ~ path("discard" / IntNumber) { jid =>
//
//    /**
//      * Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
//      * <jid> must be an ID of an existing verification job.
//      */
//    get {
//      onDiscard(jid)
//    }
//  } ~ path("cache" /  "flush") {
//    /**
//      * Send GET request to "/cache/flush".
//      *
//      * This will invalidate the entire cache.
//      *
//      *  Use case:
//      *  - Client decided to re-verify several files from scratch.
//      */
//    get {
//      onFlushGet()
//    }
//  } ~ path("cache" /  "flush") {
//    /**
//      * Send POST request to "/cache/flush".
//      *
//      * This will invalidate the cache for the tool and file specified.
//      *
//      *  Use case:
//      *  - Client decided to re-verify the entire file from scratch.
//      */
//    post {
//      entity(as[Requests.CacheResetRequest]){ r =>
//        onFlushPost(r)
//      }
//    }
//  }
//}

/** A customizable HTTPServer additionally provides the means to add Routes.
  * */
trait CustomizableHttpServer extends HttpServerInterface {
  def addRoute(existingRoute: Route, newRoute: Route): Route = {
    existingRoute ~ newRoute
  }
}


