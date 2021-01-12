// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.{Callable, FutureTask}

import scala.language.postfixOps
import akka.actor.ActorRef
import akka.pattern.ask
import akka.stream.QueueOfferResult
import akka.util.Timeout
import viper.server.core.VerificationExecutionContext

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}



/** This class is a generic wrapper for a any sort of task a VerificationServer might
  * work on.
  *
  * It has the following properties:
  *  - implements callable and provides an artifact future that completes when the task terminates
  *  - provides a reference to a queue actor.
  *
  *  The first serves the purpose of running the task concurrently. The second allows to
  *  communicate from the verification process to the server.
  * */
abstract class MessageStreamingTask[T]() extends Callable[T] with Post {

  private lazy val artifactPromise = Promise[T]()
  lazy val artifact: Future[T] = artifactPromise.future
  lazy val futureTask: FutureTask[T] = new FutureTask(this) {
    override def done(): Unit = artifactPromise.complete(Try(get()))
  }

  private var q_actor: ActorRef = _

  final def setQueueActor(actor: ActorRef): Unit = {
    q_actor = actor
  }

  /** Sends massage to the attached actor.
    *
    * The actor receiving this message offers it to a queue. This offering returns a Future,
    * which  will eventually indicate whether or not the offer was successful. This method is
    * blocking, as it waits for the successful completion of such an offer.
    * */
  protected def enqueueMessage(msg: Envelope)(implicit executor: VerificationExecutionContext): Unit = {
    // FIXME ATG: this method needs a review
    implicit val askTimeout: Timeout = Timeout(5000 milliseconds)

    val answer = q_actor ? TaskProtocol.BackendReport(msg)
    val current_offer: Future[QueueOfferResult] = answer transformWith {
      case Success(current_offer) if current_offer.isInstanceOf[Future[QueueOfferResult]] =>
        current_offer.asInstanceOf[Future[QueueOfferResult]]
      case Success(s) => throw new IllegalStateException(s"unexpected answer received from queue actor: $s")
      case Failure(exception) =>
        println(s"enqueuing message into queue has failed with exception: $exception")
        Future.failed(exception)
    }
    // wait until either answer completes with a timeout or current_offer completes (an exception is thrown if
    // current_offer completes with a failure
    Await.result(current_offer, Duration.Inf)
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
