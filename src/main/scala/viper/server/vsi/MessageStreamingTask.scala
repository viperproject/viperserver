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
import ch.qos.logback.classic.Logger
import viper.server.core.VerificationExecutionContext

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try



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
abstract class MessageStreamingTask[T] extends Callable[T] with Post {

  private lazy val artifactPromise = Promise[T]()
  lazy val artifact: Future[T] = artifactPromise.future
  lazy val futureTask: FutureTask[T] = new FutureTask(this) {
    override def done(): Unit = artifactPromise.complete(Try(get()))
  }

  private var q_actor: ActorRef = _
  private var hasEnded: Boolean = false

  final def setQueueActor(actor: ActorRef): Unit = {
    assert(q_actor == null)
    q_actor = actor
  }

  /** Sends massage to the attached actor.
    *
    * The actor receiving this message offers it to a queue. This offering returns a Future,
    * which  will eventually indicate whether or not the offer was successful. This method is
    * blocking, as it waits for the successful completion of such an offer.
    * */
  protected def enqueueMessage(msg: Envelope, logger: Logger): Unit = {
    assert(!hasEnded)
    logger.trace(s"enqueueMessage: $msg")
    implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
    // answer is a future that will resolve with the actor's response to the BackendReport request
    val answer = (q_actor ? TaskProtocol.BackendReport(msg)).mapTo[Future[QueueOfferResult]]
    // currentOffer is the future that the actor will send in its response (assuming that no timeout occurred requesting it from the actor)
    // currentOffer will resolve when the message is dequeued from the queue
    val currentOffer = answer.flatten
    try {
      // note that an exception is thrown if the currentOffer future fails, e.g. because the askTimeout occurred
      Await.result(currentOffer, Duration.Inf)
    } catch {
      case ex: Exception =>
        // print exception such that one sees that something went wrong:
        val errorMsg = s"exception in enqueueMessage occurred: $ex"
        println(errorMsg)
        logger.error(errorMsg)
        // rethrow exception:
        throw ex
    }
  }

  /** Notify the queue actor that the task has come to an end
    *
    * The actor receiving this message will close the queue.
    *
    * @param success indicates whether or not the task has ended as successfully.
    * */
  protected def registerTaskEnd(success: Boolean, logger: Logger): Unit = {
    assert(!hasEnded)
    hasEnded = true
    logger.trace(s"registerTaskEnd: $success")
    q_actor ! TaskProtocol.FinalBackendReport(success)
  }
}
