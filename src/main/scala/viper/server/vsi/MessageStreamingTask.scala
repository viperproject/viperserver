// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.{Callable, CancellationException, FutureTask}
import scala.language.postfixOps
import akka.actor.ActorRef
import akka.pattern.ask
import akka.stream.QueueOfferResult
import akka.util.Timeout
import ch.qos.logback.classic.Logger

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

  /** guards `callableStarted` and thereby synchronizes starting the task against `cancelledBeforeStart` */
  private val startSync = new Object
  private var callableStarted: Boolean = false // guarded by `startSync`

  lazy val futureTask: FutureTask[T] = new FutureTask[T](() => {
    startSync.synchronized {
      // a `FutureTask` remains in its initial state while the callable is executing, i.e. a
      // successful `cancel` alone cannot tell a task that will never run apart from a task that is
      // running right now. This handshake makes that distinction: `cancelledBeforeStart` observing
      // a cancelled task without `callableStarted` under this lock guarantees that this check has
      // not run yet and will fail once it does, i.e. that `call()` will never be invoked.
      if (futureTask.isCancelled) {
        throw new CancellationException()
      }
      callableStarted = true
    }
    call()
  }) {
    override def done(): Unit = artifactPromise.complete(Try(get()))
  }

  /** Returns true iff this task has been cancelled without its callable ever starting. A positive
    * answer is stable: the callable is then guaranteed to never run. Callers may hence take over
    * the cancelled task's cleanup duties that the callable would otherwise have performed, such as
    * completing the task's message queue (see `JobActor`).
    */
  final def cancelledBeforeStart: Boolean = startSync.synchronized {
    futureTask.isCancelled && !callableStarted
  }

  private var q_actor: ActorRef = _
  private var hasEnded: Boolean = false

  final def setQueueActor(actor: ActorRef): Unit = {
    if (q_actor != null) {
      throw new IllegalStateException("cannot set queue actor - a queue actor has already been set")
    }

    q_actor = actor
  }

  /** the actor managing this task's message queue; null until `setQueueActor` has been called */
  private[vsi] def queueActor: ActorRef = q_actor

  /** Sends massage to the attached actor.
    *
    * The actor receiving this message offers it to a queue. This offering returns a Future,
    * which  will eventually indicate whether or not the offer was successful. This method is
    * blocking, as it waits for the successful completion of such an offer.
    * */
  protected def enqueueMessage(msg: Envelope, logger: Logger): Unit = {
    if (hasEnded) {
      throw new IllegalStateException("cannot enqueue message - message streaming task's end has already been registered")
    }

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
        logger.error(s"exception in enqueueMessage occurred: $ex")
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
    if (hasEnded) {
      throw new IllegalStateException("cannot register task end - message streaming task's end has already been registered")
    }

    hasEnded = true
    logger.trace(s"registerTaskEnd: $success")
    q_actor ! TaskProtocol.FinalBackendReport(success)
  }
}
