// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.{Callable, FutureTask}
import ch.qos.logback.classic.Logger

import scala.concurrent.{Future, Promise}
import scala.util.Try



/** Generic wrapper for any task a VerificationServer might work on.
  *
  * Implements Callable and provides an artifact future that completes when the
  * task terminates. Holds a reference to an `EnvelopeStream` used to push
  * backend messages to downstream consumers.
  * */
abstract class MessageStreamingTask[T] extends Callable[T] with Post {

  private lazy val artifactPromise = Promise[T]()
  lazy val artifact: Future[T] = artifactPromise.future
  lazy val futureTask: FutureTask[T] = new FutureTask(this) {
    override def done(): Unit = artifactPromise.complete(Try(get()))
  }

  private var stream: EnvelopeStream = _
  private var hasEnded: Boolean = false

  final def setStream(s: EnvelopeStream): Unit = {
    if (stream != null) {
      throw new IllegalStateException("cannot set stream - a stream has already been set")
    }
    stream = s
  }

  /** Offers `msg` to the downstream stream, blocking until queue space is available
    * (natural backpressure when consumers are slow).
    */
  protected def enqueueMessage(msg: Envelope, logger: Logger): Unit = {
    if (hasEnded) {
      throw new IllegalStateException("cannot enqueue message - message streaming task's end has already been registered")
    }

    logger.trace(s"enqueueMessage: $msg")
    try {
      stream.offer(msg)
    } catch {
      case ex: Exception =>
        logger.error(s"exception in enqueueMessage occurred: $ex")
        throw ex
    }
  }

  /** Closes the downstream stream, signalling the end of the message stream.
    *
    * @param success retained for API compatibility; stream completion is the same
    *                regardless of success/failure outcome.
    */
  protected def registerTaskEnd(success: Boolean, logger: Logger): Unit = {
    if (hasEnded) {
      throw new IllegalStateException("cannot register task end - message streaming task's end has already been registered")
    }

    hasEnded = true
    logger.trace(s"registerTaskEnd: $success")
    stream.complete()
  }
}
