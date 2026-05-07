// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.{Callable, FutureTask}
import ch.qos.logback.classic.Logger

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try



/** Generic wrapper for any task a VerificationServer might work on.
  *
  * Implements Callable and provides an artifact future that completes when the
  * task terminates. Owns the `EnvelopeStream` it pushes backend messages to —
  * created at construction so it's a `final val`, removing the prior
  * post-construction `setStream` indirection and its visibility concerns.
  *
  * `enqueueMessage` and `registerTaskEnd` are both expected to be invoked from
  * the same producer (the worker thread). The stream's own synchronization
  * keeps things safe even if that contract is violated.
  *
  * Completion model: `settled` is the canonical "all internal cleanup is
  * done" signal. Consumers chaining post-completion logic should await
  * `settled` rather than `stream.watchCompletion` or `artifact`, because
  * cleanup hooks registered via `onSettled` are guaranteed to have run
  * before `settled` resolves AND before the stream's Done sentinel can
  * unblock any iterator. This eliminates the multi-signal fanout race
  * where a consumer wakes up on one signal while side-effects registered
  * on another haven't run yet.
  * */
abstract class MessageStreamingTask[T] extends Callable[T] with Post {

  private lazy val artifactPromise = Promise[T]()
  lazy val artifact: Future[T] = artifactPromise.future

  private val settledPromise = Promise[Unit]()
  /** Resolves after the producer has finished and every `onSettled` hook
    * has run. Always resolves with `Success(())` — task failures are
    * surfaced through `artifact`, not here.
    */
  def settled: Future[Unit] = settledPromise.future

  private val cleanupHooks = mutable.ArrayBuffer.empty[() => Unit]
  private var finalized = false

  /** Register cleanup to run inside the finalize sequence, BEFORE
    * `stream.complete()` and BEFORE `settled` resolves. Hooks run
    * synchronously on the producer's thread in registration order.
    *
    * If the task is already finalized, the hook runs inline.
    *
    * Hook exceptions are swallowed so a faulty hook cannot block
    * subsequent hooks or signaling.
    */
  def onSettled(hook: () => Unit): Unit = {
    val runNow = this.synchronized {
      if (finalized) true
      else { cleanupHooks += hook; false }
    }
    if (runNow) {
      try hook() catch { case _: Throwable => /* swallow */ }
    }
  }

  lazy val futureTask: FutureTask[T] = new FutureTask(this) {
    override def done(): Unit = {
      // Backstop: if the task body threw before calling registerTaskEnd,
      // we still need the stream to complete and `settled` to resolve so
      // consumers don't hang. Idempotent — a normal completion already
      // ran finalize and this is a no-op.
      finalizeOnce()
      artifactPromise.complete(Try(get()))
    }
  }

  val stream: EnvelopeStream = new EnvelopeStream()

  /** Offers `msg` to the downstream stream, blocking until queue space is available
    * (natural backpressure when consumers are slow).
    */
  protected def enqueueMessage(msg: Envelope, logger: Logger): Unit = {
    logger.trace(s"enqueueMessage: $msg")
    try {
      stream.offer(msg)
    } catch {
      case ex: Exception =>
        logger.error(s"exception in enqueueMessage occurred: $ex")
        throw ex
    }
  }

  /** Run cleanup hooks, complete the stream, then signal `settled`.
    * Idempotent. Order matters: hooks run BEFORE `stream.complete()` so
    * any iterator waking on the Done sentinel observes post-hook state.
    */
  private def finalizeOnce(): Unit = {
    val hooks = this.synchronized {
      if (finalized) return
      finalized = true
      val h = cleanupHooks.toList
      cleanupHooks.clear()
      h
    }
    hooks.foreach { h => try h() catch { case _: Throwable => /* swallow */ } }
    stream.complete()
    settledPromise.trySuccess(())
  }

  /** Closes the downstream stream, signalling the end of the message stream.
    *
    * @param success retained for API compatibility; stream completion is the same
    *                regardless of success/failure outcome. Idempotent (a second
    *                call is a no-op).
    */
  protected def registerTaskEnd(success: Boolean, logger: Logger): Unit = {
    logger.trace(s"registerTaskEnd: $success")
    finalizeOnce()
  }
}
