// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.{Future, Promise}

/** Single-producer/single-consumer message stream backed by a bounded
  * `LinkedBlockingQueue`. Replaces the previous `Source.queue` plumbing —
  * `offer` blocks the producer when the queue is full (natural backpressure)
  * and `iterator` blocks the consumer when the queue is empty.
  *
  * `complete` enqueues a sentinel so the iterator terminates cleanly even if
  * the consumer is currently waiting on `take`.
  */
final class EnvelopeStream(capacity: Int = EnvelopeStream.DefaultCapacity) {
  import EnvelopeStream._

  private val queue = new LinkedBlockingQueue[Item](capacity)
  private val completionPromise: Promise[Unit] = Promise()
  @volatile private var completed: Boolean = false

  /** Block until `e` can be enqueued. Throws if the stream is already complete. */
  def offer(e: Envelope): Unit = {
    if (completed) {
      throw new IllegalStateException("EnvelopeStream is already complete")
    }
    queue.put(Msg(e))
  }

  /** Mark the stream complete. Idempotent. */
  def complete(): Unit = this.synchronized {
    if (!completed) {
      completed = true
      queue.put(Done)
      completionPromise.trySuccess(())
    }
  }

  /** Future that resolves when `complete` is called. */
  def watchCompletion: Future[Unit] = completionPromise.future

  /** Single-shot iterator over the stream. Blocks on `hasNext`/`next` until
    * envelopes are available. Returns false from `hasNext` once the
    * completion sentinel is observed.
    */
  def iterator: Iterator[Envelope] = new Iterator[Envelope] {
    private var nextItem: Envelope = _
    private var atEnd: Boolean = false
    private var hasFetched: Boolean = false

    override def hasNext: Boolean = {
      if (atEnd) return false
      if (hasFetched) return true
      queue.take() match {
        case Msg(e) =>
          nextItem = e
          hasFetched = true
          true
        case Done =>
          atEnd = true
          // Re-enqueue the sentinel in case anything else is also draining.
          queue.put(Done)
          false
      }
    }

    override def next(): Envelope = {
      if (!hasNext) throw new NoSuchElementException("EnvelopeStream iterator exhausted")
      val r = nextItem
      hasFetched = false
      nextItem = null.asInstanceOf[Envelope]
      r
    }
  }
}

object EnvelopeStream {
  val DefaultCapacity: Int = 10000

  private sealed trait Item
  private final case class Msg(e: Envelope) extends Item
  private case object Done extends Item
}
