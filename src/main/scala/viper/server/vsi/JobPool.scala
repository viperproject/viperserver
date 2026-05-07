// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.{Future, Promise}

sealed trait JobId {
  val id: Int
  def tag: String
  override def toString: String = s"${tag}_id_${id}"
}

case class AstJobId(id: Int) extends JobId {
  def tag = "ast"
}

case class VerJobId(id: Int) extends JobId {
  def tag = "ver"
}

sealed trait JobHandle {
  def tag: String  // identify the kind of job this is
  val execution: JobExecution[_]
  val stream: EnvelopeStream
}

case class AstHandle[R](execution: JobExecution[_],
                        stream: EnvelopeStream,
                        artifact: Future[R]) extends JobHandle {
  def tag = "AST"
}

case class VerHandle(execution: JobExecution[_],
                     stream: EnvelopeStream,
                     prev_job_id: Option[AstJobId]) extends JobHandle {
  def tag = "VER"
}

/** Bounded registry of active jobs. Single mutex guards all access; the API is
  * compound-action-safe.
  *
  * Two maps separate **slot accounting** from **entry visibility**:
  *  - `active` counts toward `MAX_ACTIVE_JOBS`. Booked jobs land here.
  *  - `completed` does not. `markCompleted` moves an entry from `active` to
  *    `completed` so the slot is freed but `lookupJob` still finds it.
  *
  * `markCompleted` is invoked synchronously from `EnvelopeStream.complete`'s
  * sync-hook list, so a consumer awaiting the stream's iterator-drain Future
  * is guaranteed to see `active.size` decremented before its continuation
  * runs. Without this, the iterator's `take()` could unblock before the
  * (async) discard fired, and the next `tryBook` would race-fail.
  */
class JobPool[S <: JobId, T <: JobHandle](val tag: String, val MAX_ACTIVE_JOBS: Int = 3)
                                         (implicit val jid_fact: Int => S) {

  private val active: mutable.Map[S, Future[T]] = mutable.Map()
  private val completed: mutable.Map[S, Future[T]] = mutable.Map()
  private val _nextJobId: AtomicInteger = new AtomicInteger(0)

  /** Active job handles (used at shutdown to interrupt running work). */
  def jobHandles: Map[S, Future[T]] = synchronized { active.toMap }

  /** Atomically check capacity, allocate a fresh id, register a placeholder
    * Future, and start work eagerly via `buildStart`. Returns `None` if at
    * capacity.
    *
    * `synchronized` is reentrant, so `buildStart` may call `discardJob` on
    * this pool synchronously (used by the placeholder/no-task path).
    */
  def tryBook(buildStart: S => Future[T]): Option[S] = synchronized {
    if (active.size >= MAX_ACTIVE_JOBS) None
    else {
      val jid: S = jid_fact(_nextJobId.getAndIncrement())
      val promise = Promise[T]()
      active(jid) = promise.future
      promise.completeWith(buildStart(jid))
      Some(jid)
    }
  }

  /** Move the entry from `active` to `completed`. Slot freed; `lookupJob`
    * still finds the entry until an explicit `discardJob` removes it.
    */
  def markCompleted(jid: S): Unit = synchronized {
    active.remove(jid).foreach { fut => completed(jid) = fut }
  }

  /** Remove the entry from both maps. Used on explicit cancellation/cleanup. */
  def discardJob(jid: S): Unit = synchronized {
    active -= jid
    completed -= jid
  }

  def lookupJob(jid: S): Option[Future[T]] = synchronized {
    active.get(jid).orElse(completed.get(jid))
  }
}
