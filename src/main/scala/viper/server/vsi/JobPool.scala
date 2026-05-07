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
  * compound-action-safe (capacity check + insert is one atomic step).
  *
  * `tryBook` registers the entry *before* invoking the start callback so that
  * synchronous `discardJob` calls inside the callback (used to free the slot
  * for placeholder/no-op jobs) find a live entry to remove.
  */
class JobPool[S <: JobId, T <: JobHandle](val tag: String, val MAX_ACTIVE_JOBS: Int = 3)
                                         (implicit val jid_fact: Int => S) {

  private val jobs: mutable.Map[S, Future[T]] = mutable.Map()
  private val _nextJobId: AtomicInteger = new AtomicInteger(0)

  def jobHandles: Map[S, Future[T]] = synchronized { jobs.toMap }

  /** Atomically: check capacity, allocate a fresh id, register a placeholder
    * Future, then invoke `buildStart` to obtain the real Future and wire it
    * into the placeholder. Returns `None` if at capacity.
    *
    * `synchronized` is reentrant, so `buildStart` may call `discardJob` on
    * this pool synchronously without deadlock.
    */
  def tryBook(buildStart: S => Future[T]): Option[S] = synchronized {
    if (jobs.size >= MAX_ACTIVE_JOBS) None
    else {
      val jid: S = jid_fact(_nextJobId.getAndIncrement())
      val promise = Promise[T]()
      jobs(jid) = promise.future
      promise.completeWith(buildStart(jid))
      Some(jid)
    }
  }

  def discardJob(jid: S): Unit = synchronized {
    jobs -= jid
  }

  def lookupJob(jid: S): Option[Future[T]] = synchronized {
    jobs.get(jid)
  }
}
