// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.actor.{Actor, Props}


// --- Actor: JobActor ---

object JobActor {
  def props[T](id: JobId): Props = Props(new JobActor[T](id))

  private def interruptedReply(id: JobId): String = s"$id has been successfully interrupted."
  private def finalizedReply(id: JobId): String = s"$id has already been finalized."

  /** whether a reply to a [[VerificationProtocol.StopProcessRequest]] indicates that an active
    * task has been interrupted (as opposed to there being nothing left to interrupt) */
  def indicatesInterrupted(reply: String): Boolean = reply.endsWith("interrupted.")
}

class JobActor[T](private val id: JobId) extends Actor {

  import VerificationProtocol._

  private var _astConstructionRequest: StartProcessRequest[T] = _
  private var _verificationRequest: StartProcessRequest[T] = _

  private def interrupt(req: StartProcessRequest[T]): Boolean = {
    if (req != null && !req.task.futureTask.isDone) {
      req.task.futureTask.cancel(true)
      completeQueueIfTaskNeverRan(req)
      return true
    }
    false
  }

  private def resetTask(req: StartProcessRequest[T]): Unit = {
    if (req != null && !req.task.futureTask.isDone) {
      req.task.futureTask.cancel(true)
      completeQueueIfTaskNeverRan(req)
    }
  }

  /** The message queue is normally completed by the running task (via `registerTaskEnd`). A task
    * that is cancelled before it ever started will never do so, which would leave the queue --
    * and thereby the job's message stream and its completion-triggered cleanup -- pending forever.
    * Complete the queue on the task's behalf in that case; `cancelledBeforeStart` guarantees that
    * the task can no longer start, i.e. that nobody else will complete the queue.
    */
  private def completeQueueIfTaskNeverRan(req: StartProcessRequest[T]): Unit = {
    if (req.task.cancelledBeforeStart) {
      // completing via the queue actor (mirroring `registerTaskEnd`) also stops that actor;
      // completing the queue directly would leak it. The queue actor is unset only for tasks
      // constructed outside `initializeProcess` (e.g. in unit tests):
      val queueActor = req.task.queueActor
      if (queueActor != null) {
        queueActor ! TaskProtocol.FinalBackendReport(success = false)
      } else {
        req.queue.complete()
      }
    }
  }

  private def resetAstConstructionTask(): Unit = {
    resetTask(_astConstructionRequest)
    _astConstructionRequest = null
  }

  private def resetVerificationTask(): Unit = {
    resetTask(_verificationRequest)
    _verificationRequest = null
  }

  override def receive: PartialFunction[Any, Unit] = {
    // The type argument cannot be checked at runtime because of erasure. This is safe because this
    // actor only ever receives requests parameterized with its own type argument.
    case req: StartProcessRequest[T @unchecked] =>
      req match {
        case _: ConstructAst[T] =>
          //println(">>> JobActor received request ConstructAst")
          resetAstConstructionTask()
          _astConstructionRequest = req
          req.executor.execute(req.task.futureTask)
          sender() ! AstHandle(self, req.queue, req.publisher, req.task.artifact)

        case ver_req: Verify[T] =>
          //println(">>> JobActor received request Verify")
          resetVerificationTask()
          _verificationRequest = ver_req
          req.executor.execute(ver_req.task.futureTask)
          sender() ! VerHandle(self, ver_req.queue, ver_req.publisher, ver_req.prev_job_id)
      }
    case req: StopProcessRequest =>
      val did_I_interrupt = req match {
        case StopAstConstruction =>
          interrupt(_astConstructionRequest)
        case StopVerification =>
          interrupt(_verificationRequest)
      }
      if (did_I_interrupt) {
        sender() ! JobActor.interruptedReply(id)
      } else {
        // FIXME: Saying this is a potential vulnerability
        sender() ! JobActor.finalizedReply(id)
      }
    case msg =>
      throw new Exception("JobActor: received unexpected message: " + msg)
  }
}
