// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.FutureTask

import akka.actor.{Actor, Props}


// --- Actor: JobActor ---

object JobActor {
  def props[T](id: JobId): Props = Props(new JobActor[T](id))
}

class JobActor[T](private val id: JobId) extends Actor {

  import VerificationProtocol._

  private var _astConstructionTask: FutureTask[T] = _
  private var _verificationTask: FutureTask[T] = _

  private def interrupt(task: FutureTask[T]): Boolean = {
    if (task != null && !task.isDone) {
      task.cancel(true)
      return true
    }
    false
  }

  private def resetTask(task: FutureTask[T]): Unit = {
    if (task != null && !task.isDone) {
      task.cancel(true)
    }
  }

  private def resetAstConstructionTask(): Unit = {
    resetTask(_astConstructionTask)
    _astConstructionTask = null
  }

  private def resetVerificationTask(): Unit = {
    resetTask(_verificationTask)
    _verificationTask = null
  }

  override def receive: PartialFunction[Any, Unit] = {
    case req: StartProcessRequest[T] =>
      req match {
        case _: ConstructAst[T] =>
          //println(">>> JobActor received request ConstructAst")
          resetAstConstructionTask()
          _astConstructionTask = req.task.futureTask
          req.executor.execute(_astConstructionTask)
          sender() ! AstHandle(self, req.queue, req.publisher, req.task.artifact)

        case ver_req: Verify[T] =>
          //println(">>> JobActor received request Verify")
          resetVerificationTask()
          _verificationTask = ver_req.task.futureTask
          req.executor.execute(_verificationTask)
          sender() ! VerHandle(self, ver_req.queue, ver_req.publisher, ver_req.prev_job_id)
      }
    case req: StopProcessRequest =>
      val did_I_interrupt = req match {
        case StopAstConstruction =>
          interrupt(_astConstructionTask)
        case StopVerification =>
          interrupt(_verificationTask)
      }
      if (did_I_interrupt) {
        sender() ! s"$id has been successfully interrupted."
      } else {
        // FIXME: Saying this is a potential vulnerability
        sender() ! s"$id has already been finalized."
      }
    case msg =>
      throw new Exception("JobActor: received unexpected message: " + msg)
  }
}
