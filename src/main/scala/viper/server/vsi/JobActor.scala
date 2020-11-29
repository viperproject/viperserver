package viper.server.vsi

import akka.actor.{Actor, Props}

import scala.concurrent.Future


class TaskThread[T](private val _task: MessageStreamingTask[T]) extends Thread(_task) {
  def getArtifact: Future[T] = _task.artifact
}

// --- Actor: JobActor ---

object JobActor {
  def props[T](id: JobId): Props = Props(new JobActor[T](id))
}

class JobActor[T](private val id: JobId) extends Actor {

  import VerificationProtocol._



  private var _astConstructionTask: TaskThread[T] = _
  private var _verificationTask: TaskThread[T] = _

  private def interrupt(task: TaskThread[T]): Boolean = {
    if (task != null && task.isAlive) {
      task.interrupt()
      task.join()
      return true
    }
    false
  }

  private def resetTask(task: TaskThread[T]): Unit = {
    if (task != null && task.isAlive) {
      task.interrupt()
      task.join()
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
          println(">>> JobActor received request ConstructAst")
          resetAstConstructionTask()
          _astConstructionTask = req.task
          _astConstructionTask.start()
          sender ! AstHandle(self, req.queue, req.publisher, _astConstructionTask.getArtifact)

        case ver_req: Verify[T] =>
          println(">>> JobActor received request Verify")
          resetVerificationTask()
          _verificationTask = ver_req.task
          _verificationTask.start()
          sender ! VerHandle(self, ver_req.queue, ver_req.publisher, ver_req.prev_job_id)
      }
    case req: StopProcessRequest =>
      val did_I_interrupt = req match {
        case StopAstConstruction =>
          interrupt(_astConstructionTask)
        case StopVerification =>
          interrupt(_verificationTask)
      }
      if (did_I_interrupt) {
        sender ! s"$id has been successfully interrupted."
      } else {
        // FIXME: Saying this is a potential vulnerability
        sender ! s"$id has already been finalized."
      }
    case msg =>
      throw new Exception("JobActor: received unexpected message: " + msg)
  }
}