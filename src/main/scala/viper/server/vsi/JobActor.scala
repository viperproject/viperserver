package viper.server.vsi

import akka.actor.{Actor, Props}

// --- Actor: JobActor ---

object JobActor {
  def props(id: JobId): Props = Props(new JobActor(id))
}

class JobActor(private val id: JobId) extends Actor {

  import VerificationProtocol._

  private var _astConstructionTask: Thread = _
  private var _verificationTask: Thread = _

  private def interrupt(task: Thread): Boolean = {
    if (task != null && task.isAlive) {
      task.interrupt()
      task.join()
      return true
    }
    false
  }

  private def resetTask(task: Thread): Unit = {
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
    case req: StartProcessRequest =>
      req match {
        case _: ConstructAst =>
          println(">>> JobActor received request ConstructAst")
          resetAstConstructionTask()
          _astConstructionTask = req.task
          _astConstructionTask.start()
          sender ! AstHandle(self, req.queue, req.publisher)
        case _: Verify =>
          println(">>> JobActor received request Verify")
          resetVerificationTask()
          _verificationTask = req.task
          _verificationTask.start()
          sender ! VerHandle(self, req.queue, req.publisher)
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