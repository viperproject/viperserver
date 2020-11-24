package viper.server.vsi

import akka.actor.{Actor, Props}
import akka.stream.scaladsl.SourceQueueWithComplete
import org.reactivestreams.Publisher

// --- Actor: JobActor ---

object JobActor {
  def props(id: Int): Props = Props(new JobActor(id))
}

class JobActor(private val id: Int) extends Actor {

  private var _verificationTask: Thread = _

  private def interrupt: Boolean = {
    if (_verificationTask != null && _verificationTask.isAlive) {
      _verificationTask.interrupt()
      _verificationTask.join()
      return true
    }
    false
  }

  private def resetVerificationTask(): Unit = {
    if (_verificationTask != null && _verificationTask.isAlive) {
      _verificationTask.interrupt()
      _verificationTask.join()
    }
    _verificationTask = null
  }

  override def receive: PartialFunction[Any, Unit] = {
    case VerificationProtocol.Stop =>
      val did_I_interrupt = interrupt
      if (did_I_interrupt) {
        sender ! s"Job #$id has been successfully interrupted."
      } else {
        sender ! s"Job #$id has already been finalized."
      }
    case VerificationProtocol.Verify(task, queue, publisher) =>
      resetVerificationTask()
      sender ! startJob(task, queue, publisher)
    case msg =>
      throw new Exception("Main Actor: unexpected message received: " + msg)
  }

  private def startJob(task: Thread, queue: SourceQueueWithComplete[Envelope], publisher: Publisher[Envelope]): JobHandle = {
    _verificationTask = task
    _verificationTask.start()
    JobHandle(self, queue, publisher)
  }
}