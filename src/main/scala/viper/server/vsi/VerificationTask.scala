package viper.server.vsi

import akka.actor.ActorRef
import akka.pattern.ask
import akka.stream.QueueOfferResult
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._



/** This class is a generic wrapper for a any sort of verification a VerificationServer might
  * work on.
  *
  * It has the following properties:
  *  - implements runnable
  *  - provides a reference to a queue actor.
  *
  *  The first serves the purpose of running the process concurrently. The second allows to
  *  communicate from the verification process to the server.
  * */
abstract class VerificationTask()(implicit val executionContext: ExecutionContext) extends Runnable with Packer {

  private var q_actor: ActorRef = _

  final def setQueueActor(actor: ActorRef): Unit = {
    q_actor = actor
  }

  /** Sends massage to the attached actor.
    *
    * The actor receiving this message offers it to a queue. This offering returns a Future,
    * which  will eventually indicate whether or not the offer was successful. This method is
    * blocking, as it waits for the successful completion of such an offer.
    * */
  protected def enqueueMessages(msg: A): Unit = {
    implicit val askTimeout: Timeout = Timeout(5000 milliseconds)

    var current_offer: Future[QueueOfferResult] = null
    val answer = q_actor ? TaskProtocol.BackendReport(pack(msg))
    current_offer = answer.flatMap({
      case res: Future[QueueOfferResult] => res
    })
    while(current_offer == null || !current_offer.isCompleted){
      Thread.sleep(10)
    }
  }

  /** Notify the queue actor that the task has come to an end
    *
    * The actor receiving this message will close the queue.
    *
    * @param success indicates whether or not the task has ended as successfully.
    * */
  protected def registerTaskEnd(success: Boolean): Unit = {
    q_actor ! TaskProtocol.FinalBackendReport(success)
  }
}