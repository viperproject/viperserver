package viper.server.vsi

import akka.Done
import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

// --- Actor: Terminator ---


object Terminator {
  case object Exit
  case class WatchJobQueue(jid: VerJobId, handle: VerHandle)

  def props(jobs: JobPool[VerJobId, VerHandle],
            bindingFuture: Option[Future[Http.ServerBinding]] = None)
            (implicit ctx: ExecutionContext, sys: ActorSystem): Props = Props(new Terminator(jobs, bindingFuture)(ctx, sys))
}

class Terminator(jobs: JobPool[VerJobId, VerHandle],
                 bindingFuture: Option[Future[Http.ServerBinding]])
                (implicit val ctx: ExecutionContext,
                 implicit val sys: ActorSystem) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case Terminator.Exit =>
      bindingFuture match {
        case Some(future) =>
          future
            .flatMap(_.unbind()) // trigger unbinding from the port
            .onComplete(_ => {
              sys.terminate() // and shutdown when done
            })
        case None =>
          sys.terminate() // shutdown
      }
    case Terminator.WatchJobQueue(jid, handle) =>
      val queue_completion_future: Future[Done] = handle.queue.watchCompletion()
      queue_completion_future.onComplete( {
        case Failure(e) =>
          throw e
        case Success(_) =>
          jobs.discardJob(jid)
      })
  }
}