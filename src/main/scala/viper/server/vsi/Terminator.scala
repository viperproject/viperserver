package viper.server.vsi

import akka.Done
import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

// --- Actor: Terminator ---


object Terminator {
  case object Exit
  case class WatchJobQueue(jid: JobId, handle: JobHandle)

  def props[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
               ver_jobs: JobPool[VerJobId, VerHandle],
               bindingFuture: Option[Future[Http.ServerBinding]] = None)
            (implicit ctx: ExecutionContext, sys: ActorSystem): Props
  = Props(new Terminator(ast_jobs, ver_jobs, bindingFuture)(ctx, sys))
}

class Terminator[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
                    ver_jobs: JobPool[VerJobId, VerHandle],
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
      queue_completion_future.onComplete {
        case Failure(e) =>
          throw e
        case Success(_) =>
          jid match {
            case ast_id: AstJobId =>
              ast_jobs.discardJob(ast_id)
            case ver_id: VerJobId =>
              ver_jobs.discardJob(ver_id)
          }
      }
  }
}