// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.{Done, NotUsed}
import akka.actor.{ActorRef, ActorSystem, Status}
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.OverflowStrategy
import akka.util.Timeout
import viper.server.core.VerificationExecutionContext

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}


abstract class VerificationServerException extends Exception
case object JobNotFoundException extends VerificationServerException
abstract class AstConstructionException extends VerificationServerException

/** This trait provides state and process management functionality for verification servers.
  *
  * The server runs on Akka's actor system. This means that the entire server's state
  * and process management are run by actors. The 3 actors in charge are:
  *
  *   1) Job Actor
  *   2) Queue Actor
  *   3) Terminator Actor
  *
  *  The first two actors manage individual verification processes. I.e., on
  *  initializeVerificationProcess() and instance of each actor is created. The JobActor launches
  *  the actual VerificationTask, while the QueueActor acts as a middleman for communication
  *  between a VerificationTask's backend and the server. The Terminator Actor is in charge of
  *  terminating both individual processes and the server.
  */
trait VerificationServer extends Post {

  type AST

  implicit val executor: VerificationExecutionContext
  implicit val system: ActorSystem = executor.actorSystem
  implicit def askTimeout: Timeout

  /** Hook invoked from `stop()` after all jobs have been interrupted. Default
    * is a no-op. The HTTP frontend overrides this to unbind its server port.
    */
  protected def onExit(): Future[Unit] = Future.unit

  implicit val ast_id_fact: Int => AstJobId = AstJobId.apply
  implicit val ver_id_fact: Int => VerJobId = VerJobId.apply

  protected var ast_jobs: JobPool[AstJobId, AstHandle[Option[AST]]] = _
  protected var ver_jobs: JobPool[VerJobId, VerHandle] = _

  var isRunning: Boolean = false

  /** Configures an instance of VerificationServer.
    *
    * This function must be called before any other. Calling any other function before this one
    * will result in an IllegalStateException.
    * The returned future resolves when the server has been started.
    *
    * Note that a default implementation is provided in DefaultVerificationServerStart
    */
  def start(active_jobs: Int): Future[Done]

  protected def initializeProcess[S <: JobId, T <: JobHandle : ClassTag]
      (pool: JobPool[S, T],
      task_maybe_fut: Future[Option[MessageStreamingTask[_]]],
      discardOnCompletion: Boolean,
      prev_job_id_maybe: Option[AstJobId] = None): S = {

    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    require(pool.newJobsAllowed)

    /** Ask the pool to book a new job using the above function
      * to construct Future[JobHandle] and Promise[AST] later on. */
    pool.bookNewJob((new_jid: S) => task_maybe_fut.flatMap((task_maybe: Option[MessageStreamingTask[_]]) => {
      task_maybe match {
        case None =>
          /** If there's no task, that means their prerequisite tasks haven't produced usable artifacts.
            * The sole purpose of this task is hence to hold the identifier of its predecessor.
            * We should remove this task from the job pool. */
          pool.discardJob(new_jid)
          new_jid match {
            case _: VerJobId =>
              Future.successful(VerHandle(null, null, null, prev_job_id_maybe))
          }
        case Some(task) =>
          val (queue, publisher) = Source.queue[Envelope](10000, OverflowStrategy.backpressure)
            .toMat(Sink.asPublisher(false))(Keep.both).run()

          task.setQueue(queue)

          val execution = new JobExecution(task.futureTask)

          /** Register cleanup task. */
          queue.watchCompletion().onComplete(_ => {
            if (discardOnCompletion) {
              pool.discardJob(new_jid)
            }
          })

          execution.start(executor.executorService)

          val handle: JobHandle = new_jid match {
            case _: AstJobId =>
              AstHandle(execution, queue, publisher, task.artifact)
            case _: VerJobId =>
              VerHandle(execution, queue, publisher,
                prev_job_id_maybe match {
                  case Some(prev_job_id: AstJobId) =>
                    Some(prev_job_id)
                  case Some(prev_job_id) =>
                    throw new IllegalArgumentException(s"cannot map ${prev_job_id.toString} to expected type AstJobId")
                  case None =>
                    None
                })
          }
          Future.successful(handle.asInstanceOf[T])
      }

    }).recover({
      case e: AstConstructionException =>
        // If the AST construction phase failed, remove the verification job handle
        // from the corresponding pool.
        val msg = s"AST construction job ${prev_job_id_maybe.get} resulted in a failure: $e"
        println(msg)
        pool.discardJob(new_jid)
    }).mapTo[T])
  }

  protected def initializeAstConstruction(task: MessageStreamingTask[Option[AST]]): AstJobId = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    if (ast_jobs.newJobsAllowed) {
      initializeProcess(ast_jobs, Future.successful(Some(task)), false)
    } else {
      AstJobId(-1) // Process Management running  at max capacity.
    }
  }

  protected def discardAstJob(jid: AstJobId): Unit = {
    ast_jobs.discardJob(jid)
  }

  /** This method starts a verification process.
    *
    * As such, it accepts an instance of a VerificationTask, which it will pass to the JobActor.
    */
  protected def initializeVerificationProcess(task_maybe_fut: Future[Option[MessageStreamingTask[Unit]]],
                                              ast_job_id_maybe: Option[AstJobId]): VerJobId = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    if (ver_jobs.newJobsAllowed) {
      initializeProcess(ver_jobs, task_maybe_fut, true, ast_job_id_maybe)
    } else {
      VerJobId(-1)  // Process Management running  at max capacity.
    }
  }

  /** Build the combined Envelope source for a verification job (optionally
    * prepending the AST job's messages). Callers attach a sink to consume.
    */
  protected def messageEnvelopeSource(jid: VerJobId, include_ast: Boolean): Option[Future[(VerHandle, Source[Envelope, NotUsed])]] = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    ver_jobs.lookupJob(jid).map(handle_future =>
      handle_future.flatMap((ver_handle: VerHandle) => {
        ver_handle.prev_job_id match {
          case _ if !include_ast =>
            Future.successful((None, ver_handle))
          case None =>
            Future.successful((None, ver_handle))
          case Some(ast_id) =>
            ast_jobs.lookupJob(ast_id) match {
              case Some(ast_handle_fut) =>
                ast_handle_fut.map(ast_handle => (Some(ast_handle), ver_handle))
              case None =>
                Future.successful((None, ver_handle))
            }
        }
      }).map {
        case (ast_handle_maybe: Option[AstHandle[Option[AST]]], ver_handle: VerHandle) =>
          val ver_source = ver_handle match {
            case VerHandle(null, null, null, _) =>
              Source.empty[Envelope]
            case _ =>
              Source.fromPublisher(ver_handle.publisher)
          }
          val combined_source = ast_handle_maybe match {
            case None => ver_source
            case Some(ast_handle) => ver_source.prepend(Source.fromPublisher(ast_handle.publisher))
          }
          (ver_handle, combined_source)
      })
  }

  /** Build the Envelope source for an AST job. */
  protected def messageEnvelopeSource(jid: AstJobId): Option[Future[(AstHandle[Option[AST]], Source[Envelope, NotUsed])]] = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    ast_jobs.lookupJob(jid).map(_.map(ast_handle =>
      (ast_handle, Source.fromPublisher(ast_handle.publisher))))
  }

  /** Stream all messages generated by the Verification backend to some actor.
    * If `include_ast` is set, this will also stream the AST messages.
    *
    * Deletes the JobHandle on completion.
    */
  protected def streamMessages(jid: VerJobId, clientActor: ActorRef, include_ast: Boolean): Option[Future[Done]] = {
    messageEnvelopeSource(jid, include_ast).map(_.flatMap { case (ver_handle, combined_source) =>
      val sink = Sink.actorRef(clientActor, Status.Success, Status.Failure)
      combined_source.map(e => unpack(e)).runWith(sink)
      // FIXME This assumes that someone will actually complete the verification job queue.
      // FIXME Could we guarantee that the client won't forget to do this?
      ver_handle.queue.watchCompletion()
    })
  }

  /** Collect all messages generated by a verification job (including AST
    * messages) into a list. Returns None if the job is unknown.
    */
  def collectMessages(jid: VerJobId): Option[Future[List[A]]] = {
    messageEnvelopeSource(jid, include_ast = true).map(_.flatMap { case (_, combined_source) =>
      combined_source.map(e => unpack(e)).runFold(List.empty[A])(_ :+ _)
    })
  }

  /** Stream all messages generated by the AST backend to some actor.
    *
    * Deletes the JobHandle on completion.
    */
  protected def streamMessages(jid: AstJobId, clientActor: ActorRef): Option[Future[Done]] = {
    messageEnvelopeSource(jid).map(_.flatMap { case (ast_handle, ast_source) =>
      ast_source.map(e => unpack(e)).runWith(Sink.actorRef(clientActor, Status.Success, Status.Failure))
      ast_handle.queue.watchCompletion()
    })
  }

  /** Stops an instance of VerificationServer from running.
    * The actor system and executor do not get terminated and are the responsibility of the caller
    *
    * As such it should be the last method called. Calling any other function after stop will
    * result in an IllegalStateException.
    * */
  def stop(): Future[List[String]] = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }
    isRunning = false
    getInterruptFutureList().transform(r => {
      onExit()
      r match {
        case Success(_) => println(s"shutting down...")
        case Failure(_) => println(s"forcibly shutting down...")
      }
      r
    })
  }

  /** This method interrupts active jobs upon termination of the server.
    */
  protected def getInterruptFutureList(): Future[List[String]] = {
    val handles = ver_jobs.jobHandles ++ ast_jobs.jobHandles
    val interrupt_future_list: List[Future[String]] = handles.map {
      case (jid, handle_future) =>
        handle_future.map(handle => formatInterruptResult(jid, handle.execution.cancel()))
    }.toList
    Future.sequence(interrupt_future_list)
  }

  protected def formatInterruptResult(jid: JobId, interrupted: Boolean): String = {
    if (interrupted) s"$jid has been successfully interrupted."
    else s"$jid has already been finalized."
  }
}

trait DefaultVerificationServerStart extends VerificationServer {
  override def start(active_jobs: Int): Future[Done] = {
    ast_jobs = new JobPool("VSI-AST-pool", active_jobs)
    ver_jobs = new JobPool("VSI-Verification-pool", active_jobs)
    isRunning = true
    Future.successful(Done)
  }
}
