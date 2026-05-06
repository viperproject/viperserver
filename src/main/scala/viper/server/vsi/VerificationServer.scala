// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import viper.server.core.VerificationExecutionContext

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}


abstract class VerificationServerException extends Exception
case object JobNotFoundException extends VerificationServerException
abstract class AstConstructionException extends VerificationServerException

/** State and process management for verification servers.
  *
  * Each booked job owns a `JobExecution` (cancellable Future-task wrapper) and an
  * `EnvelopeStream` over which the backend streams messages back to consumers.
  * Two pools (AST construction, verification) are managed in parallel.
  */
trait VerificationServer extends Post {

  type AST

  implicit val executor: VerificationExecutionContext

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
    * Calling any other function before this one will result in an
    * IllegalStateException. The returned future resolves when the server has
    * been started. A default implementation is provided in
    * `DefaultVerificationServerStart`.
    */
  def start(active_jobs: Int): Future[Unit]

  protected def initializeProcess[S <: JobId, T <: JobHandle : ClassTag]
      (pool: JobPool[S, T],
      task_maybe_fut: Future[Option[MessageStreamingTask[_]]],
      discardOnCompletion: Boolean,
      prev_job_id_maybe: Option[AstJobId] = None): S = {

    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    require(pool.newJobsAllowed)

    pool.bookNewJob((new_jid: S) => task_maybe_fut.flatMap((task_maybe: Option[MessageStreamingTask[_]]) => {
      task_maybe match {
        case None =>
          /** No task means a prerequisite produced no usable artifact; the placeholder
            * just holds the predecessor's id and is removed from the pool.
            */
          pool.discardJob(new_jid)
          new_jid match {
            case _: VerJobId =>
              Future.successful(VerHandle(null, null, prev_job_id_maybe))
          }
        case Some(task) =>
          val stream = new EnvelopeStream()
          task.setStream(stream)

          val execution = new JobExecution(task.futureTask)

          stream.watchCompletion.onComplete(_ => {
            if (discardOnCompletion) {
              pool.discardJob(new_jid)
            }
          })

          execution.start(executor.executorService)

          val handle: JobHandle = new_jid match {
            case _: AstJobId =>
              AstHandle(execution, stream, task.artifact)
            case _: VerJobId =>
              VerHandle(execution, stream,
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
        // If the AST construction phase failed, drop the verification job handle.
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

  /** Starts a verification process. */
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

  /** Combined envelope iterator for a verification job, optionally prepending
    * the AST job's messages. Caller iterates synchronously to drain.
    */
  protected def messageEnvelopes(jid: VerJobId, include_ast: Boolean): Option[Future[(VerHandle, Iterator[Envelope])]] = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    ver_jobs.lookupJob(jid).map(handle_future =>
      handle_future.flatMap((ver_handle: VerHandle) => {
        ver_handle.prev_job_id match {
          case _ if !include_ast =>
            Future.successful((None: Option[AstHandle[Option[AST]]], ver_handle))
          case None =>
            Future.successful((None: Option[AstHandle[Option[AST]]], ver_handle))
          case Some(ast_id) =>
            ast_jobs.lookupJob(ast_id) match {
              case Some(ast_handle_fut) =>
                ast_handle_fut.map(ast_handle => (Some(ast_handle): Option[AstHandle[Option[AST]]], ver_handle))
              case None =>
                Future.successful((None: Option[AstHandle[Option[AST]]], ver_handle))
            }
        }
      }).map {
        case (ast_handle_maybe, ver_handle) =>
          val ver_iter: Iterator[Envelope] = ver_handle match {
            case VerHandle(null, null, _) => Iterator.empty
            case _ => ver_handle.stream.iterator
          }
          val combined: Iterator[Envelope] = ast_handle_maybe match {
            case None => ver_iter
            case Some(ast_handle) => ast_handle.stream.iterator ++ ver_iter
          }
          (ver_handle, combined)
      })
  }

  /** Envelope iterator for an AST job. */
  protected def messageEnvelopes(jid: AstJobId): Option[Future[(AstHandle[Option[AST]], Iterator[Envelope])]] = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    ast_jobs.lookupJob(jid).map(_.map(ast_handle =>
      (ast_handle, ast_handle.stream.iterator)))
  }

  /** Collect all messages from a verification job (including AST messages) into a list. */
  def collectMessages(jid: VerJobId): Option[Future[List[A]]] = {
    messageEnvelopes(jid, include_ast = true).map(_.flatMap { case (_, iter) =>
      Future {
        iter.map(e => unpack(e)).toList
      }(executor)
    })
  }

  /** Stops an instance of VerificationServer from running.
    *
    * Should be the last method called. Calling any other function after `stop`
    * will result in an IllegalStateException.
    */
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

  /** Interrupts all active jobs (used during server shutdown). */
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
  override def start(active_jobs: Int): Future[Unit] = {
    ast_jobs = new JobPool("VSI-AST-pool", active_jobs)
    ver_jobs = new JobPool("VSI-Verification-pool", active_jobs)
    isRunning = true
    Future.unit
  }
}
