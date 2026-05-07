// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.atomic.AtomicBoolean

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
  *
  * Pools are constructor-initialised by the implementing class, so an instance
  * is ready as soon as it is constructed. `start()` is a hook for subclass
  * startup work (e.g. binding an HTTP listener); `stop()` flips the running
  * flag idempotently.
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

  protected val ast_jobs: JobPool[AstJobId, AstHandle[Option[AST]]]
  protected val ver_jobs: JobPool[VerJobId, VerHandle]

  private val _running: AtomicBoolean = new AtomicBoolean(true)
  def isRunning: Boolean = _running.get()

  /** Hook for subclasses to perform startup work after construction (e.g. bind
    * an HTTP listener). Default is a no-op. Pools and running state are
    * already established by the constructor.
    */
  def start(): Future[Unit] = Future.unit

  protected def initializeProcess[S <: JobId, T <: JobHandle : ClassTag]
      (pool: JobPool[S, T],
      task_maybe_fut: Future[Option[MessageStreamingTask[_]]],
      discardOnCompletion: Boolean,
      prev_job_id_maybe: Option[AstJobId] = None): Option[S] = {

    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    pool.tryBook((new_jid: S) => task_maybe_fut.flatMap((task_maybe: Option[MessageStreamingTask[_]]) => {
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
          val execution = new JobExecution(task.futureTask)

          task.stream.watchCompletion.onComplete(_ => {
            if (discardOnCompletion) {
              pool.discardJob(new_jid)
            }
          })

          execution.start(executor.executorService)

          val handle: JobHandle = new_jid match {
            case _: AstJobId =>
              AstHandle(execution, task.stream, task.artifact)
            case _: VerJobId =>
              VerHandle(execution, task.stream,
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
    initializeProcess(ast_jobs, Future.successful(Some(task)), false).getOrElse(AstJobId(-1))
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
    initializeProcess(ver_jobs, task_maybe_fut, true, ast_job_id_maybe).getOrElse(VerJobId(-1))
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
    * will result in an IllegalStateException. Idempotent: a second call throws.
    */
  def stop(): Future[List[String]] = {
    if (!_running.compareAndSet(true, false)) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }
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
