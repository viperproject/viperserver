// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.Done
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Status}
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.OverflowStrategy
import akka.util.Timeout
import viper.server.core.VerificationExecutionContext

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scala.language.postfixOps


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

  protected var _termActor: ActorRef = _

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
          /** What we really want here is SourceQueueWithComplete[Envelope]
            * Publisher[Envelope] might be needed to create a stream later on,
            * but the publisher and the queue are synchronized are should be viewed
            * as different representation of the same concept.
            */
          val (queue, publisher) = Source.queue[Envelope](10000, OverflowStrategy.backpressure)
            .toMat(Sink.asPublisher(false))(Keep.both).run()

          /** This actor will be responsible for managing ONE queue,
            * whereas the JobActor can manage multiple tasks, all of which are related to some pipeline,
            * e.g.   [Text] ---> [AST] ---> [VerificationResult]
            *        '--- Task I ----'                         |
            *                    '---------- Task II ----------'
            **/
          val message_actor = system.actorOf(QueueActor.props(queue), s"${pool.tag}--message_actor--${new_jid.id}")
          task.setQueueActor(message_actor)

          val job_actor = system.actorOf(JobActor.props(new_jid), s"${pool.tag}_job_actor_${new_jid}")

          /** Register cleanup task. */
          queue.watchCompletion().onComplete(_ => {
            pool.discardJob(new_jid)
            /** FIXME: if the job actors are meant to be reused from one phase to another (not implemented yet),
              * FIXME: then they should be stopped only after the **last** job is completed in the pipeline. */
            job_actor ! PoisonPill
          })

          (job_actor ? (new_jid match {
            case _: AstJobId =>
              VerificationProtocol.ConstructAst(task, queue, publisher, executor)
            case _: VerJobId =>
              VerificationProtocol.Verify(task, queue, publisher,
                /** TODO: Use factories for specializing the messages.
                  * TODO: Clearly, there should be a clean separation between concrete job types
                  * TODO: (AST Construction, Verification) and generic types (JobHandle). */
                prev_job_id_maybe match {
                  case Some(prev_job_id: AstJobId) =>
                    Some(prev_job_id)
                  case Some(prev_job_id) =>
                    throw new IllegalArgumentException(s"cannot map ${prev_job_id.toString} to expected type AstJobId")
                  case None =>
                    None
                }, executor)
          })).mapTo[T]
      }
      /** TODO avoid hardcoded parameters */

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
      initializeProcess(ast_jobs, Future.successful(Some(task)))
    } else {
      AstJobId(-1) // Process Management running  at max capacity.
    }
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
      initializeProcess(ver_jobs, task_maybe_fut, ast_job_id_maybe)
    } else {
      VerJobId(-1)  // Process Management running  at max capacity.
    }
  }

  /** Stream all messages generated by the backend to some actor.
    *
    * Deletes the JobHandle on completion.
    */
  protected def streamMessages(jid: VerJobId, clientActor: ActorRef): Option[Future[Done]] = {
    if (!isRunning) {
      throw new IllegalStateException("Instance of VerificationServer already stopped")
    }

    ver_jobs.lookupJob(jid) match {
      case None =>
        /** Verification job not found */
        None
      case Some(handle_future) =>
        Some(handle_future.flatMap((ver_handle: VerHandle) => {
          ver_handle.prev_job_id match {
            case None =>
              /** The AST for this verification job wasn't created by this server. */
              Future.successful((None, ver_handle))
            case Some(ast_id) =>
              /** The AST construction job may have been cleaned up
                * (if all of its messages were already consumed) */
              ast_jobs.lookupJob(ast_id) match {
                case Some(ast_handle_fut) =>
                  ast_handle_fut.map(ast_handle => (Some(ast_handle), ver_handle))
                case None =>
                  Future.successful((None, ver_handle))
              }
          }
        }) flatMap {
          case (ast_handle_maybe: Option[AstHandle[Option[AST]]], ver_handle: VerHandle) =>
            val ver_source = ver_handle match {
              case VerHandle(null, null, null, _) =>
                /** There were no messages produced during verification. */
                Source.empty[Envelope]
              case _ =>
                Source.fromPublisher(ver_handle.publisher)
            }
            val ast_source = ast_handle_maybe match {
              case None =>
                /** The AST messages were already consumed. */
                Source.empty[Envelope]
              case Some(ast_handle) =>
                Source.fromPublisher(ast_handle.publisher)
            }
            val resulting_source = ver_source.prepend(ast_source).map(e => unpack(e))
            resulting_source.runWith(Sink.actorRef(clientActor, Status.Success, Status.Failure))

            // FIXME This assumes that someone will actually complete the verification job queue.
            // FIXME Could we guarantee that the client won't forget to do this?
            ver_handle.queue.watchCompletion()
        })
    }
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
      _termActor ! Terminator.Exit
      r match {
        case Success(_) => println(s"shutting down...")
        case Failure(_) => println(s"forcibly shutting down...")
      }
      // delete termActor since we no longer need it. Otherwise, start() cannot be called
      _termActor = null
      r
    })
  }

  /** This method interrupts active jobs upon termination of the server.
    */
  protected def getInterruptFutureList(): Future[List[String]] = {
    implicit val askTimeout: Timeout = Timeout(1000 milliseconds)
    val handles = ver_jobs.jobHandles ++ ast_jobs.jobHandles
    val interrupt_future_list: List[Future[String]] = handles map {
      case (_, handle_future) =>
        handle_future.flatMap {
          case AstHandle(actor, _, _, _) =>
            (actor ? VerificationProtocol.StopAstConstruction).mapTo[String]
          case VerHandle(actor, _, _, _) =>
            (actor ? VerificationProtocol.StopVerification).mapTo[String]
        }
      } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }
}

trait DefaultVerificationServerStart extends VerificationServer {
  override def start(active_jobs: Int): Future[Done] = {
    ast_jobs = new JobPool("VSI-AST-pool", active_jobs)
    ver_jobs = new JobPool("VSI-Verification-pool", active_jobs)
    _termActor = system.actorOf(Terminator.props(ast_jobs, ver_jobs), "terminator")
    isRunning = true
    Future.successful(Done)
  }
}
