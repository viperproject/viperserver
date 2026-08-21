// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.vsi

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.util.Timeout
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import viper.server.core.{DefaultVerificationExecutionContext, VerificationExecutionContext}

import java.util.concurrent.{CountDownLatch, ExecutorService, TimeUnit, Future => JFuture}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.control.NonFatal

/** a minimal `MessageStreamingTask` recording whether its callable ever ran.
  */
class ProbeTask extends MessageStreamingTask[Unit] {
  type A = String
  override def pack(m: String): Envelope = ProbeEnvelope(m)
  override def unpack(e: Envelope): String = e.asInstanceOf[ProbeEnvelope].s

  @volatile var ran: Boolean = false
  /** released once the callable has started; used by tests that cancel a running task */
  val startedLatch = new CountDownLatch(1)
  /** the callable blocks on this latch until it is interrupted or released */
  val blockLatch = new CountDownLatch(1)
  /** whether the callable should block on `blockLatch` after starting */
  @volatile var blockUntilInterrupted: Boolean = false

  override def call(): Unit = {
    ran = true
    startedLatch.countDown()
    if (blockUntilInterrupted) {
      blockLatch.await()
    }
  }
}
case class ProbeEnvelope(s: String) extends Envelope

class TaskInterruptionTests extends AnyWordSpec with Matchers {

  "MessageStreamingTask" should {

    "report cancelledBeforeStart for a task that is cancelled before it executes" in {
      val task = new ProbeTask
      assert(!task.cancelledBeforeStart)
      task.futureTask.cancel(true)
      assert(task.cancelledBeforeStart)
      // executing the cancelled task must not run its callable:
      task.futureTask.run()
      assert(!task.ran)
      // the answer is stable:
      assert(task.cancelledBeforeStart)
    }

    "not report cancelledBeforeStart for a completed task" in {
      val task = new ProbeTask
      task.futureTask.run()
      assert(task.ran)
      task.futureTask.cancel(true) // has no effect on a completed task
      assert(!task.cancelledBeforeStart)
    }

    "not report cancelledBeforeStart for a task that is cancelled while it is running" in {
      val task = new ProbeTask
      task.blockUntilInterrupted = true
      val thread = new Thread(task.futureTask)
      thread.start()
      assert(task.startedLatch.await(5, TimeUnit.SECONDS))
      task.futureTask.cancel(true)
      assert(!task.cancelledBeforeStart)
      task.blockLatch.countDown() // in case the interrupt was not observed
      thread.join(5000)
      assert(!thread.isAlive)
      assert(task.ran)
      assert(!task.cancelledBeforeStart)
    }
  }

  "JobActor" should {

    "complete the message queue when stopping a task that never started" in {
      val context = new DefaultVerificationExecutionContext(actorSystemName = "TaskInterruptionTests")
      try {
        implicit val system: ActorSystem = context.actorSystem
        implicit val askTimeout: Timeout = Timeout(5 seconds)

        /** delegates to `context` but drops submitted tasks, simulating a saturated thread pool
          * whose queued task is stopped before any thread ever picks it up */
        val droppingExecutor: VerificationExecutionContext = new VerificationExecutionContext {
          override def execute(runnable: Runnable): Unit = { /* drop */ }
          override def reportFailure(cause: Throwable): Unit = context.reportFailure(cause)
          override def executorService: ExecutorService = context.executorService
          override def actorSystem: ActorSystem = context.actorSystem
          override def submit(r: Runnable): JFuture[_] = context.submit(r)
          override def terminate(timeoutMSec: Long): Unit = ()
          override def restart(): Future[Unit] = Future.successful(())
        }

        val (queue, publisher) = Source.queue[Envelope](10, OverflowStrategy.backpressure)
          .toMat(Sink.asPublisher(false))(Keep.both).run()
        val task = new ProbeTask
        // mirror `initializeProcess`, which attaches a QueueActor to every task -- the JobActor
        // completes the queue of a never-started task through this actor:
        task.setQueueActor(system.actorOf(QueueActor.props(queue)))
        val jobActor = system.actorOf(JobActor.props[Unit](VerJobId(0)))

        val handle = Await.result(
          (jobActor ? VerificationProtocol.Verify[Unit](task, queue, publisher, None, droppingExecutor)).mapTo[VerHandle],
          5 seconds)
        assert(handle.job_actor == jobActor)

        val reply = Await.result(
          (jobActor ? VerificationProtocol.StopVerification).mapTo[VerificationProtocol.StopProcessReply],
          5 seconds)
        assert(reply.interrupted)

        // without the JobActor completing the queue on behalf of the never-started task, this
        // would hang forever (nobody else ever completes the queue):
        Await.result(queue.watchCompletion(), 5 seconds)
        assert(task.cancelledBeforeStart)
        assert(!task.ran)
      } finally {
        // never let termination problems mask the actual test outcome:
        try {
          context.terminate(10000)
        } catch {
          case NonFatal(e) => println(s"terminating the execution context failed: $e")
        }
      }
    }
  }
}
