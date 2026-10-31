// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.vsi

import akka.actor.{Actor, Props, Status}
import akka.util.Timeout
import ch.qos.logback.classic.Logger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory
import viper.server.core.{DefaultVerificationExecutionContext, VerificationExecutionContext}

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.util.control.NonFatal

/** A `MessageStreamingTask` that reports a fixed list of messages and then returns `result`.
  *
  * It follows the same protocol as the real workers (`AstWorker`, `VerificationWorker`): it reports
  * its end via `registerTaskEnd` on success as well as on interruption.
  *
  * @param blockAfter if defined, the task blocks after having reported that many messages until it
  *                   is either released via `release` or interrupted
  */
class ScriptedTask[T](messages: Seq[String], result: T, blockAfter: Option[Int] = None)
  extends MessageStreamingTask[T] {

  type A = String
  override def pack(m: String): Envelope = ProbeEnvelope(m)
  override def unpack(e: Envelope): String = e.asInstanceOf[ProbeEnvelope].s

  private val logger: Logger = LoggerFactory.getLogger(getClass.getName).asInstanceOf[Logger]

  /** counted down once the callable has started */
  val started = new CountDownLatch(1)
  /** counted down once the callable has returned or thrown */
  val finished = new CountDownLatch(1)
  /** unblocks a task that has been configured with `blockAfter` */
  val release = new CountDownLatch(1)
  @volatile var wasInterrupted: Boolean = false

  override def call(): T = {
    started.countDown()
    try {
      messages.zipWithIndex.foreach { case (m, idx) =>
        if (blockAfter.contains(idx)) release.await()
        enqueueMessage(pack(m), logger)
      }
      if (blockAfter.contains(messages.length)) release.await()
      registerTaskEnd(success = true, logger)
      result
    } catch {
      case e: InterruptedException =>
        wasInterrupted = true
        registerTaskEnd(success = false, logger)
        throw e
    } finally {
      finished.countDown()
    }
  }
}

/** A minimal `VerificationServer` that operates on [[ScriptedTask]]s instead of real backends.
  *
  * It exposes the protected job-management API of `VerificationServer` such that tests can exercise
  * the job lifecycle directly. `subscribe` is the only place in these tests that depends on how
  * messages are delivered to consumers (currently: via an actor), i.e., it is the single place
  * that needs to change once the consumer API changes.
  */
class TestVerificationServer(implicit val executor: VerificationExecutionContext)
  extends VerificationServer with DefaultVerificationServerStart {

  override type AST = String
  override type A = String
  override def pack(m: String): Envelope = ProbeEnvelope(m)
  override def unpack(e: Envelope): String = e.asInstanceOf[ProbeEnvelope].s

  override lazy val askTimeout: Timeout = Timeout(5.seconds)

  def startAst(task: MessageStreamingTask[Option[String]]): AstJobId =
    initializeAstConstruction(task)

  def startVerification(task: MessageStreamingTask[Unit]): VerJobId =
    initializeVerificationProcess(Future.successful(Some(task)), None)

  /** mirrors `ViperCoreServer.verifyWithAstJob`: the verification task is only created once the AST
    * job produced its artifact, and it is not created at all if there is no AST */
  def startVerificationOfAst(astId: AstJobId, mkTask: String => MessageStreamingTask[Unit]): VerJobId = {
    ast_jobs.lookupJob(astId) match {
      case Some(handleFut) =>
        val taskFut = handleFut.flatMap(handle => handle.artifact.map(_.map(mkTask)))
        initializeVerificationProcess(taskFut, Some(astId))
      case None => VerJobId(-1)
    }
  }

  def activeAstJobs: Int = ast_jobs.jobHandles.size
  def activeVerificationJobs: Int = ver_jobs.jobHandles.size

  /** CURRENT QUIRK: jobs are started lazily, namely the first time they are looked up in their pool.
    * This forces the job to start without subscribing to its messages. */
  def forceStart(jid: VerJobId): Unit = ver_jobs.lookupJob(jid)
  def forceStart(jid: AstJobId): Unit = ast_jobs.lookupJob(jid)

  def subscribe(jid: VerJobId, includeAst: Boolean): Future[List[String]] =
    collect(collector => streamMessages(jid, collector, includeAst))

  def subscribe(jid: AstJobId): Future[List[String]] =
    collect(collector => streamMessages(jid, collector))

  /** completes with all messages delivered to the consumer once the consumer has been notified
    * about the end of the stream; fails if the stream fails or the job is unknown */
  private def collect(stream: akka.actor.ActorRef => Option[Future[akka.Done]]): Future[List[String]] = {
    val promise = Promise[List[String]]()
    val collector = system.actorOf(Props(new TestVerificationServer.Collector(promise)))
    stream(collector) match {
      case Some(streamFut) => streamFut.failed.foreach(promise.tryFailure)
      case None => promise.tryFailure(JobNotFoundException)
    }
    promise.future
  }
}

object TestVerificationServer {
  private class Collector(promise: Promise[List[String]]) extends Actor {
    private var received: List[String] = Nil
    override def receive: PartialFunction[Any, Unit] = {
      case m: String => received = received :+ m
      case Status.Success => promise.trySuccess(received); context.stop(self)
      case Status.Failure(cause) => promise.tryFailure(cause); context.stop(self)
    }
  }
}

/** Characterization tests for the job lifecycle of `VerificationServer`: running jobs, streaming
  * their messages, chaining verification jobs onto AST jobs, interrupting jobs, and the job pools'
  * capacity.
  *
  * These tests pin down the behavior that frontends (LSP, HTTP) and library users (e.g. Gobra) rely
  * on, independently of real backends, such that the job management can be reimplemented without
  * Akka. Tests in the section "current quirks" document incidental behavior of the current
  * implementation that a reimplementation may deliberately change.
  */
class JobLifecycleTests extends AnyWordSpec with Matchers {

  private val timeout: FiniteDuration = 10.seconds

  private def await[T](f: Future[T]): T = Await.result(f, timeout)

  private def awaitLatch(latch: CountDownLatch): Unit =
    assert(latch.await(timeout.toMillis, TimeUnit.MILLISECONDS), "timed out waiting for latch")

  /** polls `condition` until it holds; job slots are freed asynchronously after streams complete */
  private def eventually(condition: => Boolean): Unit = {
    val deadline = timeout.fromNow
    while (!condition) {
      if (deadline.isOverdue()) fail("condition did not hold before timeout")
      Thread.sleep(10)
    }
  }

  private def withServer(maxActiveJobs: Int = 3)(test: TestVerificationServer => Unit): Unit = {
    val context = new DefaultVerificationExecutionContext(
      actorSystemName = "JobLifecycleTests", threadPoolSize = Some(4))
    try {
      val server = new TestVerificationServer()(context)
      await(server.start(maxActiveJobs))
      try {
        test(server)
      } finally {
        if (server.isRunning) Try(await(server.stop()))
      }
    } finally {
      try {
        context.terminate(10000)
      } catch {
        case NonFatal(e) => println(s"terminating the execution context failed: $e")
      }
    }
  }

  "VerificationServer" should {

    "deliver all messages of a verification job in order and then complete the stream" in withServer() { server =>
      val task = new ScriptedTask[Unit](Seq("m1", "m2", "m3"), ())
      val jid = server.startVerification(task)
      assert(jid.id >= 0)
      await(server.subscribe(jid, includeAst = true)) shouldBe List("m1", "m2", "m3")
    }

    "deliver more messages than the internal buffer holds without losing or reordering any" in withServer() { server =>
      val messages = (1 to 25000).map(i => s"m$i")
      val jid = server.startVerification(new ScriptedTask[Unit](messages, ()))
      await(server.subscribe(jid, includeAst = true)) shouldBe messages.toList
    }

    "deliver messages that a job reported before the consumer subscribed" in withServer() { server =>
      val task = new ScriptedTask[Unit](Seq("early1", "early2"), ())
      val jid = server.startVerification(task)
      server.forceStart(jid)
      awaitLatch(task.finished)
      await(server.subscribe(jid, includeAst = true)) shouldBe List("early1", "early2")
    }

    "deliver the messages of an AST job to a consumer of that AST job" in withServer() { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("a1", "a2"), Some("program")))
      assert(astId.id >= 0)
      await(server.subscribe(astId)) shouldBe List("a1", "a2")
    }

    "fail to subscribe to an unknown job" in withServer() { server =>
      a[JobNotFoundException.type] should be thrownBy await(server.subscribe(VerJobId(42), includeAst = true))
      a[JobNotFoundException.type] should be thrownBy await(server.subscribe(AstJobId(42)))
    }

    "pass the AST job's artifact to the dependent verification task" in withServer() { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("a1"), Some("program")))
      @volatile var receivedAst: Option[String] = None
      val verId = server.startVerificationOfAst(astId, ast => {
        receivedAst = Some(ast)
        new ScriptedTask[Unit](Seq("v1"), ())
      })
      await(server.subscribe(verId, includeAst = false)) shouldBe List("v1")
      receivedAst shouldBe Some("program")
    }

    "prepend the AST job's messages to the verification job's messages if requested" in withServer() { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("a1", "a2"), Some("program")))
      val verId = server.startVerificationOfAst(astId, _ => new ScriptedTask[Unit](Seq("v1", "v2"), ()))
      await(server.subscribe(verId, includeAst = true)) shouldBe List("a1", "a2", "v1", "v2")
    }

    "omit the AST job's messages from the verification job's stream if not requested" in withServer() { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("a1", "a2"), Some("program")))
      val verId = server.startVerificationOfAst(astId, _ => new ScriptedTask[Unit](Seq("v1", "v2"), ()))
      await(server.subscribe(verId, includeAst = false)) shouldBe List("v1", "v2")
      // the AST messages remain available to a separate consumer of the AST job:
      await(server.subscribe(astId)) shouldBe List("a1", "a2")
    }

    "complete a verification job without verification messages if its AST job produced no AST" in withServer() { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("parse error"), None))
      @volatile var taskCreated = false
      val verId = server.startVerificationOfAst(astId, _ => {
        taskCreated = true
        new ScriptedTask[Unit](Seq("v1"), ())
      })
      assert(verId.id >= 0)
      await(server.subscribe(verId, includeAst = true)) shouldBe List("parse error")
      assert(!taskCreated)
      eventually(server.activeVerificationJobs == 0)
    }

    "reject new verification jobs at capacity and admit them again once earlier jobs completed" in withServer(maxActiveJobs = 2) { server =>
      val blocked = (1 to 2).map(i => new ScriptedTask[Unit](Seq(s"m$i"), (), blockAfter = Some(1)))
      val jids = blocked.map(server.startVerification)
      assert(jids.forall(_.id >= 0))
      server.startVerification(new ScriptedTask[Unit](Seq("rejected"), ())).id shouldBe -1

      val streams = jids.map(jid => server.subscribe(jid, includeAst = true))
      blocked.foreach(_.release.countDown())
      streams.map(await(_)) shouldBe List(List("m1"), List("m2"))

      // verification jobs free their slot automatically once their stream completed:
      eventually(server.activeVerificationJobs == 0)
      val jid = server.startVerification(new ScriptedTask[Unit](Seq("admitted"), ()))
      assert(jid.id >= 0)
      await(server.subscribe(jid, includeAst = true)) shouldBe List("admitted")
    }

    "keep an AST job's slot after the job completed until the job is discarded" in withServer(maxActiveJobs = 1) { server =>
      val astId = server.startAst(new ScriptedTask[Option[String]](Seq("a1"), Some("program")))
      await(server.subscribe(astId)) shouldBe List("a1")
      server.activeAstJobs shouldBe 1
      server.startAst(new ScriptedTask[Option[String]](Seq("rejected"), Some("program"))).id shouldBe -1

      server.discardAstJob(astId)
      server.activeAstJobs shouldBe 0
      assert(server.startAst(new ScriptedTask[Option[String]](Seq("a2"), Some("program"))).id >= 0)
    }

    "interrupt a running verification, complete its stream, and free its slot" in withServer() { server =>
      val task = new ScriptedTask[Unit](Seq("before", "after"), (), blockAfter = Some(1))
      val jid = server.startVerification(task)
      val stream = server.subscribe(jid, includeAst = true)
      awaitLatch(task.started)

      await(server.interruptVerification(jid)) shouldBe true
      await(stream) shouldBe List("before")
      assert(task.wasInterrupted)
      eventually(server.activeVerificationJobs == 0)
    }

    "not report an interruption for unknown or already completed verification jobs" in withServer() { server =>
      await(server.interruptVerification(VerJobId(42))) shouldBe false

      val task = new ScriptedTask[Unit](Seq("m1"), ())
      val jid = server.startVerification(task)
      await(server.subscribe(jid, includeAst = true)) shouldBe List("m1")
      awaitLatch(task.finished)
      await(server.interruptVerification(jid)) shouldBe false
    }

    "interrupt a running AST job and complete its stream" in withServer() { server =>
      val task = new ScriptedTask[Option[String]](Seq("a1", "a2"), Some("program"), blockAfter = Some(1))
      val astId = server.startAst(task)
      val stream = server.subscribe(astId)
      awaitLatch(task.started)

      await(server.interruptAstConstruction(astId)) shouldBe true
      await(stream) shouldBe List("a1")
      assert(task.wasInterrupted)
    }

    "end and discard a verification job whose AST job is interrupted" in withServer() { server =>
      val astTask = new ScriptedTask[Option[String]](Seq("a1"), Some("program"), blockAfter = Some(1))
      val astId = server.startAst(astTask)
      @volatile var taskCreated = false
      val verId = server.startVerificationOfAst(astId, _ => {
        taskCreated = true
        new ScriptedTask[Unit](Seq("v1"), ())
      })
      val verStream = server.subscribe(verId, includeAst = true)
      awaitLatch(astTask.started)
      server.activeVerificationJobs shouldBe 1

      // the verification job has no task yet that could be interrupted:
      await(server.interruptVerification(verId)) shouldBe false
      await(server.interruptAstConstruction(astId)) shouldBe true

      // the verification's stream ends (successfully or not) without any verification messages:
      Await.ready(verStream, timeout)
      verStream.value.get.foreach(msgs => assert(!msgs.contains("v1")))
      assert(!taskCreated)
      eventually(server.activeVerificationJobs == 0)
    }

    "interrupt running jobs when the server is stopped" in withServer() { server =>
      val verTask = new ScriptedTask[Unit](Seq("v1", "v2"), (), blockAfter = Some(1))
      val astTask = new ScriptedTask[Option[String]](Seq("a1", "a2"), Some("program"), blockAfter = Some(1))
      val verStream = server.subscribe(server.startVerification(verTask), includeAst = true)
      val astStream = server.subscribe(server.startAst(astTask))
      awaitLatch(verTask.started)
      awaitLatch(astTask.started)

      await(server.stop())
      assert(!server.isRunning)
      await(verStream) shouldBe List("v1")
      await(astStream) shouldBe List("a1")
      assert(verTask.wasInterrupted)
      assert(astTask.wasInterrupted)
    }

    "refuse new jobs after the server has been stopped" in withServer() { server =>
      await(server.stop())
      an[IllegalStateException] should be thrownBy server.startVerification(new ScriptedTask[Unit](Nil, ()))
      an[IllegalStateException] should be thrownBy server.startAst(new ScriptedTask[Option[String]](Nil, None))
    }
  }

  "VerificationServer (current quirks)" should {

    "not start a job's task before the job is looked up, e.g. by subscribing to its messages" in withServer() { server =>
      val task = new ScriptedTask[Unit](Seq("m1"), ())
      val jid = server.startVerification(task)
      assert(!task.started.await(500, TimeUnit.MILLISECONDS),
        "the task started although nobody looked up its job")
      val stream = server.subscribe(jid, includeAst = true)
      awaitLatch(task.started)
      await(stream) shouldBe List("m1")
    }
  }
}
