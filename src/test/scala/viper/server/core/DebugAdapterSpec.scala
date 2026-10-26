// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.core

import org.eclipse.lsp4j.debug._
import org.eclipse.lsp4j.debug.launch.DSPLauncher
import org.eclipse.lsp4j.debug.services.{IDebugProtocolClient, IDebugProtocolServer}
import org.scalatest.wordspec.AnyWordSpec
import viper.server.ViperConfig
import viper.server.frontends.lsp.{ClientCoordinator, ViperServerService}
import viper.server.frontends.lsp.debug.dap.{ConsoleCommand, DebugAdapterServer}

import java.net.Socket
import java.nio.file.Paths
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
  * Drives the verification debugger over the Debug Adapter Protocol, the way VS Code does: a DAP client
  * connects to the socket opened by [[DebugAdapterServer]] and goes through DAP's initialization sequence, then
  * inspects and changes the proof obligation of the failing assertion in `debug_error.vpr`.
  */
class DebugAdapterSpec extends AnyWordSpec {

  private val debug_error_file = "src/test/resources/viper/debug_error.vpr"
  private val debug_package_file = "src/test/resources/viper/debug_package.vpr"
  private val debug_fold_file = "src/test/resources/viper/debug_fold.vpr"
  private val timeoutSeconds = 300L

  /** Records the events the adapter sends. */
  private class RecordingClient extends IDebugProtocolClient {
    val events = new LinkedBlockingQueue[AnyRef]()
    override def initialized(): Unit = events.put("initialized")
    override def stopped(args: StoppedEventArguments): Unit = events.put(args)
    override def terminated(args: TerminatedEventArguments): Unit = events.put(args)
    override def invalidated(args: InvalidatedEventArguments): Unit = events.put(args)
    /** Recorded separately, since they arrive before the `stopped` event that [[withDebugSession]] waits for. */
    val breakpoints = new java.util.concurrent.ConcurrentLinkedQueue[Breakpoint]()
    override def breakpoint(args: BreakpointEventArguments): Unit = breakpoints.add(args.getBreakpoint)
    val outputs = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    override def output(args: OutputEventArguments): Unit = outputs.add(args.getOutput)

    /** Waits for the next event that satisfies `p`, dropping the others. */
    def await[T](p: PartialFunction[AnyRef, T]): T = {
      val deadline = System.currentTimeMillis() + timeoutSeconds * 1000
      while (System.currentTimeMillis() < deadline) {
        val e = events.poll(deadline - System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        if (e != null && p.isDefinedAt(e)) return p(e)
      }
      fail("timed out waiting for an event")
    }
  }

  private def get[T](f: CompletableFuture[T]): T = f.get(timeoutSeconds, TimeUnit.SECONDS)

  /**
    * Starts a server and a debug adapter, connects a DAP client to it, and runs the initialization sequence.
    *
    * @param breakpointLines the (1-based) lines of `debug_error.vpr` to set breakpoints on. Without breakpoints (and
    *                        unless `pauseOnFailures`), the session is started for the failing assertion, like the
    *                        IDE's quick fix does.
    * @param pauseOnFailures whether to enable the exception filter that pauses at verification failures.
    * @param file            the file to debug.
    * @param conditions      the conditions of some of the breakpoints, by line.
    * @param failure         the (0-based) line and column of the failure to start the session for, if it is not the
    *                        one in `debug_error.vpr`.
    */
  private def withDebugSession(breakpointLines: Seq[Int] = Nil, pauseOnFailures: Boolean = false,
                               file: String = debug_error_file, conditions: Map[Int, String] = Map.empty,
                               failure: Option[(Int, Int)] = None)
                              (test: (IDebugProtocolServer, RecordingClient, ClientCoordinator, StoppedEventArguments) => Unit): Unit = {
    val context = new DefaultVerificationExecutionContext()
    val logFile = Paths.get("logs", s"viperserver_dap_${System.currentTimeMillis()}.log").toFile
    logFile.getParentFile.mkdirs
    logFile.createNewFile()
    val server = new ViperServerService(new ViperConfig(Seq("--logLevel", "INFO", "--logFile", logFile.getAbsolutePath)))(context)
    Await.result(server.start(), 1.minute)
    val coordinator = new ClientCoordinator(server)(context)
    coordinator.setClient(new SilentClient())

    val port = DebugAdapterServer.start(coordinator)(context).get
    val socket = new Socket("localhost", port)
    try {
      val client = new RecordingClient()
      val launcher = DSPLauncher.createClientLauncher(client, socket.getInputStream, socket.getOutputStream)
      launcher.startListening()
      val adapter = launcher.getRemoteProxy

      val init = new InitializeRequestArguments()
      init.setAdapterID("viper")
      init.setLinesStartAt1(true)
      init.setColumnsStartAt1(true)
      init.setSupportsInvalidatedEvent(true)
      val caps = get(adapter.initialize(init))
      assert(caps.getSupportsConfigurationDoneRequest)

      val failureArgs = failure match {
        case Some((line, column)) => Map[String, AnyRef]("line" -> Int.box(line), "column" -> Int.box(column)).asJava
        case None => Map[String, AnyRef]("line" -> Int.box(11), "column" -> Int.box(9),
          "message" -> "Assert might fail. Assertion x.f > 5 might not hold.").asJava
      }
      val launchArgs = Map[String, AnyRef]("uri" -> Paths.get(file).toAbsolutePath.toUri.toString) ++
        (if (breakpointLines.isEmpty && !pauseOnFailures) Map("failure" -> failureArgs) else Map.empty)
      get(adapter.launch(launchArgs.asJava))
      client.await { case "initialized" => () }
      if (breakpointLines.nonEmpty) {
        val response = get(adapter.setBreakpoints(breakpointsArgs(breakpointLines, file, conditions)))
        assert(response.getBreakpoints.forall(!_.isVerified), "breakpoints can only be checked once the program is known")
      }
      if (pauseOnFailures) {
        assert(caps.getExceptionBreakpointFilters.exists(_.getFilter == "failures"))
        val filters = new SetExceptionBreakpointsArguments()
        filters.setFilters(Array("failures"))
        get(adapter.setExceptionBreakpoints(filters))
      }
      get(adapter.configurationDone(new ConfigurationDoneArguments()))
      val stopped = client.await { case s: StoppedEventArguments => s }

      test(adapter, client, coordinator, stopped)

      get(adapter.disconnect(new DisconnectArguments()))
    } finally {
      socket.close()
      Await.result(coordinator.debug.closeAll("the test has finished"), 1.minute)
      Await.result(server.stop(), 1.minute)
      context.terminate(5000)
    }
  }

  private def breakpointsArgs(lines: Seq[Int], file: String, conditions: Map[Int, String]): SetBreakpointsArguments = {
    val source = new Source()
    source.setPath(Paths.get(file).toAbsolutePath.toString)
    val args = new SetBreakpointsArguments()
    args.setSource(source)
    args.setBreakpoints(lines.map { l =>
      val b = new SourceBreakpoint()
      b.setLine(l)
      conditions.get(l).foreach(b.setCondition)
      b
    }.toArray)
    args
  }

  private def lineOf(adapter: IDebugProtocolServer, threadId: Int): Int = {
    val args = new StackTraceArguments()
    args.setThreadId(threadId)
    get(adapter.stackTrace(args)).getStackFrames.head.getLine
  }

  /** Takes a step of the given kind and returns the next stop. */
  private def step(adapter: IDebugProtocolServer, client: RecordingClient, threadId: Int, kind: String): StoppedEventArguments = {
    kind match {
      case "next" => val a = new NextArguments(); a.setThreadId(threadId); get(adapter.next(a))
      case "in" => val a = new StepInArguments(); a.setThreadId(threadId); get(adapter.stepIn(a))
      case "out" => val a = new StepOutArguments(); a.setThreadId(threadId); get(adapter.stepOut(a))
    }
    client.await { case s: StoppedEventArguments => s }
  }

  private def continue_(adapter: IDebugProtocolServer, threadId: Int): Unit = {
    val args = new ContinueArguments()
    args.setThreadId(threadId)
    get(adapter.continue_(args))
  }

  private def evaluate(adapter: IDebugProtocolServer, expression: String): EvaluateResponse = {
    val args = new EvaluateArguments()
    args.setExpression(expression)
    args.setContext(EvaluateArgumentsContext.REPL)
    get(adapter.evaluate(args))
  }

  private def scopes(adapter: IDebugProtocolServer, threadId: Int): Seq[Scope] = {
    val stArgs = new StackTraceArguments()
    stArgs.setThreadId(threadId)
    val frame = get(adapter.stackTrace(stArgs)).getStackFrames.head
    val scArgs = new ScopesArguments()
    scArgs.setFrameId(frame.getId)
    get(adapter.scopes(scArgs)).getScopes.toSeq
  }

  private def variables(adapter: IDebugProtocolServer, ref: Int): Seq[Variable] = {
    val args = new VariablesArguments()
    args.setVariablesReference(ref)
    get(adapter.variables(args)).getVariables.toSeq
  }

  "The debug adapter" should {

    "stop at the failing assertion and show its proof obligation" in withDebugSession() { (adapter, _, _, stopped) =>
      assert(stopped.getReason == StoppedEventArgumentsReason.EXCEPTION)
      val threads = get(adapter.threads()).getThreads.toSeq
      assert(threads.exists(_.getId == stopped.getThreadId))

      val stArgs = new StackTraceArguments()
      stArgs.setThreadId(stopped.getThreadId)
      val frame = get(adapter.stackTrace(stArgs)).getStackFrames.head
      assert(frame.getLine == 12, "the assertion is on line 12 of debug_error.vpr")
      assert(frame.getSource.getPath.endsWith("debug_error.vpr"))

      val names = scopes(adapter, stopped.getThreadId).map(_.getName)
      assert(Seq("Assertion", "Assumptions", "Store", "Branch conditions").forall(names.contains), names)

      val store = scopes(adapter, stopped.getThreadId).find(_.getName == "Store").get
      val storeVars = variables(adapter, store.getVariablesReference)
      assert(storeVars.exists(_.getName.startsWith("i")), storeVars.map(_.getName))

      val assumptions = scopes(adapter, stopped.getThreadId).find(_.getName == "Assumptions").get
      val assumptionVars = variables(adapter, assumptions.getVariablesReference)
      assert(assumptionVars.nonEmpty)
      assert(assumptionVars.forall(_.getName.matches("""\[\d+\]""")), assumptionVars.map(_.getName))
    }

    "run Debug Console commands and refresh the variables" in withDebugSession() { (adapter, client, _, _) =>
      assert(evaluate(adapter, ":help").getResult == ConsoleCommand.help)
      assert(evaluate(adapter, ":prove").getResult.contains("FAIL"))
      // Contradicting the branch condition makes the obligation trivially provable.
      evaluate(adapter, ":assume i > 5")
      client.await { case e: InvalidatedEventArguments => e }
      assert(evaluate(adapter, ":prove").getResult.contains("PASS"))
      evaluate(adapter, ":reset")
      assert(evaluate(adapter, ":prove").getResult.contains("FAIL"))
      // Unknown commands are reported as errors rather than silently ignored.
      assert(scala.util.Try(evaluate(adapter, ":frobnicate")).isFailure)
    }

    "end the DAP session when the debug session is closed by the server" in withDebugSession() { (_, client, coordinator, _) =>
      Await.result(coordinator.debug.closeAll("the file was modified"), 1.minute)
      client.await { case t: TerminatedEventArguments => t }
      assert(!coordinator.debug.hasSession)
    }

    "pause at breakpoints on every path, and stop at the failure afterwards" in withDebugSession(Seq(6, 12)) { (adapter, client, _, first) =>
      // The breakpoint on line 6 (the opening brace) moves to the first statement after it, on line 8.
      val resolved = client.breakpoints.asScala.toSeq
      assert(resolved.forall(_.isVerified), resolved.map(_.getMessage))
      assert(resolved.map(_.getLine).toSet == Set(8, 12))

      // The then-branch reaches line 8 and line 12, the else-branch only line 12.
      assert(first.getReason == StoppedEventArgumentsReason.BREAKPOINT)
      assert(lineOf(adapter, first.getThreadId) == 8)
      val names = scopes(adapter, first.getThreadId).map(_.getName)
      assert(!names.contains("Assertion"), "a paused verification has no assertion to prove")
      assert(Seq("Assumptions", "Store", "Branch conditions").forall(names.contains), names)
      assert(scala.util.Try(evaluate(adapter, ":prove")).isFailure, "there is nothing to prove at a breakpoint")

      val results = (1 to 2).map { _ =>
        continue_(adapter, first.getThreadId)
        val stopped = client.await { case s: StoppedEventArguments => s }
        assert(stopped.getReason == StoppedEventArgumentsReason.BREAKPOINT)
        assert(lineOf(adapter, stopped.getThreadId) == 12)
        evaluate(adapter, "x.f > 5").getResult
      }
      assert(results.toSet == Set("holds", "does not hold (it cannot be proved in this state)"),
        s"the assertion holds on one path but not on the other, got: $results")

      // Changes to a paused state are made on a copy and do not affect the verification.
      evaluate(adapter, ":assume false")
      continue_(adapter, first.getThreadId)
      val failure = client.await { case s: StoppedEventArguments => s }
      assert(failure.getReason == StoppedEventArgumentsReason.EXCEPTION, "the verification error is still found")
      assert(lineOf(adapter, failure.getThreadId) == 12)
    }

    "abort a paused verification when the debug session is closed" in withDebugSession(Seq(12)) { (_, client, coordinator, stopped) =>
      assert(stopped.getReason == StoppedEventArgumentsReason.BREAKPOINT)
      Await.result(coordinator.debug.closeAll("the file was modified"), 1.minute)
      client.await { case t: TerminatedEventArguments => t }
      // The aborted run must not leave the server refusing verifications.
      val deadline = System.currentTimeMillis() + 60000
      while (coordinator.debug.launchInProgress && System.currentTimeMillis() < deadline) java.lang.Thread.sleep(100)
      assert(!coordinator.debug.launchInProgress)
      assert(!coordinator.debug.hasSession)
    }

    "step from statement to statement, across the paths of a method" in withDebugSession(Seq(8)) { (adapter, client, _, first) =>
      // The then-branch: line 8, then the assertion on line 12.
      assert(lineOf(adapter, first.getThreadId) == 8)
      val afterAssignment = step(adapter, client, first.getThreadId, "next")
      assert(afterAssignment.getReason == StoppedEventArgumentsReason.STEP)
      assert(lineOf(adapter, afterAssignment.getThreadId) == 12)
      // The then-branch ends after the assertion, so the next statement is the first one of the else-branch.
      val elseBranch = step(adapter, client, first.getThreadId, "in")
      assert(lineOf(adapter, elseBranch.getThreadId) == 10)
      assert(evaluate(adapter, "i > 5").getResult.startsWith("does not hold"), "the else-branch assumes !(i > 5)")
      // Outside of a package proof script, stepping out continues; the debugger stops at the failure afterwards.
      val failure = step(adapter, client, first.getThreadId, "out")
      assert(failure.getReason == StoppedEventArgumentsReason.EXCEPTION)
      // Once the verification has finished, there is nothing to step any more.
      val next = new NextArguments()
      next.setThreadId(failure.getThreadId)
      assert(scala.util.Try(get(adapter.next(next))).isFailure)
    }

    "pause at a verification failure while the verification is running" in withDebugSession(pauseOnFailures = true) { (adapter, client, _, stopped) =>
      assert(stopped.getReason == StoppedEventArgumentsReason.EXCEPTION)
      assert(stopped.getThreadId == 1, "the failure should be shown on the thread of the running verification")
      assert(lineOf(adapter, stopped.getThreadId) == 12)
      val names = scopes(adapter, stopped.getThreadId).map(_.getName)
      assert(names.contains("Assertion"), names)
      assert(evaluate(adapter, ":prove").getResult.contains("FAIL"))
      val info = new ExceptionInfoArguments()
      info.setThreadId(stopped.getThreadId)
      assert(get(adapter.exceptionInfo(info)).getDescription.contains("might not hold"))
      // After the verification, the failure is shown again, with all others.
      continue_(adapter, stopped.getThreadId)
      val afterwards = client.await { case s: StoppedEventArguments => s }
      assert(afterwards.getThreadId >= 2)
    }

    "step over or into the proof script of a package statement" in withDebugSession(Seq(7), file = debug_package_file) { (adapter, client, _, first) =>
      // Paused before the package statement on line 7; its proof script is on line 8.
      assert(lineOf(adapter, first.getThreadId) == 7)
      val inScript = step(adapter, client, first.getThreadId, "in")
      assert(lineOf(adapter, inScript.getThreadId) == 8, "stepping in enters the proof script")
      val afterScript = step(adapter, client, first.getThreadId, "out")
      assert(afterScript.getReason == StoppedEventArgumentsReason.STEP)
      assert(lineOf(adapter, afterScript.getThreadId) == 10, "stepping out leaves the proof script")
      // The method verifies, so the session ends with the verification.
      continue_(adapter, first.getThreadId)
      client.await { case t: TerminatedEventArguments => t }
    }

    "step over the proof script of a package statement" in withDebugSession(Seq(7), file = debug_package_file) { (adapter, client, _, first) =>
      val next = step(adapter, client, first.getThreadId, "next")
      assert(lineOf(adapter, next.getThreadId) == 10, "stepping over skips the proof script")
    }

    "only stop at a conditional breakpoint on paths on which the condition may hold" in
      withDebugSession(Seq(12), conditions = Map(12 -> "i == 7")) { (adapter, client, _, first) =>
        // On the then-branch, i > 5, so i may be 7; on the else-branch, i <= 5, so i == 7 is provably false.
        assert(first.getReason == StoppedEventArgumentsReason.BREAKPOINT)
        assert(lineOf(adapter, first.getThreadId) == 12)
        assert(evaluate(adapter, "x.f > 5").getResult == "holds", "the breakpoint should only stop on the then-branch")
        continue_(adapter, first.getThreadId)
        val next = client.await { case s: StoppedEventArguments => s }
        assert(next.getReason == StoppedEventArgumentsReason.EXCEPTION, "the else-branch should not stop at the breakpoint")
      }

    "stop at a breakpoint whose condition cannot be checked, and say why" in
      withDebugSession(Seq(12), conditions = Map(12 -> "y > 0")) { (adapter, client, _, first) =>
        assert(first.getReason == StoppedEventArgumentsReason.BREAKPOINT)
        assert(client.outputs.asScala.exists(_.contains("could not be checked")),
          client.outputs.asScala.mkString)
      }

    "unfold a predicate, check the failure again, and report a failing fold" in
      withDebugSession(pauseOnFailures = true, file = debug_fold_file) { (adapter, _, _, stopped) =>
        // readCell: x.f cannot be read, since cell(x) is folded.
        assert(stopped.getReason == StoppedEventArgumentsReason.EXCEPTION)
        assert(lineOf(adapter, stopped.getThreadId) == 15)
        assert(evaluate(adapter, ":recheck").getResult.contains("FAIL"))

        def heap(): Seq[Variable] = {
          val s = scopes(adapter, stopped.getThreadId).find(_.getName == "Heap").get
          variables(adapter, s.getVariablesReference)
        }
        val chunks = heap()
        // The obligation names variables by their version, which is also what the user can type.
        assert(chunks.exists(v => v.getEvaluateName == "cell(x@1)" && v.getValue == "acc(cell(x@1), write)"),
          chunks.map(v => s"${v.getValue} (${v.getEvaluateName})"))

        assert(evaluate(adapter, ":unfold cell(x)").getResult.contains("Unfolded cell(x)"))
        assert(heap().exists(_.getValue.startsWith("acc(x@1.f")), heap().map(_.getValue))
        assert(scopes(adapter, stopped.getThreadId).exists(_.getName == "Assertion (before fold/unfold)"))
        assert(evaluate(adapter, ":recheck").getResult.contains("PASS"))

        val failedFold = scala.util.Try(evaluate(adapter, ":fold list(x)"))
        assert(failedFold.isFailure, "folding list(x) needs permission to x.next, which there is none of")
        assert(failedFold.failed.get.getMessage.contains("Folding list(x) failed"), failedFold.failed.get.getMessage)
        // The failed fold leaves the obligation as it was.
        assert(heap().exists(_.getValue.startsWith("acc(x@1.f")))
      }

    "unfold a conditional predicate into one case per branch" in
      withDebugSession(pauseOnFailures = true, file = debug_fold_file) { (adapter, client, _, first) =>
        // Go on to the failure in readList.
        continue_(adapter, first.getThreadId)
        val stopped = client.await { case s: StoppedEventArguments => s }
        assert(lineOf(adapter, stopped.getThreadId) == 21)

        val unfolded = evaluate(adapter, ":unfold list(x)").getResult
        assert(unfolded.contains("2 cases"), unfolded)
        val first_ = evaluate(adapter, "x.next != null").getResult
        assert(evaluate(adapter, ":case 2").getResult.contains("Showing case 2"))
        val second = evaluate(adapter, "x.next != null").getResult
        assert(Set(first_, second) == Set("holds", "does not hold (it cannot be proved in this state)"),
          s"the cases should differ in x.next != null, got: $first_, $second")
        assert(scala.util.Try(evaluate(adapter, ":case 3")).isFailure)
      }

    "explain why a predicate cannot be unfolded in a session restricted to the failing member" in
      withDebugSession(file = debug_fold_file, failure = Some((14, 9))) { (adapter, _, _, stopped) =>
        // Like the IDE's quick fix, the session verifies only readCell, in which cell is abstract.
        assert(stopped.getReason == StoppedEventArgumentsReason.EXCEPTION)
        assert(lineOf(adapter, stopped.getThreadId) == 15)
        val unfold = scala.util.Try(evaluate(adapter, ":unfold cell(x@1)"))
        assert(unfold.isFailure)
        assert(unfold.failed.get.getMessage.contains("has no body in the program that was verified"),
          unfold.failed.get.getMessage)
      }
  }
}
