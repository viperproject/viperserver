// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug.dap

import org.eclipse.lsp4j.debug._
import org.eclipse.lsp4j.debug.services.{IDebugProtocolClient, IDebugProtocolServer}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.ClientCoordinator
import viper.server.frontends.lsp.debug.{DebugLaunchRequest, ServerDebugSession, StartedDebugSession}
import viper.silicon.{debugger => sil}

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import java.{lang, util}
import scala.collection.mutable
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

/** The arguments of the custom `viperRemoveAssumptions` request. */
case class RemoveAssumptionsArguments(ids: Array[Int])

/** The arguments of the custom `viperSetPrintConfig` request; fields that are null are left unchanged. */
case class SetPrintConfigArguments(printInternal: lang.Boolean,
                                   printAxioms: lang.Boolean,
                                   printInternalTermRepresentation: lang.Boolean,
                                   printOldHeaps: lang.Boolean,
                                   hierarchyLevel: Integer,
                                   nChildrenToShow: Integer)

/** The arguments of the custom `viperCommand` request: a Debug Console command such as `:prove`. */
case class CommandArguments(command: String)

/**
  * The response of the custom `viperCommand` request.
  *
  * @param proved null unless the command ran the prover, on the proof obligation or on an expression.
  */
case class CommandResponse(ok: Boolean, message: String, proved: lang.Boolean)

/** The arguments of the custom `viperFold` request: a predicate instance such as `list(x)`. */
case class FoldArguments(predicate: String, unfold: Boolean)

/** The response of the custom `viperGetText` request: the obligation and counterexample as the CLI prints them. */
case class TextResponse(obligation: String, counterexample: String)

/**
  * The verification debugger as a Debug Adapter Protocol server.
  *
  * A DAP session covers one debug run: the debug run is started once the client has sent `configurationDone`,
  * i.e. after the initialization sequence of DAP, which is also when the client has sent its breakpoints.
  *
  *  - While the debug run is verified, it is shown as the thread [[LiveThreadId]]. A [[sil.DebugRunController]]
  *    pauses it at breakpoints, after steps (`next`, `stepIn`, `stepOut`), on `pause`, and at verification failures
  *    if the exception filter [[ViperDebugAdapter.FailuresFilter]] is enabled. The thread then stops with the state
  *    of the paused path (or the failure) as its proof obligation, and `continue` lets the verification go on.
  *  - Once the verification has finished, every verification error is shown as a thread that is stopped at the
  *    error: thread `i + 2` is failure `i` of the session. Silicon's debug session has only one current proof
  *    obligation, so selecting a frame of another thread opens that thread's obligation and discards changes made
  *    to the previous one. `continue` goes to the next error.
  *
  * Commands are entered in the Debug Console (see [[ConsoleCommand]]); an expression without a command is checked
  * in the current state, which also makes watch expressions work.
  *
  * Several custom requests (`viperCommand`, `viperRemoveAssumptions`, `viperFold`, `viperGetPrintConfig`,
  * `viperSetPrintConfig`, `viperGetText`) let the IDE offer the same operations from menus.
  */
class ViperDebugAdapter(coordinator: ClientCoordinator)(implicit executor: VerificationExecutionContext)
  extends IDebugProtocolServer with sil.DebugRunListener {

  import ViperDebugAdapter._

  @volatile private var client: Option[IDebugProtocolClient] = None

  private var linesStartAt1 = true
  private var columnsStartAt1 = true
  private var supportsInvalidated = false

  private val handles = new VariableHandles
  @volatile private var dapModel = new DapModel(handles, linesStartAt1, columnsStartAt1)

  private val controller = new sil.DebugRunController(this)
  /** The path each breakpoint was set for, as the client sent it, by breakpoint id. */
  private val breakpointSources = mutable.Map.empty[Int, String]

  @volatile private var launchRequest: Option[DebugLaunchRequest] = None
  /** Whether the debug run is still being verified. */
  @volatile private var running = false
  /** The pause the debug run is in, and a session that serializes the commands on its state. */
  @volatile private var livePause: Option[(sil.DebugPause, ServerDebugSession)] = None
  private val pauseCounter = new AtomicInteger(0)
  @volatile private var started: Option[StartedDebugSession] = None
  /** The thread whose proof obligation is open, and the model of that obligation. */
  @volatile private var current: Option[(Int, sil.ObligationModel)] = None
  /** Whether the `terminated` event has been sent (or need not be sent any more). */
  @volatile private var terminated = false
  /** Whether the client asked to end the session, in which case closing it needs no explanation. */
  @volatile private var endedByClient = false

  def connect(c: IDebugProtocolClient): Unit = client = Some(c)

  private def postMortemSession: Option[ServerDebugSession] = started.map(_.session).filterNot(_.isClosed)

  private def failures: Seq[sil.DebugFailureInfo] = started.map(_.failures).getOrElse(Seq.empty)

  /** The thread commands apply to unless the client names one: the one whose obligation is open, or the paused one. */
  private def focusedThread: Option[Int] = current.map(_._1).orElse(livePause.map(_ => LiveThreadId))

  private def sessionFor(threadId: Int): Option[ServerDebugSession] =
    if (threadId == LiveThreadId) livePause.map(_._2).filterNot(_.isClosed) else postMortemSession

  // --- Initialization ------------------------------------------------------------------------------------

  override def initialize(args: InitializeRequestArguments): CompletableFuture[Capabilities] = {
    linesStartAt1 = Option(args.getLinesStartAt1).forall(_.booleanValue)
    columnsStartAt1 = Option(args.getColumnsStartAt1).forall(_.booleanValue)
    supportsInvalidated = Option(args.getSupportsInvalidatedEvent).exists(_.booleanValue)
    dapModel = new DapModel(handles, linesStartAt1, columnsStartAt1)

    val caps = new Capabilities()
    caps.setSupportsConfigurationDoneRequest(true)
    caps.setSupportsTerminateRequest(true)
    caps.setSupportsExceptionInfoRequest(true)
    caps.setSupportsEvaluateForHovers(false)
    caps.setSupportsSetVariable(false)
    caps.setSupportsRestartRequest(false)
    caps.setSupportsConditionalBreakpoints(true)
    val failuresFilter = new ExceptionBreakpointsFilter()
    failuresFilter.setFilter(FailuresFilter)
    failuresFilter.setLabel("Verification failures")
    failuresFilter.setDescription("Pause the verification when an assertion fails, with the failing proof obligation.")
    failuresFilter.setDefault_(false)
    caps.setExceptionBreakpointFilters(Array(failuresFilter))
    CompletableFuture.completedFuture(caps)
  }

  /**
    * Accepts the launch configuration. The attributes are documented in the `debuggers` contribution of the
    * Viper IDE's package.json: `uri` (or `program`, a path), `backend`, `customArgs`, `failure` (`line`,
    * `column`, both 0-based, and `message`), `restrictToMember` and `counterexample`.
    */
  override def launch(args: util.Map[String, AnyRef]): CompletableFuture[Void] = {
    val a = args.asScala
    val uri = a.get("uri").collect { case s: String if s.nonEmpty => s }
      .orElse(a.get("program").collect { case s: String if s.nonEmpty => java.nio.file.Paths.get(s).toUri.toString })
    uri match {
      case None =>
        failed("The launch configuration must specify the file to debug ('uri' or 'program').")
      case Some(u) =>
        val failure = a.get("failure").collect { case m: util.Map[_, _] => m.asScala.toMap.map { case (k, v) => k.toString -> v } }
        def int(m: Map[String, Any], key: String): Option[Int] = m.get(key).collect { case n: Number => n.intValue }
        val position = failure.flatMap(f => for (l <- int(f, "line"); c <- int(f, "column")) yield (l, c))
        val message = failure.flatMap(_.get("message")).collect { case s: String => s }
        def bool(key: String, default: Boolean): Boolean = a.get(key).collect { case b: lang.Boolean => b.booleanValue }.getOrElse(default)

        launchRequest = Some(DebugLaunchRequest(
          uri = u,
          backend = a.get("backend").collect { case s: String => s }.getOrElse("silicon"),
          customArgs = a.get("customArgs").collect { case s: String => s }.getOrElse(""),
          position = position,
          message = message,
          restrictToMember = bool("restrictToMember", default = true) && position.isDefined,
          withCounterexample = bool("counterexample", default = true)))
        // Only now are we ready to receive the configuration (breakpoints etc.).
        client.foreach(_.initialized())
        CompletableFuture.completedFuture(null)
    }
  }

  override def configurationDone(args: ConfigurationDoneArguments): CompletableFuture[Void] = {
    launchRequest match {
      case None =>
        failed("configurationDone was sent before launch.")
      case Some(request) =>
        // Verifying only the failing member would skip the breakpoints in all other members.
        startDebugRun(if (controller.breakpoints.nonEmpty) request.copy(restrictToMember = false) else request)
        CompletableFuture.completedFuture(null)
    }
  }

  /**
    * Breakpoints are resolved against the program once the verification has started (see
    * [[breakpointsChanged]]); until then, they are reported as unverified. A breakpoint's condition is a Viper
    * expression; the breakpoint stops unless it is provably false (see [[sil.DebugRunController.setBreakpoints]]).
    */
  override def setBreakpoints(args: SetBreakpointsArguments): CompletableFuture[SetBreakpointsResponse] = {
    val path = Option(args.getSource).flatMap(s => Option(s.getPath))
    val requested = Option(args.getBreakpoints).getOrElse(Array.empty[SourceBreakpoint]).toSeq
    val lines = requested.map(_.getLine)
    val response = new SetBreakpointsResponse()
    path match {
      case None =>
        response.setBreakpoints(lines.map(l => unverifiedBreakpoint(l, "Breakpoints need a file.")).toArray)
      case Some(p) =>
        val statuses = controller.setBreakpoints(p, requested.map(sb =>
          sil.BreakpointRequest(viperLine(sb.getLine), Option(sb.getCondition).map(_.trim).filter(_.nonEmpty))))
        synchronized(statuses.foreach(st => breakpointSources(st.id) = p))
        response.setBreakpoints(statuses.map(toBreakpoint).toArray)
    }
    CompletableFuture.completedFuture(response)
  }

  override def setExceptionBreakpoints(args: SetExceptionBreakpointsArguments): CompletableFuture[SetExceptionBreakpointsResponse] = {
    controller.pauseOnFailures = Option(args.getFilters).exists(_.contains(FailuresFilter))
    CompletableFuture.completedFuture(new SetExceptionBreakpointsResponse())
  }

  private def startDebugRun(request: DebugLaunchRequest): Unit = {
    running = true
    coordinator.debug.start(request, msg => output(msg + "\n"), Some(controller)).foreach { result =>
      running = false
      livePause = None
      if (current.exists(_._1 == LiveThreadId)) current = None
      threadExited(LiveThreadId)
      result match {
        case Left(_) if endedByClient =>
        case Left(err) =>
          controller.abortReason match {
            case Some(reason) => terminate(Some(s"The debug session was closed: $reason."))
            case None =>
              output(s"$err\n")
              terminate(None)
          }

        case Right(_) if endedByClient =>
          // The client went away while the debug run was being verified; nobody is going to close the session,
          // and an open session blocks all verifications.
          coordinator.debug.closeAll("the debugger disconnected while the debug session was started")

        case Right(s) =>
          started = Some(s)
          s.session.onClose(reason => terminate(if (endedByClient) None else Some(s"The debug session was closed: $reason.")))
          output(s"The debug run reported ${s.failures.size} verification error(s):\n" +
            s.failures.map(f => s"  [${f.index + 1}] ${f.message} (${f.posString})" +
              (if (f.debuggable) "" else " - no debug information") + "\n").mkString +
            "Type :help in the Debug Console for the available commands.\n")
          val first = if (s.selected >= 0) Some(s.selected) else s.failures.find(_.debuggable).map(_.index)
          first match {
            case None =>
              // Closing the session also ends the DAP session, see `onClose` above.
              coordinator.debug.closeAll("none of the verification errors can be debugged")
            case Some(idx) =>
              openThread(failureThreadId(idx)).foreach { res =>
                // Stop even if the obligation could not be opened: selecting the frame tries again.
                res.swap.foreach(err => output(s"$err\n", "stderr"))
                stoppedAtFailure(idx, preserveFocus = false)
              }
          }
      }
    }
  }

  // --- The debug run (DebugRunListener) -----------------------------------------------------------------

  override def breakpointsChanged(breakpoints: Seq[sil.BreakpointStatus]): Unit =
    breakpoints.foreach { st =>
      val e = new BreakpointEventArguments()
      e.setReason(BreakpointEventArgumentsReason.CHANGED)
      e.setBreakpoint(toBreakpoint(st))
      client.foreach(_.breakpoint(e))
    }

  /** Called on the thread of the paused verifier, which waits until `continue` is requested. */
  override def paused(pause: sil.DebugPause): Unit = {
    val uri = launchRequest.map(_.uri).getOrElse("")
    val session = new ServerDebugSession(s"pause-${pauseCounter.incrementAndGet()}", uri, None, pause.session)
    pause.messages.foreach(m => output(m.text.stripSuffix("\n") + "\n", outputCategory(m)))
    livePause = Some((pause, session))
    if (current.exists(_._1 == LiveThreadId)) current = None
    handles.reset()
    stoppedLive(pause, preserveFocus = false)
  }

  // --- Threads, frames, scopes, variables ----------------------------------------------------------------

  override def threads(): CompletableFuture[ThreadsResponse] = {
    val live =
      if (running) {
        val t = new org.eclipse.lsp4j.debug.Thread()
        t.setId(LiveThreadId)
        t.setName("Verification")
        Seq(t)
      } else Seq.empty
    val errors = failures.map { f =>
      val t = new org.eclipse.lsp4j.debug.Thread()
      t.setId(failureThreadId(f.index))
      t.setName(s"[${f.index + 1}] ${f.message}")
      t
    }
    val response = new ThreadsResponse()
    response.setThreads((live ++ errors).toArray)
    CompletableFuture.completedFuture(response)
  }

  override def stackTrace(args: StackTraceArguments): CompletableFuture[StackTraceResponse] = {
    def frame(name: String, pos: Option[sil.DebugSourcePos]): StackFrame = {
      val frame = new StackFrame()
      frame.setId(args.getThreadId)
      frame.setName(name)
      pos.foreach { p =>
        dapModel.source(p).foreach(frame.setSource)
        frame.setLine(dapModel.line(p.startLine))
        frame.setColumn(dapModel.column(p.startColumn))
        frame.setEndLine(dapModel.line(p.endLine))
        frame.setEndColumn(dapModel.column(p.endColumn))
      }
      frame
    }
    val frames =
      if (args.getThreadId == LiveThreadId)
        livePause.toSeq.map { case (p, _) => frame(p.memberName.getOrElse("verification"), p.position) }
      else
        failure(args.getThreadId).toSeq.map(f => frame(f.memberName.getOrElse("verification error"), f.pos))
    val response = new StackTraceResponse()
    response.setStackFrames(frames.toArray)
    response.setTotalFrames(frames.size)
    CompletableFuture.completedFuture(response)
  }

  /** The frame of a thread has the thread's id, so the frame identifies the failure or the pause. */
  override def scopes(args: ScopesArguments): CompletableFuture[ScopesResponse] = {
    val threadId = args.getFrameId
    val fut = openThread(threadId).map {
      case Left(err) => throw error(err)
      case Right(model) =>
        val response = new ScopesResponse()
        // At a failure, the obligation has an assertion (and maybe a counterexample) like after the verification.
        val atStatement = threadId == LiveThreadId && !livePause.exists(_._1.reason.isInstanceOf[sil.PauseReason.Failure])
        response.setScopes(dapModel.scopes(model, paused = atStatement).toArray)
        response
    }
    fut.asJava.toCompletableFuture
  }

  override def variables(args: VariablesArguments): CompletableFuture[VariablesResponse] = {
    import VariableContainer._
    val session = current.flatMap { case (threadId, _) => sessionFor(threadId) }
    val fut: Future[Seq[Variable]] = handles.get(args.getVariablesReference) match {
      case None => Future.successful(Seq.empty)
      case Some(Nodes(nodes)) => Future.successful(dapModel.variables(nodes))
      case Some(Sections(sections)) => Future.successful(dapModel.sectionVariables(sections))
      case Some(Entries(entries)) => Future.successful(dapModel.entryVariables(entries))
      case Some(Children(node)) if node.hasHiddenChildren && node.id >= 0 && session.isDefined =>
        // The model only contains the first levels of the assumption tree; the rest is requested on demand.
        session.get.run(_.expand(node.id)).map {
          case Right(children) => dapModel.variables(children)
          case Left(err) => throw error(err)
        }
      case Some(Children(node)) => Future.successful(dapModel.variables(node.children))
    }
    fut.map { vars =>
      val response = new VariablesResponse()
      response.setVariables(vars.toArray)
      response
    }.asJava.toCompletableFuture
  }

  override def exceptionInfo(args: ExceptionInfoArguments): CompletableFuture[ExceptionInfoResponse] = {
    val response = new ExceptionInfoResponse()
    response.setExceptionId("Verification error")
    val liveFailure = if (args.getThreadId != LiveThreadId) None else livePause.map(_._1.reason).collect {
      case sil.PauseReason.Failure(message) => message
    }
    response.setDescription(liveFailure.orElse(failure(args.getThreadId).map(_.message)).getOrElse(""))
    response.setBreakMode(ExceptionBreakMode.ALWAYS)
    CompletableFuture.completedFuture(response)
  }

  // --- Execution control ---------------------------------------------------------------------------------

  /**
    * While the verification is paused, `continue` lets it go on. Afterwards, there is nothing to run any more,
    * so `continue` goes to the next verification error that can be debugged.
    */
  override def continue_(args: ContinueArguments): CompletableFuture[ContinueResponse] = {
    val response = new ContinueResponse()
    if (livePause.isDefined && (args.getThreadId == LiveThreadId || failures.isEmpty)) {
      resumeLive()
      response.setAllThreadsContinued(true)
      return CompletableFuture.completedFuture(response)
    }

    val from = current.map(_._1).filter(_ != LiveThreadId).map(failureIndex).getOrElse(failureIndex(args.getThreadId))
    failures.find(f => f.debuggable && f.index > from) match {
      case Some(next) =>
        openThread(failureThreadId(next.index)).foreach {
          case Left(err) =>
            output(s"$err\n", "stderr")
            stoppedAtFailure(from, preserveFocus = true)
          case Right(_) => stoppedAtFailure(next.index, preserveFocus = false)
        }
      case None =>
        output("There are no further verification errors to debug.\n")
        // The client considers the thread running until it is told otherwise.
        executor.execute(() => stoppedAtFailure(from, preserveFocus = true))
    }
    response.setAllThreadsContinued(false)
    CompletableFuture.completedFuture(response)
  }

  override def next(args: NextArguments): CompletableFuture[Void] = step(args.getThreadId, sil.StepKind.Over)
  override def stepIn(args: StepInArguments): CompletableFuture[Void] = step(args.getThreadId, sil.StepKind.In)
  override def stepOut(args: StepOutArguments): CompletableFuture[Void] = step(args.getThreadId, sil.StepKind.Out)

  /** Steps are steps of the paused verification; see [[sil.DebugRunController.step]]. */
  private def step(threadId: Int, kind: sil.StepKind): CompletableFuture[Void] =
    if (livePause.isEmpty || threadId != LiveThreadId) {
      failed("Only a paused verification can step; once the verification has finished, use Continue to go to the next verification error.")
    } else {
      releaseLive(controller.step(kind))
      CompletableFuture.completedFuture(null)
    }

  /** Pauses the running verification before the next statement it executes. */
  override def pause(args: PauseArguments): CompletableFuture[Void] = {
    if (running && livePause.isEmpty) controller.requestPause()
    CompletableFuture.completedFuture(null)
  }

  override def disconnect(args: DisconnectArguments): CompletableFuture[Void] = {
    // The client is going away, so there is nobody to send `terminated` to.
    endedByClient = true
    terminated = true
    closeSession("the debug session was ended").asJava.toCompletableFuture.thenApply[Void](_ => null)
  }

  override def terminate(args: TerminateArguments): CompletableFuture[Void] = {
    endedByClient = true
    closeSession("the debug session was ended").foreach(_ => terminate(None))
    CompletableFuture.completedFuture(null)
  }

  /** Called when the connection to the client is gone. */
  def connectionClosed(): Unit = {
    endedByClient = true
    terminated = true
    closeSession("the debugger disconnected")
  }

  // --- The Debug Console ---------------------------------------------------------------------------------

  override def evaluate(args: EvaluateArguments): CompletableFuture[EvaluateResponse] = {
    // The frame of a thread has the thread's id; without a frame, the command applies to the open obligation.
    val threadId = Option(args.getFrameId).map(_.intValue).orElse(focusedThread)
    runCommand(args.getExpression, threadId).map { case (ok, message, _) =>
      if (!ok) throw error(message)
      val response = new EvaluateResponse()
      response.setResult(message)
      response.setVariablesReference(0)
      response
    }.asJava.toCompletableFuture
  }

  @JsonRequest("viperCommand")
  def command(args: CommandArguments): CompletableFuture[CommandResponse] =
    runCommand(Option(args.command).getOrElse(""), focusedThread)
      .map { case (ok, message, proved) => CommandResponse(ok, message, proved.map(lang.Boolean.valueOf).orNull) }
      .asJava.toCompletableFuture

  @JsonRequest("viperRemoveAssumptions")
  def removeAssumptions(args: RemoveAssumptionsArguments): CompletableFuture[CommandResponse] =
    withSession(focusedThread)(_.run(_.removeAssumptions(Option(args.ids).getOrElse(Array.empty[Int]).toSeq)))
      .map(r => CommandResponse(r.ok, r.messages.map(_.text).mkString("\n").trim, null))
      .asJava.toCompletableFuture

  @JsonRequest("viperSetPrintConfig")
  def setPrintConfig(args: SetPrintConfigArguments): CompletableFuture[CommandResponse] =
    current match {
      case None => failed("There is no proof obligation.")
      case Some((threadId, model)) =>
        val c = model.printConfig
        val updated = sil.PrintConfigModel(
          printInternal = Option(args.printInternal).map(_.booleanValue).getOrElse(c.printInternal),
          nChildrenToShow = Option(args.nChildrenToShow).map(_.intValue).getOrElse(c.nChildrenToShow),
          hierarchyLevel = Option(args.hierarchyLevel).map(_.intValue).getOrElse(c.hierarchyLevel),
          printAxioms = Option(args.printAxioms).map(_.booleanValue).getOrElse(c.printAxioms),
          printInternalTermRepresentation =
            Option(args.printInternalTermRepresentation).map(_.booleanValue).getOrElse(c.printInternalTermRepresentation),
          printOldHeaps = Option(args.printOldHeaps).map(_.booleanValue).getOrElse(c.printOldHeaps))
        withSession(Some(threadId))(_.run(_.setPrintConfig(updated)))
          .map(r => CommandResponse(r.ok, r.messages.map(_.text).mkString("\n").trim, null))
          .asJava.toCompletableFuture
    }

  /** Folds or unfolds a predicate instance, e.g. the one of a heap chunk the user right-clicked. */
  @JsonRequest("viperFold")
  def fold(args: FoldArguments): CompletableFuture[CommandResponse] =
    withSession(focusedThread)(_.run(_.foldText(Option(args.predicate).getOrElse(""), args.unfold)))
      .map(r => CommandResponse(r.ok, r.messages.map(_.text).mkString("\n").trim, null))
      .asJava.toCompletableFuture

  /** The print configuration of the current proof obligation, so that the IDE can toggle its settings. */
  @JsonRequest("viperGetPrintConfig")
  def getPrintConfig(): CompletableFuture[sil.PrintConfigModel] =
    current match {
      case None => failed("There is no proof obligation.")
      case Some((_, model)) => CompletableFuture.completedFuture(model.printConfig)
    }

  @JsonRequest("viperGetText")
  def getText(): CompletableFuture[TextResponse] =
    CompletableFuture.completedFuture(current match {
      case None => TextResponse(null, null)
      case Some((_, model)) =>
        TextResponse(sil.DebugRenderer.renderObligation(model),
          model.counterexample.map(sil.DebugRenderer.renderCounterexample).orNull)
    })

  /**
    * Runs a Debug Console command on the obligation of the given thread and returns whether it succeeded, what to
    * show, and whether it proved anything.
    */
  private def runCommand(input: String, threadId: Option[Int]): Future[(Boolean, String, Option[Boolean])] = {
    import ConsoleCommand._
    def ok(message: String) = Future.successful((true, message, None))
    def fail(message: String) = Future.successful((false, message, None))
    def obligation[T](f: sil.ObligationModel => T): Option[T] = current.map { case (_, m) => f(m) }

    parse(input) match {
      case Help => ok(help)
      case Invalid(message) => fail(message)
      case Expression(text) if text.isEmpty => ok("")
      case Expression(text) =>
        // Checking an expression may assume its well-definedness conditions in the obligation, but showing that
        // would refresh the variables and with them the watch expressions, which would be checked again.
        withSession(threadId, refresh = false)(_.run(_.assertText(text))).map { r =>
          r.proved match {
            case Some(true) => (true, "holds", r.proved)
            case Some(false) => (true, "does not hold (it cannot be proved in this state)", r.proved)
            case None => (false, r.messages.map(_.text).mkString("\n").trim, None)
          }
        }
      case Text => obligation(sil.DebugRenderer.renderObligation).map(ok).getOrElse(fail("There is no proof obligation."))
      case ConsoleCommand.Counterexample =>
        obligation(_.counterexample.map(sil.DebugRenderer.renderCounterexample))
          .map(_.map(ok).getOrElse(fail("There is no counterexample for this proof obligation.")))
          .getOrElse(fail("There is no proof obligation."))
      case cmd =>
        val run: sil.SiliconDebugSession => sil.DebugCommandResult = cmd match {
          case Prove => _.prove()
          case Reset => _.reset()
          case Assume(e, free) => _.addAssumption(e, free)
          case Remove(ids) => _.removeAssumptions(ids)
          case Solver(name, args) => _.setProver(name, args)
          case Timeout(ms) => _.setTimeout(ms)
          case Fold(predicate, unfold) => _.foldText(predicate, unfold)
          case Case(n) => _.selectCase(n)
          case Recheck => _.recheck()
          case Set(setting, value) => s => s.currentObligation match {
            case None => sil.DebugCommandResult.error("There is no proof obligation.")
            case Some(m) => s.setPrintConfig(updatedPrintConfig(m.printConfig, setting, value))
          }
          case other => _ => sil.DebugCommandResult.error(s"Unexpected command $other")
        }
        withSession(threadId)(_.run(run)).map(r => (r.ok, r.messages.map(_.text).mkString("\n").trim, r.proved))
    }
  }

  private def updatedPrintConfig(c: sil.PrintConfigModel, setting: ConsoleCommand.PrintSetting,
                                 value: Either[Boolean, Int]): sil.PrintConfigModel = {
    import ConsoleCommand.PrintSetting._
    (setting, value) match {
      case (Internal, Left(b)) => c.copy(printInternal = b)
      case (Axioms, Left(b)) => c.copy(printAxioms = b)
      case (Terms, Left(b)) => c.copy(printInternalTermRepresentation = b)
      case (OldHeaps, Left(b)) => c.copy(printOldHeaps = b)
      case (Depth, Right(n)) => c.copy(hierarchyLevel = n)
      case (Children, Right(n)) => c.copy(nChildrenToShow = n)
      case _ => c
    }
  }

  /**
    * Runs a command of Silicon's debug session for the given thread and makes its outcome visible: messages go to
    * the Debug Console, and (if `refresh`) a changed obligation makes the client re-request the variables.
    */
  private def withSession(threadId: Option[Int], refresh: Boolean = true)
                         (f: ServerDebugSession => Future[sil.DebugCommandResult]): Future[sil.DebugCommandResult] =
    threadId.flatMap(id => sessionFor(id).map(id -> _)) match {
      case None => Future.successful(sil.DebugCommandResult.error("There is no proof obligation to work with."))
      case Some((id, s)) =>
        f(s).map { r =>
          r.obligation.foreach { model =>
            if (refresh && current.exists(_._1 == id)) {
              setCurrent(id, model)
              invalidate(id)
            }
          }
          r
        }
    }

  // --- Internals -----------------------------------------------------------------------------------------

  private def failure(threadId: Int): Option[sil.DebugFailureInfo] = failures.find(_.index == failureIndex(threadId))

  /** Makes the proof obligation of the given thread the current one, unless it already is. */
  private def openThread(threadId: Int): Future[Either[String, sil.ObligationModel]] = {
    current match {
      case Some((t, model)) if t == threadId => Future.successful(Right(model))
      case _ if threadId == LiveThreadId =>
        sessionFor(LiveThreadId) match {
          case None => Future.successful(Left("The verification is not paused."))
          case Some(s) =>
            s.run(_.currentObligation).map {
              case Some(model) =>
                setCurrent(LiveThreadId, model)
                Right(model)
              case None => Left("The state of the paused verification could not be opened.")
            }
        }
      case previous =>
        val idx = failureIndex(threadId)
        (postMortemSession, failures.find(_.index == idx)) match {
          case (None, _) => Future.successful(Left("There is no debug session."))
          case (_, None) => Future.successful(Left(s"There is no verification error ${idx + 1}."))
          case (_, Some(f)) if !f.debuggable =>
            Future.successful(Left(s"Verification error ${idx + 1} carries no debug information."))
          case (Some(s), Some(_)) =>
            s.run(_.openObligation(idx)).map { r =>
              r.messages.foreach(m => output(m.text.stripSuffix("\n") + "\n", outputCategory(m)))
              r.obligation match {
                case Some(model) if r.ok =>
                  if (previous.exists(_._1 != LiveThreadId)) {
                    output(s"Switched to verification error ${idx + 1}; changes to the previous proof obligation were discarded.\n")
                  }
                  setCurrent(threadId, model)
                  Right(model)
                case _ =>
                  Left(s"Could not open the proof obligation of verification error ${idx + 1}.")
              }
            }
        }
    }
  }

  private def setCurrent(threadId: Int, model: sil.ObligationModel): Unit = {
    handles.reset()
    current = Some((threadId, model))
  }

  private def resumeLive(): Unit = releaseLive(controller.resume())

  /** Forgets the current pause and lets the verification continue with `continueVerification`. */
  private def releaseLive(continueVerification: => Unit): Unit = {
    livePause = None
    if (current.exists(_._1 == LiveThreadId)) current = None
    handles.reset()
    continueVerification
  }

  /** Tells the client that the variables of the given thread changed. */
  private def invalidate(threadId: Int): Unit = {
    if (supportsInvalidated) {
      val e = new InvalidatedEventArguments()
      e.setAreas(Array(InvalidatedAreas.VARIABLES))
      e.setThreadId(threadId)
      client.foreach(_.invalidated(e))
    } else if (threadId == LiveThreadId) {
      livePause.foreach { case (p, _) => stoppedLive(p, preserveFocus = true) }
    } else {
      stoppedAtFailure(failureIndex(threadId), preserveFocus = true)
    }
  }

  private def stoppedLive(pause: sil.DebugPause, preserveFocus: Boolean): Unit = {
    val e = new StoppedEventArguments()
    pause.reason match {
      case sil.PauseReason.Breakpoint(ids) =>
        e.setReason(StoppedEventArgumentsReason.BREAKPOINT)
        e.setDescription("Paused at a breakpoint")
        e.setHitBreakpointIds(ids.map(Int.box).toArray)
      case sil.PauseReason.Step =>
        e.setReason(StoppedEventArgumentsReason.STEP)
      case sil.PauseReason.Pause =>
        e.setReason(StoppedEventArgumentsReason.PAUSE)
      case sil.PauseReason.Failure(message) =>
        e.setReason(StoppedEventArgumentsReason.EXCEPTION)
        e.setDescription("Verification failure")
        e.setText(message)
    }
    e.setThreadId(LiveThreadId)
    e.setAllThreadsStopped(true)
    e.setPreserveFocusHint(preserveFocus)
    client.foreach(_.stopped(e))
  }

  private def stoppedAtFailure(idx: Int, preserveFocus: Boolean): Unit = {
    val e = new StoppedEventArguments()
    e.setReason(StoppedEventArgumentsReason.EXCEPTION)
    e.setThreadId(failureThreadId(idx))
    e.setAllThreadsStopped(true)
    e.setPreserveFocusHint(preserveFocus)
    e.setDescription("Verification error")
    failures.find(_.index == idx).foreach(f => e.setText(f.message))
    client.foreach(_.stopped(e))
  }

  private def threadExited(threadId: Int): Unit = {
    val e = new ThreadEventArguments()
    e.setReason(ThreadEventArgumentsReason.EXITED)
    e.setThreadId(threadId)
    client.foreach(_.thread(e))
  }

  private def output(text: String, category: String = OutputEventArgumentsCategory.CONSOLE): Unit = {
    val e = new OutputEventArguments()
    e.setCategory(category)
    e.setOutput(text)
    client.foreach(_.output(e))
  }

  private def outputCategory(m: sil.DebugMessage): String =
    if (m.severity == sil.DebugMessageSeverity.Error) OutputEventArgumentsCategory.STDERR
    else OutputEventArgumentsCategory.CONSOLE

  /** The (1-based) Viper line of a line sent by the client, and vice versa. */
  private def viperLine(clientLine: Int): Int = if (linesStartAt1) clientLine else clientLine + 1

  private def toBreakpoint(st: sil.BreakpointStatus): Breakpoint = {
    val bp = new Breakpoint()
    bp.setId(st.id)
    bp.setVerified(st.verified)
    bp.setLine(dapModel.line(st.location.map(_.line).getOrElse(st.requestedLine)))
    st.location.foreach(l => bp.setColumn(dapModel.column(l.column)))
    synchronized(breakpointSources.get(st.id)).foreach { path =>
      val src = new Source()
      src.setPath(path)
      bp.setSource(src)
    }
    if (!st.verified) {
      bp.setMessage(
        if (running) "There is no statement on or after this line in a method."
        else "The breakpoint is checked once the verification has started.")
    }
    bp
  }

  private def unverifiedBreakpoint(clientLine: Int, message: String): Breakpoint = {
    val bp = new Breakpoint()
    bp.setVerified(false)
    bp.setLine(clientLine)
    bp.setMessage(message)
    bp
  }

  /** Ends the DAP session (once), optionally explaining why. */
  private def terminate(reason: Option[String]): Unit = synchronized {
    if (!terminated) {
      terminated = true
      reason.foreach(r => output(r + "\n"))
      client.foreach(_.terminated(new TerminatedEventArguments()))
    }
  }

  /** Ends the debug run, if it is still being verified, and closes the Silicon debug session shown by this adapter. */
  private def closeSession(reason: String): Future[Unit] = {
    if (running) {
      // Aborts the debug run via its controller; the registry has no session yet.
      return coordinator.debug.closeAll(reason)
    }
    postMortemSession match {
      case Some(s) if coordinator.debug.current.contains(s) => coordinator.debug.closeAll(reason)
      case Some(s) => s.close(reason)
      case None => Future.unit
    }
  }

  private def error(message: String): ResponseErrorException =
    new ResponseErrorException(new ResponseError(ResponseErrorCode.InvalidRequest, message, null))

  private def failed[T](message: String): CompletableFuture[T] = CompletableFuture.failedFuture(error(message))
}

object ViperDebugAdapter {
  /** The exception filter that makes the verification pause at verification failures. */
  val FailuresFilter: String = "failures"

  /** The thread that shows the verification while it is running and paused at breakpoints. */
  val LiveThreadId: Int = 1

  /** Failure `i` of the debug run is shown as thread `i + 2`. */
  def failureThreadId(failureIndex: Int): Int = failureIndex + 2

  def failureIndex(threadId: Int): Int = threadId - 2
}
