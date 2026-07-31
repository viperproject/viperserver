// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.core

import org.eclipse.lsp4j.{MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ShowMessageRequestParams}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Assertion, Outcome, Succeeded}
import viper.server.ViperConfig
import viper.server.frontends.lsp._
import viper.server.utility.ViperAstGenerator
import viper.silver.logger.SilentLogger
import viper.silver.reporter.OverallFailureMessage

import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/**
  * End-to-end tests of the verification debugger as it is driven over LSP: a debug session is started for a
  * verification error, commands are issued against it, and it is torn down again.
  *
  * These tests exercise everything except the JSON transport itself, i.e. the same code paths the Viper IDE
  * uses. They are structured like `CoreServerSpec`, with one execution context per test case.
  */
class DebugSessionSpec extends AnyWordSpec with Matchers {

  private val debug_error_file = "src/test/resources/viper/debug_error.vpr"
  private val execution_context_terminate_timeout_ms = 5000

  private def uriOf(file: String): String = Paths.get(file).toAbsolutePath.toUri.toString

  /** The position (0-based, as in LSP) of the failing assertion in `debug_error.vpr`. */
  private val failingAssertion = new Position(11, 9)
  private val failingAssertionMessage = "Assert might fail. Assertion x.f > 5 might not hold."

  private var currentTestName: Option[String] = None
  override def withFixture(test: NoArgTest): Outcome = {
    currentTestName = Some(test.name)
    val res = super.withFixture(test)
    currentTestName = None
    res
  }

  /** Starts a server with a connected coordinator, runs the test code, and shuts everything down again. */
  private def withCoordinator(testCode: (ClientCoordinator, ViperServerService, VerificationExecutionContext) => Future[Assertion]): Assertion = {
    val context = new DefaultVerificationExecutionContext()
    val logFile = Paths.get("logs", s"viperserver_debug_${System.currentTimeMillis()}.log").toFile
    logFile.getParentFile.mkdirs
    logFile.createNewFile()
    val config = new ViperConfig(Seq("--logLevel", "INFO", "--logFile", logFile.getAbsolutePath))
    val server = new ViperServerService(config)(context)

    val fut = server.start().flatMap({ _ =>
      val coordinator = new ClientCoordinator(server)(context)
      coordinator.setClient(new SilentClient())
      testCode(coordinator, server, context).transformWith(res => {
        coordinator.debug.closeAll("the test has finished").transformWith(_ => Future.fromTry(res))(context)
      })(context)
    })(context)

    val withShutdown = fut.transformWith(res => server.stop().transformWith(_ => Future.fromTry(res))(context))(context)
    // A generous timeout, but not unbounded: a hanging session should fail the test rather than the build.
    val res = Await.result(withShutdown, 10 minutes)
    context.terminate(execution_context_terminate_timeout_ms.toLong)
    res
  }

  private def startSession(coordinator: ClientCoordinator, restrictToMember: Boolean = true)
                          (implicit ctx: VerificationExecutionContext): Future[DebugStartResponse] =
    coordinator.debug.start(DebugStartParams(
      uri = uriOf(debug_error_file),
      backend = "silicon",
      customArgs = "",
      position = failingAssertion,
      message = failingAssertionMessage,
      restrictToMember = restrictToMember,
      withCounterexample = true))

  "The verification debugger" should {

    "open a proof obligation for a verification error" in withCoordinator { (coordinator, _, context) =>
      implicit val ctx: VerificationExecutionContext = context
      startSession(coordinator).map { res =>
        assert(res.error == null, s"starting the session failed: ${res.error}")
        assert(res.sessionId != null)
        assert(res.failures.nonEmpty)
        assert(res.selected >= 0, "the failure the user asked about should have been found")
        val obl = res.obligation
        assert(obl != null)
        assert(obl.assertion != null)
        assert(obl.errorMessage.contains("might not hold"))
        // The obligation of this program has a branch condition, a store and a heap chunk.
        assert(obl.branchConditions.nonEmpty)
        assert(obl.store.nonEmpty)
        assert(obl.heaps.nonEmpty)
        assert(obl.assumptions.nonEmpty)
        assert(obl.renderedText.contains("Original Error:"))
        // The debug run computes a counterexample for the failure, which comes with the obligation.
        assert(obl.counterexample != null, "no counterexample was computed")
        assert(!obl.counterexample.stale)
        assert(obl.counterexample.sections.exists(_.nodes.exists(_.label.startsWith("i -> "))),
          s"expected a value for i, got: ${obl.counterexample.sections.flatMap(_.nodes).map(_.label).mkString(", ")}")
      }
    }

    "recompute the counterexample from the assumptions the user added" in withCoordinator { (coordinator, _, context) =>
      implicit val ctx: VerificationExecutionContext = context
      for {
        started <- startSession(coordinator)
        session = coordinator.debug.get(started.sessionId).get
        // Adding an assumption invalidates the counterexample, ...
        added <- session.run(_.addAssumption("i == 4", free = true))
        // ... and proving again produces one that satisfies it.
        proved <- session.run(_.prove())
      } yield {
        assert(added.obligation.exists(_.counterexample.exists(_.stale)),
          "the counterexample should be marked as out of date after an assumption was added")
        assert(proved.proved.contains(false))
        val ce = proved.obligation.flatMap(_.counterexample)
        assert(ce.exists(!_.stale))
        val values = ce.toSeq.flatMap(_.sections.filter(_.key == "current").flatMap(_.nodes.map(_.label)))
        assert(values.contains("i -> 4"), s"expected i to be 4, got: ${values.mkString(", ")}")
      }
    }

    "report that the obligation cannot be proved, and accept a free assumption" in withCoordinator { (coordinator, _, context) =>
      implicit val ctx: VerificationExecutionContext = context
      for {
        started <- startSession(coordinator)
        session = coordinator.debug.get(started.sessionId).get
        proveRes <- session.run(_.prove())
        assumptionsBefore = started.obligation.assumptions.length
        addRes <- session.run(_.addAssumption("i > 3", free = true))
      } yield {
        assert(proveRes.proved.contains(false), "the obligation must not be provable")
        assert(addRes.ok, s"adding an assumption failed: ${addRes.messages.map(_.text).mkString("; ")}")
        assert(addRes.obligation.exists(_.assumptions.length > assumptionsBefore))
      }
    }

    "expand an assumption and change the print configuration" in withCoordinator { (coordinator, _, context) =>
      implicit val ctx: VerificationExecutionContext = context
      for {
        started <- startSession(coordinator)
        session = coordinator.debug.get(started.sessionId).get
        // Showing internal assumptions must reveal at least as many nodes as before.
        withInternal <- session.run(_.setPrintConfig(
          viper.silicon.debugger.PrintConfigModel(printInternal = true, nChildrenToShow = 5, hierarchyLevel = 2,
            printAxioms = false, printInternalTermRepresentation = false, printOldHeaps = false)))
        expandable = started.obligation.assumptions.find(_.childCount > 0).map(_.id)
        expanded <- expandable match {
          case Some(id) => session.run(_.expand(id))
          case None => Future.successful(Right(Seq.empty))
        }
      } yield {
        assert(withInternal.ok)
        assert(withInternal.obligation.exists(_.assumptions.length >= started.obligation.assumptions.length))
        expanded match {
          case Right(nodes) => assert(expandable.isEmpty || nodes.nonEmpty)
          case Left(err) => fail(s"expanding a node failed: $err")
        }
      }
    }

    "refuse to debug a backend other than Silicon" in withCoordinator { (coordinator, _, context) =>
      implicit val ctx: VerificationExecutionContext = context
      coordinator.debug.start(DebugStartParams(uriOf(debug_error_file), "carbon", "", failingAssertion,
        failingAssertionMessage, restrictToMember = false)).map { res =>
        assert(res.error != null)
        assert(!coordinator.debug.hasSession)
      }
    }

    "allow a normal verification after the session has been closed" in withCoordinator { (coordinator, server, context) =>
      implicit val ctx: VerificationExecutionContext = context
      for {
        started <- startSession(coordinator)
        _ = assert(started.error == null)
        _ <- coordinator.debug.closeAll("the test closes the session")
        _ = assert(!coordinator.debug.hasSession)
        // Verifying again must work, i.e. the debug run must not have left the server in a broken state
        // (in particular, Silicon's global configuration must not still be the one of the debug run).
        program = new ViperAstGenerator(SilentLogger().get).generateViperAst(debug_error_file).get
        jid = server.verify(debug_error_file, SiliconConfig(List("--disableCaching")), program)
        messages <- ViperCoreServerUtils.getMessagesFuture(server, jid)
      } yield {
        assert(jid.id >= 0)
        assert(messages.exists(_.isInstanceOf[OverallFailureMessage]),
          s"expected the verification to fail again, got: ${messages.map(_.name).mkString(", ")}")
      }
    }
  }

  /** A client that ignores everything the server sends it. */
  private class SilentClient extends IdeLanguageClient {
    override def requestIdentifier(pos: Position): CompletableFuture[GetIdentifierResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
    override def requestRange(range: Range): CompletableFuture[GetRangeResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
    override def requestVprFileEndings(): CompletableFuture[GetViperFileEndingsResponse] = CompletableFuture.completedFuture(GetViperFileEndingsResponse(Array(".vpr", ".sil")))
    override def requestSetupProject(param: SetupProjectParams): CompletableFuture[Unit] = CompletableFuture.completedFuture(())
    override def notifyLog(param: LogParams): Unit = {}
    override def notifyHint(param: HintMessage): Unit = {}
    override def notifyUnhandledViperServerMessage(params: UnhandledViperServerMessageTypeParams): Unit = {}
    override def notifyVerificationNotStarted(params: VerificationNotStartedParams): Unit = {}
    override def notifyStateChanged(params: StateChangeParams): Unit = {}
    override def notifyDebugSessionState(params: DebugSessionStateParams): Unit = {}
    override def notifyDebugSessionClosed(params: DebugSessionClosedParams): Unit = {}
    override def telemetryEvent(`object`: Any): Unit = {}
    override def publishDiagnostics(diagnostics: PublishDiagnosticsParams): Unit = {}
    override def showMessage(messageParams: MessageParams): Unit = {}
    override def showMessageRequest(requestParams: ShowMessageRequestParams): CompletableFuture[MessageActionItem] = CompletableFuture.failedFuture(new UnsupportedOperationException())
    override def logMessage(message: MessageParams): Unit = {}
  }
}
