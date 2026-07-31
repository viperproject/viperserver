// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug

import org.eclipse.lsp4j.Position
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.ClientCoordinator
import viper.silicon.{debugger => sil}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future

/**
  * Owns the debug session of one client.
  *
  * There can be at most one debug session at a time, and none while a verification is running: Silicon keeps
  * the configuration of the most recently created verifier in a global (`Verifier.config`), and the frontend
  * state the debugger needs to typecheck user input in another one (`FrontendStateCache`). A second
  * verification would therefore silently change the meaning of a live session. The single-session rule is
  * enforced across clients, since they all share one `ViperServerService`.
  */
class DebugSessionRegistry(coordinator: ClientCoordinator)(implicit executor: VerificationExecutionContext) {

  private val counter = new AtomicInteger(0)

  /** True while a debug run is being started; verifications are refused during that window. */
  @volatile private var _launchInProgress: Boolean = false

  def launchInProgress: Boolean = _launchInProgress

  def current: Option[ServerDebugSession] = DebugSessionRegistry.current

  def get(sessionId: String): Option[ServerDebugSession] =
    current.filter(s => s.sessionId == sessionId && !s.isClosed)

  def hasSession: Boolean = current.isDefined

  /** Starts a debug session for the failure the user asked about, replacing any session that is still open. */
  def start(params: lsp.DebugStartParams): Future[lsp.DebugStartResponse] = {
    if (params.backend != null && params.backend.toLowerCase != "silicon") {
      return Future.successful(errorResponse(s"Debugging is only supported for Silicon, not for '${params.backend}'."))
    }
    val path = lsp.Common.uriToPath(lsp.Common.uriFromString(params.uri)).toString

    _launchInProgress = true
    closeAll("a new debug session is being started").flatMap { _ =>
      // A verification that is still running would create another Silicon verifier, and with it overwrite the
      // global configuration the debug run depends on: the debug run would then produce failures without any
      // debug information. `launchInProgress` keeps further verifications from starting in the meantime.
      coordinator.stopAllRunningVerifications().recover { case _ => () }
    }.flatMap { _ =>
      val sessionId = s"debug-${counter.incrementAndGet()}"
      notifyState(sessionId, "starting", "Starting the verification debugger...")

      val selectAt =
        if (params.restrictToMember && params.position != null)
          Some((params.position.getLine + 1, params.position.getCharacter + 1))
        else None

      coordinator.server.startDebugVerification(
        file = path,
        customArgs = Option(params.customArgs).getOrElse(""),
        selectMemberAt = selectAt,
        withCounterexample = params.withCounterexample,
        loader = Some(coordinator.fileContent(params.uri)),
        onProgress = msg => notifyState(sessionId, "running", msg),
        localLogger = Some(coordinator.localLogger)
      ).flatMap {
        case Left(err) =>
          _launchInProgress = false
          notifyState(sessionId, "failed", err)
          Future.successful(errorResponse(err))

        case Right((silicon, inner)) =>
          val session = new ServerDebugSession(sessionId, params.uri, silicon, inner)
          DebugSessionRegistry.setCurrent(session)
          val failures = inner.failures.map(DebugProtocolConverter.toFailure).toArray
          val selected = selectFailure(failures, params.position, params.message)
          if (selected < 0) {
            _launchInProgress = false
            notifyState(sessionId, "ready", "Select a verification error to debug.")
            Future.successful(lsp.DebugStartResponse(sessionId, failures, -1, null, Array.empty))
          } else {
            session.run(_.openObligation(selected)).map { res =>
              _launchInProgress = false
              notifyState(sessionId, "ready", "The debug session is ready.")
              lsp.DebugStartResponse(sessionId, failures, selected,
                res.obligation.map(DebugProtocolConverter.toObligation(sessionId, _)).orNull,
                DebugProtocolConverter.toMessages(res.messages),
                if (res.ok) null else "Could not open the proof obligation.")
            }
          }
      }
    }.recover { case e: Throwable =>
      // Whatever goes wrong, the launch must not stay "in progress", or verifications would stay blocked.
      _launchInProgress = false
      coordinator.logger.error(s"Starting a debug session failed: $e")
      errorResponse(s"Starting a debug session failed: ${e.getMessage}")
    }
  }

  /** Closes the current session, if any, and tells the client about it. */
  def closeAll(reason: String): Future[Unit] = {
    DebugSessionRegistry.takeCurrent() match {
      case None => Future.unit
      case Some(session) =>
        coordinator.logger.info(s"Closing debug session ${session.sessionId}: $reason")
        coordinator.client.foreach(_.notifyDebugSessionClosed(lsp.DebugSessionClosedParams(session.sessionId, reason)))
        session.close()
    }
  }

  /** Closes the session if it belongs to the given file; used when the file changes or is closed. */
  def closeIfFor(uri: String, reason: String): Future[Unit] =
    if (current.exists(_.uri == uri)) closeAll(reason) else Future.unit

  private def notifyState(sessionId: String, state: String, message: String): Unit =
    coordinator.client.foreach(_.notifyDebugSessionState(lsp.DebugSessionStateParams(sessionId, state, message)))

  private def errorResponse(error: String): lsp.DebugStartResponse =
    lsp.DebugStartResponse(null, Array.empty, -1, null, Array.empty, error)

  /**
    * Finds the failure that corresponds to the diagnostic the user clicked on. The debug run is a second
    * verification, so failures may be ordered differently than in the run that produced the diagnostics;
    * we therefore match on position and message rather than on an index.
    */
  private def selectFailure(failures: Array[lsp.DebugFailure], pos: Position, message: String): Int = {
    val candidates = failures.filter(_.debuggable)
    if (candidates.isEmpty) return -1

    def score(f: lsp.DebugFailure): Int = {
      var s = 0
      if (pos != null && f.pos != null) {
        if (f.pos.startLine == pos.getLine) s += 4
        if (f.pos.startCharacter == pos.getCharacter) s += 2
      }
      if (message != null && f.message != null && (message.contains(f.message) || f.message.contains(message))) s += 1
      s
    }

    val best = candidates.maxBy(score)
    if (score(best) > 0) best.index
    else if (candidates.length == 1) candidates.head.index
    else -1
  }
}

object DebugSessionRegistry {
  /* Held globally rather than per client, since all clients share one server and Silicon's global state. */
  @volatile private var _current: Option[ServerDebugSession] = None

  private def current: Option[ServerDebugSession] = synchronized(_current.filter(!_.isClosed))

  private def setCurrent(s: ServerDebugSession): Unit = synchronized { _current = Some(s) }

  private def takeCurrent(): Option[ServerDebugSession] = synchronized {
    val s = _current
    _current = None
    s
  }

  /** Whether any client currently has a debug session open. */
  def isAnySessionOpen: Boolean = current.isDefined

  val proverNames: Seq[String] = sil.SiliconDebugSession.proverNames
}
