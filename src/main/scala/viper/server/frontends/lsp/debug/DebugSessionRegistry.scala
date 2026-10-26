// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug

import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.ClientCoordinator
import viper.silicon.{debugger => sil}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future

/**
  * What a debug session is started for.
  *
  * @param uri                the file to debug.
  * @param position           the (0-based) position of the verification error the user asked about, if any.
  * @param message            the message of that error, if any; used to tell apart errors at the same position.
  * @param restrictToMember   whether to verify only the member containing `position`.
  * @param withCounterexample whether the debug run should compute counterexamples.
  */
case class DebugLaunchRequest(uri: String,
                              backend: String,
                              customArgs: String,
                              position: Option[(Int, Int)],
                              message: Option[String],
                              restrictToMember: Boolean,
                              withCounterexample: Boolean)

/**
  * A debug session that has been started, together with its failures.
  *
  * @param selected the index of the failure the user asked about, or -1 if it is not clear which one that is.
  */
case class StartedDebugSession(session: ServerDebugSession, failures: Seq[sil.DebugFailureInfo], selected: Int)

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

  /** The debug run that is currently being verified, if it can be paused: the file and its controller. */
  @volatile private var liveRun: Option[(String, sil.DebugRunController)] = None

  /**
    * Starts a debug session, replacing any session that is still open.
    *
    * @param onProgress receives human-readable progress messages while the debug run is set up.
    * @param controller pauses the debug run at breakpoints, if given. The returned future only completes once
    *                   the verification has finished, i.e. after all pauses.
    * @return the session, or why none could be started.
    */
  def start(request: DebugLaunchRequest, onProgress: String => Unit,
            controller: Option[sil.DebugRunController] = None): Future[Either[String, StartedDebugSession]] = {
    if (request.backend != null && request.backend.toLowerCase != "silicon") {
      return Future.successful(Left(s"Debugging is only supported for Silicon, not for '${request.backend}'."))
    }
    val path = lsp.Common.uriToPath(lsp.Common.uriFromString(request.uri)).toString

    _launchInProgress = true
    closeAll("a new debug session is being started").flatMap { _ =>
      // A verification that is still running would create another Silicon verifier, and with it overwrite the
      // global configuration the debug run depends on: the debug run would then produce failures without any
      // debug information. `launchInProgress` keeps further verifications from starting in the meantime.
      coordinator.stopAllRunningVerifications().recover { case _ => () }
    }.flatMap { _ =>
      val sessionId = s"debug-${counter.incrementAndGet()}"
      onProgress("Starting the verification debugger...")
      liveRun = controller.map(c => (request.uri, c))

      val selectAt =
        if (request.restrictToMember) request.position.map { case (line, char) => (line + 1, char + 1) }
        else None

      coordinator.server.startDebugVerification(
        file = path,
        customArgs = Option(request.customArgs).getOrElse(""),
        selectMemberAt = selectAt,
        withCounterexample = request.withCounterexample,
        loader = Some(coordinator.fileContent(request.uri)),
        onProgress = onProgress,
        localLogger = Some(coordinator.localLogger),
        debugHook = controller
      ).map { res =>
        liveRun = None
        res
      }.map {
        case Left(err) =>
          _launchInProgress = false
          Left(err)

        case Right((silicon, inner)) =>
          val session = new ServerDebugSession(sessionId, request.uri, Some(silicon), inner)
          DebugSessionRegistry.setCurrent(session)
          _launchInProgress = false
          val failures = inner.failures
          Right(StartedDebugSession(session, failures, DebugSessionRegistry.selectFailure(failures, request.position, request.message)))
      }
    }.recover { case e: Throwable =>
      // Whatever goes wrong, the launch must not stay "in progress", or verifications would stay blocked.
      _launchInProgress = false
      liveRun = None
      coordinator.logger.error(s"Starting a debug session failed: $e")
      Left(s"Starting a debug session failed: ${e.getMessage}")
    }
  }

  /**
    * Closes the current session, if any, and aborts a debug run that is still being verified. Whoever shows the
    * session is told via [[ServerDebugSession.onClose]]; whoever runs the debug run sees its future complete.
    */
  def closeAll(reason: String): Future[Unit] = {
    liveRun.foreach { case (_, c) => c.abort(reason) }
    DebugSessionRegistry.takeCurrent() match {
      case None => Future.unit
      case Some(session) =>
        coordinator.logger.info(s"Closing debug session ${session.sessionId}: $reason")
        session.close(reason)
    }
  }

  /** Closes the session if it belongs to the given file; used when the file changes or is closed. */
  def closeIfFor(uri: String, reason: String): Future[Unit] =
    if (current.exists(_.uri == uri) || liveRun.exists(_._1 == uri)) closeAll(reason) else Future.unit
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

  /**
    * Finds the failure that corresponds to the diagnostic the user clicked on. The debug run is a second
    * verification, so failures may be ordered differently than in the run that produced the diagnostics;
    * we therefore match on position and message rather than on an index.
    *
    * @param pos a 0-based position, as the IDE reports it; failure positions are 1-based.
    * @return the index of the failure, or -1 if it is ambiguous which one is meant.
    */
  def selectFailure(failures: Seq[sil.DebugFailureInfo], pos: Option[(Int, Int)], message: Option[String]): Int = {
    val candidates = failures.filter(_.debuggable)
    if (candidates.isEmpty) return -1

    def score(f: sil.DebugFailureInfo): Int = {
      var s = 0
      for ((line, char) <- pos; fp <- f.pos) {
        if (fp.startLine - 1 == line) s += 4
        if (fp.startColumn - 1 == char) s += 2
      }
      for (m <- message if m.nonEmpty && (m.contains(f.message) || f.message.contains(m))) s += 1
      s
    }

    val best = candidates.maxBy(score)
    if (score(best) > 0) best.index
    else if (candidates.length == 1) candidates.head.index
    else -1
  }
}
