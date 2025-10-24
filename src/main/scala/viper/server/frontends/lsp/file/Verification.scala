// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import ch.qos.logback.classic.Logger
import viper.server.frontends.lsp
import scala.concurrent.Future
import viper.server.core.ViperBackendConfig
import viper.server.vsi.{AstJobId, VerJobId}
import scala.concurrent.ExecutionContext
import akka.actor.Props

import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.frontends.lsp.VerificationState._

import viper.silver.verifier.AbstractError
import scala.collection.mutable.HashSet
import viper.silver.ast.AbstractSourcePosition
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp.RangePosition
import viper.silver.ast.HasLineColumn
import viper.silver.ast.LineColumnPosition

case class VerificationHandler(server: lsp.ViperServerService, logger: Logger) {
  private var waitingOn: Option[Either[AstJobId, VerJobId]] = None

  def clearWaitingOn(): Unit = {
    logger.info(s"Waiting on is: $waitingOn")
    waitingOn match {
      case Some(Left(jid)) =>
        logger.warn(s"Discarding and stopping uncompleted AST job $jid.")
        server.stopAstConstruction(jid, Some(logger))
      case _ =>
    }
    waitingOn = None
  }
  def waitOn(ast: AstJobId): Unit = {
    clearWaitingOn()
    waitingOn = Some(Left(ast))
  }
  def waitOn(ver: VerJobId): Unit = {
    // Do not `clearWaitingOn` since it contains the AST job we do not want to cancel
    waitingOn match {
      case Some(Left(jid)) =>
        logger.warn(s"Discarding uncompleted AST job $jid (though it will keep running), only keeping handle to verification job.")
        server.discardAstJobLookup(jid)
      case _ =>
    }
    waitingOn = Some(Right(ver))
  }

  def isVerifying: Boolean = waitingOn.exists(_.isRight)
  def isAstConstructing: Boolean = waitingOn.exists(_.isLeft)
  def astHandle: Option[AstJobId] = waitingOn.flatMap(_.left.toOption)
  def verHandle: Option[VerJobId] = waitingOn.flatMap(_.toOption)
}

object VerificationPhase {
  sealed trait VerificationPhase {
    val order: Int
  }
  case object ParseStart extends VerificationPhase {
    override val order = 0
  }
  case object ParseEnd extends VerificationPhase {
    override val order = 1
  }
  case object TypeckEnd extends VerificationPhase {
    override val order = 2
  }
  case object VerifyEnd extends VerificationPhase {
    override val order = 3
  }
}

trait VerificationManager extends ManagesLeaf {
  def stopRunningVerification(): Future[Boolean] = {
    stop()
      .map(_ => {
        coordinator.logger.trace(s"stopVerification has completed for ${file_uri}")
        val params = lsp.StateChangeParams(Ready.id, verificationCompleted = 0, verificationNeeded = 0, uri = file_uri)
        coordinator.sendStateChangeNotification(params, Some(this))
        true
      })
      .recover(_ => false)
  }

  implicit def ec: ExecutionContext
  var lastPhase: Option[VerificationPhase.VerificationPhase] = None

  private var futureAst: Option[Future[Unit]] = None
  private var futureCancel: Option[Future[Unit]] = None
  private var futureVer: Option[Future[Unit]] = None
  private def anyFutureRunning: Boolean =
    futureAst.exists(!_.isCompleted) ||
      futureCancel.exists(!_.isCompleted) ||
      futureVer.exists(!_.isCompleted)

  var handler: VerificationHandler = VerificationHandler(coordinator.server, coordinator.logger)
  def getInFuture[T](f: => T): Future[T] = {
    if (neverParsed) {
      runParseTypecheck(content)
    }
    // Delay the future since `futureAst` returns a future from `watchCompletion` which states:
    // "Note that this only means the elements have been passed downstream, not that downstream has successfully processed them."
    val future = futureAst.map(_.andThen(delay(10)(_))).getOrElse(Future.successful(()))
    future.map(_ => this.synchronized(f))
  }

  private val system = akka.actor.ActorSystem("delayer")
  def delay[T](ms: Int)(block: => T): Future[T] = {
    import scala.concurrent.duration._
    akka.pattern.after(ms.millis, system.scheduler) {
      Future(block)
    }
  }

  //other
  var lastSuccess: VerificationSuccess = NA
  var internalErrorMessage: String = ""

  //state specific to one verification
  var is_aborting: Boolean = false
  var is_verifying: Boolean = false
  var state: VerificationState = Stopped
  var manuallyTriggered: Boolean = _
  var neverParsed: Boolean = true

  //verification results
  var jid: Int = -1
  var timeMs: Long = 0
  var parsingCompleted: Boolean = false
  var typeCheckingCompleted: Boolean = false

  // does not correspond to `diagnostics.size` when
  // there are errors in other files
  var errorCount: Int = 0
  var diagnosticCount: Int = 0

  // verification config cache when parse-only
  var lastCustomArgs: Option[String] = None
  var lastBackendClassName: Option[String] = None

  def prepareVerification(mt: Boolean): Unit = {
    manuallyTriggered = mt

    is_verifying = true
    is_aborting = false
    state = Stopped
    neverParsed = false
    timeMs = 0
    parsingCompleted = true
    typeCheckingCompleted = true
    internalErrorMessage = ""
  }

  def stop(): Future[Unit] = {
    coordinator.logger.trace(s"stop verification of $file_uri")
    handler.verHandle match {
      case None => {
        coordinator.logger.trace(s"verification of $file_uri did not have to be stopped because there is no ongoing verification")
        Future.unit
      }
      case Some(verJob) => {
        coordinator.logger.info("Aborting running verification.")
        is_aborting = true
        val stop = coordinator.server.stopVerification(verJob, Some(coordinator.localLogger)).transform(
          _ => {
            is_verifying = false
            lastSuccess = Aborted
          },
          e => {
            coordinator.logger.debug(s"Error aborting verification of $filename: $e")
            e
          }
        )
        futureCancel = Some(stop)
        handler.clearWaitingOn()
        stop
      }
    }
  }

  /** Run parsing and typechecking but no verification */
  def runParseTypecheck(loader: FileContent): Boolean = {
    coordinator.logger.info(s"construct AST for $filename")
    if (anyFutureRunning) {
      coordinator.logger.debug(s"Already running parse/typecheck or verification")
      return false
    }
    val backend = ViperBackendConfig(lastBackendClassName.getOrElse("silicon"), lastCustomArgs.getOrElse(""))
    // Execute all handles
    startConstructAst(backend, loader, false) match {
      case None => false
      case Some(_) => {
        true
      }
    }
  }

  /** Do full parsing, type checking and verification */
  def startVerification(backendClassName: String, customArgs: String, loader: FileContent, mt: Boolean): Future[Boolean] = {
    lastBackendClassName = Some(backendClassName)
    lastCustomArgs = Some(customArgs)
    val backend = ViperBackendConfig(backendClassName, customArgs)

    coordinator.logger.info(s"verify $filename ($backendClassName $customArgs)")
    if (handler.isVerifying) stop()
    futureCancel.getOrElse(Future.unit).map(_ => {
      lastPhase = None
      handler.clearWaitingOn()
      val astJob = startConstructAst(backend, loader, mt) match {
        case None => return Future.successful(false)
        case Some(ast) => ast
      }
      val verJob = coordinator.server.verifyAst(astJob, file.toString(), backend, Some(coordinator.localLogger))
      if (verJob.id >= 0) {
        // Execute all handles
        this.resetContainers(false)
        this.resetDiagnostics(false)
        errorCount = 0
        diagnosticCount = 0
        handler.waitOn(verJob)
        val receiver = props(Some(backendClassName))
        futureVer = coordinator.server.startStreamingVer(verJob, receiver, Some(coordinator.localLogger))
        true
      } else {
        false
      }
    })
  }

  private def startConstructAst(backend: ViperBackendConfig, loader: FileContent, mt: Boolean): Option[AstJobId] = {
    coordinator.logger.debug(s"startConstructAst")
    prepareVerification(mt)

    val params = lsp.StateChangeParams(lsp.VerificationState.ConstructingAst.id, filename = filename)
    coordinator.sendStateChangeNotification(params, Some(this))

    val astJob = coordinator.server.constructAst(path.toString(), backend, Some(coordinator.localLogger), Some(loader))
    if (astJob.id >= 0) {
      this.resetDiagnostics(true)
      // Execute all handles
      val newFut = coordinator.server.startStreamingAst(astJob, props(None), Some(coordinator.localLogger))
      futureAst = newFut
      handler.waitOn(astJob)
      Some(astJob)
    } else {
      None
    }
  }

  def props(backendClassName: Option[String]): Props

  def processErrors(backendClassName: Option[String], errors: Seq[AbstractError], errorMsgPrefix: Option[String] = None): Unit = {
    val errMsgPrefixWithWhitespace = errorMsgPrefix.map(s => s"$s ").getOrElse("")
    val files = HashSet[String]()

    val diags = errors.map(err => {
      coordinator.logger.info(s"Handling error ${err.toString()}")
      var errorType: String = ""
      var phase: VerificationPhase.VerificationPhase = VerificationPhase.ParseEnd
      var severity = lsp4j.DiagnosticSeverity.Error
      if (err.fullId != null && err.fullId == "typechecker.error") {
        typeCheckingCompleted = false
        errorType = "Typechecker error"
      } else if (err.fullId != null && err.fullId == "parser.error") {
        parsingCompleted = false
        typeCheckingCompleted = false
        errorType = "Parser error"
      } else if (err.fullId != null && err.fullId == "typechecker.warning") {
        severity = lsp4j.DiagnosticSeverity.Warning
        errorType = "Typechecker warning"
      } else if (err.fullId != null && err.fullId == "parser.warning") {
        severity = lsp4j.DiagnosticSeverity.Warning
        errorType = "Parser warning"
      } else if (err.fullId != null && err.fullId == "verifier.warning") {
        severity = lsp4j.DiagnosticSeverity.Warning
        errorType = "Verification warning"
      } else {
        phase = VerificationPhase.VerifyEnd
        errorType = "Verification error"
      }

      val range = err.pos match {
        case sp: AbstractSourcePosition => lsp.Common.toRange(sp)
        case pos => {
          val start = lsp.Common.toPosition(pos)
          new Range(start, start)
        }
      }
      val rp = err.pos match {
        case sp: AbstractSourcePosition => RangePosition(sp.file, sp.start, sp.end.getOrElse(sp.start))
        case pos: HasLineColumn => RangePosition(path, pos, pos)
        case _ => RangePosition(path, LineColumnPosition(1, 1), LineColumnPosition(1, 1))
      }
      files.add(rp.file.toUri().toString())

      val errFullId = if(err.fullId != null) s"[${err.fullId}] " else ""
      val backendString = if (backendClassName.isDefined) s" [${backendClassName.get}]" else ""
      coordinator.logger.debug(s"$errorType:$backendString $errFullId" +
        s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1} $errMsgPrefixWithWhitespace${err.readableMessage}s")

      val cachFlag: String = if(err.cached) " (cached)" else ""
      val message = s"$errMsgPrefixWithWhitespace${err.readableMessage}$cachFlag"
      (phase, Diagnostic(backendClassName, rp, message, severity, err.cached, errorMsgPrefix))
    })
    diagnosticCount += errors.size
    errorCount += diags.count(_._2.severity == lsp4j.DiagnosticSeverity.Error)
    diags.groupBy(d => d._1).foreach { case (phase, diags) =>
      this.addDiagnostic(phase.order <= VerificationPhase.TypeckEnd.order)(diags.map(_._2))
    }
  }
}
