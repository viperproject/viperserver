// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import viper.server.frontends.lsp
import scala.concurrent.Future
import viper.server.vsi.AstJobId
import viper.server.vsi.VerJobId
import scala.concurrent.ExecutionContext
import akka.actor.Props

import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.frontends.lsp.VerificationState._
import akka.actor.ActorRef

import viper.silver.verifier.AbstractError
import scala.collection.mutable.HashSet
import viper.silver.ast.AbstractSourcePosition
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp.RangePosition
import viper.silver.ast.HasLineColumn
import viper.silver.ast.LineColumnPosition
import java.nio.file.Path

case class VerificationHandler(server: lsp.ViperServerService) {
  private var waitingOn: Option[Either[(AstJobId, ActorRef), VerJobId]] = None

  def clearWaitingOn(): Unit = {
    waitingOn match {
      case Some(Left((jid, actor))) => server.discardAstJobOnCompletion(jid, actor)
      case _ => {}
    }
    waitingOn = None
  }
  def waitOn(ast: (AstJobId, ActorRef)): Unit = {
    clearWaitingOn()
    waitingOn = Some(Left(ast))
  }
  def waitOn(ver: VerJobId): Unit = {
    clearWaitingOn()
    waitingOn = Some(Right(ver))
  }

  def isVerifying: Boolean = waitingOn.map(_.isRight).getOrElse(false)
  def isAstConstructing: Boolean = waitingOn.map(_.isLeft).getOrElse(false)
  def astHandle: Option[(AstJobId, ActorRef)] = waitingOn.flatMap(_.left.toOption)
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

trait VerificationManager extends Manager {
  implicit def ec: ExecutionContext
  def file_uri: String = file.file_uri
  def filename: String = file.filename
  def path: Path = file.path

  private var futureAst: Option[Future[Unit]] = None
  private var futureCancel: Option[Future[Unit]] = None
  private var futureVer: Option[Future[Unit]] = None
  private def anyFutureRunning: Boolean =
    futureAst.map(!_.isCompleted).getOrElse(false) ||
    futureCancel.map(!_.isCompleted).getOrElse(false) ||
    futureVer.map(!_.isCompleted).getOrElse(false)

  var handler: VerificationHandler = VerificationHandler(coordinator.server)
  def getInFuture[T](f: => T): Future[T] = {
    if (neverParsed) {
      runParseTypecheck(content)
    }
    futureAst.map(_.map(_ => this.synchronized(f))).getOrElse(Future.successful(this.synchronized(f)))
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
        return Future.unit
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
    // Execute all handles
    startConstructAst(loader, false) match {
      case None => false
      case Some(_) => {
        true
      }
    }
  }

  /** Do full parsing, type checking and verification */
  def startVerification(backendClassName: String, customArgs: String, loader: FileContent, mt: Boolean): Future[Boolean] = {
    coordinator.logger.info(s"verify $filename ($backendClassName)")
    if (handler.isVerifying) stop()
    futureCancel.getOrElse(Future.unit).map(_ => {
      lastPhase = None
      val (astJob, _) = handler.astHandle match {
        case None => startConstructAst(loader, mt) match {
          case None => return Future.successful(false)
          case Some(ast) => ast
        }
        case Some(ast) => ast
      }
      val command = getVerificationCommand(backendClassName, customArgs)
      coordinator.logger.debug(s"verification command: $command")
      val verJob = coordinator.server.verifyAst(astJob, command, Some(coordinator.localLogger))
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

  /** the file that should be verified has to already be part of `customArgs` */
  private def getVerificationCommand(backendClassName: String, customArgs: String): String = {
    if (backendClassName != "silicon" && backendClassName != "carbon") {
      throw new Error(s"Invalid verification backend value. " +
        s"Possible values are [silicon | carbon] " +
        s"but found $backendClassName")
    }
    s"$backendClassName $customArgs"
  }

  private def startConstructAst(loader: FileContent, mt: Boolean): Option[(AstJobId, ActorRef)] = {
    coordinator.logger.debug(s"startConstructAst")
    prepareVerification(mt)

    val params = lsp.StateChangeParams(lsp.VerificationState.ConstructingAst.id, filename = filename)
    coordinator.sendStateChangeNotification(params, Some(this))

    val astJob = coordinator.server.constructAst(path.toString(), Some(coordinator.localLogger), Some(loader))
    if (astJob.id >= 0) {
      this.resetDiagnostics(true)
      // Execute all handles
      val (newFut, newActorRef) = coordinator.server.startStreamingAst(astJob, props(None), Some(coordinator.localLogger))
      futureAst = newFut
      val ast = (astJob, newActorRef)
      handler.waitOn(ast)
      Some(ast)
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
      addDiagnostic(phase.order <= VerificationPhase.TypeckEnd.order)(diags.map(_._2))
    }
  }
}
