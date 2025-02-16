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
import viper.silver.verifier.{AbstractError, TypecheckerError, VerificationError}
import viper.silver.parser.{PFieldAccess, PKw}
import viper.silver.ast.{AbstractSourcePosition, FieldAccess, HasLineColumn, Label, LineColumnPosition, MemberWithSpec, Method, Position, Seqn, While}
import org.eclipse.lsp4j
import viper.server.frontends.lsp.{BranchFailureDetails, Common}
import viper.silver.ast.utility.lsp.{CodeAction, RangePosition, SelectionBoundScope}
import viper.silver.verifier.errors.PostconditionViolatedBranch
import viper.silver.verifier.reasons.InsufficientPermission

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

trait VerificationManager extends Manager with Branches {
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

  private def toRange(pos : Position) : lsp4j.Range = {
    pos match {
      case sp: AbstractSourcePosition => lsp.Common.toRange(sp)
      case pos =>
        val start = lsp.Common.toPosition(pos)
        new lsp4j.Range(start, start)
    }
  }

  private def toRangePosition(pos: Position) : RangePosition = {
    pos match {
      case sp: AbstractSourcePosition => RangePosition(sp.file, sp.start, sp.end.getOrElse(sp.start))
      case pos: HasLineColumn => RangePosition(path, pos, pos)
      case _ => RangePosition(path, LineColumnPosition(1, 1), LineColumnPosition(1, 1))
    }
  }

  private case object CodeActionType {
    val undeclaredField : Int = 0
    val fieldPermError : Int = 1
  }

  private val startOfFileRange =  new lsp4j.Range(new lsp4j.Position(0, 0), new lsp4j.Position(0, 0))
  private def getCodeActionsForUndeclaredFields(errors: Seq[TypecheckerError]) : Seq[CodeAction] = {
    errors.groupBy(_.node.get)
      .flatMap(fa => {
        val (faNode, faErrors) = fa
        val fieldAccess = faNode.asInstanceOf[PFieldAccess]
        val faDiags = faErrors.flatMap(e => {
          val pos = Common.toPosition(e.pos)
          getDiagnosticBy(Some(pos), None)
        })
        faErrors.map(err => {
          CodeAction("Add field declaration", s"field ${fieldAccess.idnref.pretty} : [your type]\n",
            startOfFileRange,
            SelectionBoundScope(toRangePosition(err.pos)),
            lsp4j.CodeActionKind.QuickFix,
            faDiags)
        })
      }).toSeq
  }
  private def getCodeActionsForFieldPermissionError(errors: Seq[VerificationError]) : Seq[CodeAction] = {
    errors.groupBy(_.reason.offendingNode.toString()).flatMap(fa => {
      val (faStr, faErrors) = fa
      val faDiags = faErrors.flatMap(err => {
        val pos = Common.toPosition(err.pos)
        getDiagnosticBy(Some(pos), None)
      })
      faErrors.flatMap(err => {
        val f = err.reason.offendingNode
        val seqn = f.getAncestor[Seqn] // TODO Stephanie check
        var parentWithSpec : Option[MemberWithSpec] = None
        if (seqn.isDefined) {
          parentWithSpec = seqn.get.getAncestor[MemberWithSpec]
        }
        parentWithSpec.collect({
          case l : Label => (l,PKw.Invariant.keyword)
          case w : While => (w,PKw.Invariant.keyword)
          case m : Method => (m,PKw.Requires.keyword)
        }).map( t => {
          val (parent, keyword) = t
          val parentPosFile = Common.toPosition(parent.pos)
          val m = getInProject(this.file_uri).content
          val closingBracketPos = m.iterForward(parentPosFile)
            .find { case (c, _) => c == '{'}
            .map(t => {val p = t._2; p.setCharacter(p.getCharacter-1); p})
          val closingBracketInSameLine = closingBracketPos
            .map(p => p.getLine == parentPosFile.getLine)
            .getOrElse(false)
          val editPos = if (closingBracketInSameLine) closingBracketPos.get else
              {val p = Common.toPosition(parent.pos);p.setLine(p.getLine+1);p}
          val parentRangePosition = toRangePosition(parent.pos)
          val indent = " "*(parentRangePosition.start.column-1)
          val beforeKeyword = if (closingBracketInSameLine) s"\n$indent  " else "  "
          CodeAction("Add access precondition", s"$beforeKeyword$keyword acc($faStr)\n$indent",
            new lsp4j.Range(editPos, editPos),
            SelectionBoundScope(toRangePosition(err.pos)),
            lsp4j.CodeActionKind.QuickFix,
            faDiags)
        }).toSeq
      })
    }).toSeq
  }

  private def addCodeActions(errors : Seq[AbstractError]) = {
    errors.groupBy({
      case e: TypecheckerError if e.node.isDefined
        && e.node.get.isInstanceOf[PFieldAccess] => CodeActionType.undeclaredField
      case e : VerificationError if e.reason.isInstanceOf[InsufficientPermission]
        && e.reason.offendingNode.isInstanceOf[FieldAccess] => CodeActionType.fieldPermError
      case _ => -1
    }).filter(t => t._1 >= 0)
    .map(e => {
      val (caType, filteredErrors) = e
      caType match {
        case CodeActionType.undeclaredField =>
          val filteredErrors_ = filteredErrors.asInstanceOf[Seq[TypecheckerError]]
          this.addCodeAction(first = true)(getCodeActionsForUndeclaredFields(filteredErrors_))
        case CodeActionType.fieldPermError =>
          val filteredErrors_ = filteredErrors.asInstanceOf[Seq[VerificationError]]
          this.addCodeAction(first = false)(getCodeActionsForFieldPermissionError(filteredErrors_))
      }
    })
  }

  def processErrors(backendClassName: Option[String], errors: Seq[AbstractError], errorMsgPrefix: Option[String] = None): Unit = {
    // Add diagnostics
    val errMsgPrefixWithWhitespace = errorMsgPrefix.map(s => s"$s ").getOrElse("")
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

      val range = toRange(err.pos)
      val rp = toRangePosition(err.pos)

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

    // Add code actions
    addCodeActions(errors)

    // Support for red beams indicating branch failure
    val branchFailureDetails = errors.collect({case err: PostconditionViolatedBranch =>
      BranchFailureDetails(err.readableMessage, getBranchRange(this.file_uri, lsp.Common.toPosition(err.pos), err.leftIsFatal, err.rightIsFatal))
    })
    if (branchFailureDetails.nonEmpty) {
      val params = lsp.StateChangeParams(
        VerificationRunning.id,
        uri=this.file_uri,
        branchFailureDetails = branchFailureDetails.toArray
      )
      coordinator.sendStateChangeNotification(params, Some(this))
    }
  }
}
