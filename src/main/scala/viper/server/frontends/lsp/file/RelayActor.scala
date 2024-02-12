// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import akka.actor.{Actor, Props, Status}
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.silver.ast
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError, ErrorMessage}

import viper.server.frontends.lsp.file.ProgressCoordinator
import viper.silver.parser._

trait MessageHandler extends ProjectManager with VerificationManager with QuantifierCodeLens with QuantifierInlayHints with SignatureHelp {
  override def props(backendClassName: Option[String]): Props = RelayActor.props(this, backendClassName)

  // var filesInProject: Set[String] = Set.empty
  var progress: ProgressCoordinator = null

  private def determineSuccess(code: Int): VerificationSuccess = {
    if (code != 0) {
      Error
    } else if (!parsingCompleted) {
      ParsingFailed
    } else if (!typeCheckingCompleted) {
      TypecheckingFailed
    } else if (is_aborting) {
      Aborted
    } else if (errorCount != 0) {
      VerificationFailed
    } else {
      lsp.VerificationSuccess.Success
    }
  }

  def completionHandler(code: Int): Unit = {
    try {
      coordinator.logger.debug(s"completionHandler is called with code $code")
      // handleFinish()

      var params: lsp.StateChangeParams = null
      var success = NA
      val mt = if (this.manuallyTriggered) 1 else 0

      val isVerifyingStage = true

      // do we need to start a followUp Stage?
      if (isVerifyingStage) {
        // inform client about postProcessing
        success = determineSuccess(code)
        params = lsp.StateChangeParams(PostProcessing.id, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(this))

        // notify client about outcome of verification
        params = lsp.StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 1, uri = file_uri,
          error = "")
        coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          coordinator.logger.debug(s"Verification Backend Terminated Abnormally: with code $code")
        }
      } else {
        success = if (is_aborting) Aborted else Success
        params = lsp.StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 0, uri = file_uri,
          error = "")
        coordinator.sendStateChangeNotification(params, Some(this))
      }

      // reset for next verification
      lastSuccess = success
      // onVerifyEnd.foreach(_(lastSuccess))
      timeMs = 0
    } catch {
      case e: Throwable =>
        // is_verifying = false
        coordinator.client.notifyVerificationNotStarted(lsp.VerificationNotStartedParams(file_uri))
        coordinator.logger.debug(s"Error handling verification completion: $e")
    }
  }
}

object RelayActor {
  def props(task: MessageHandler, backendClassName: Option[String]): Props = Props(new RelayActor(task, backendClassName))

  case class GetReportedErrors()
}

class RelayActor(task: MessageHandler, backendClassName: Option[String]) extends Actor {
  val coordinator = task.coordinator

  /**
    * errors together with the offending node's position (only for errors that have an offending node (i.e. implement
    * `ErrorMessage`). Storing the position along with the error fixes the issue that two errors with offending nodes
    * at different position just get dropped.
    */
  var reportedErrors: Set[(AbstractError, Option[ast.Position])] = Set.empty
  /** helper function to add an error to `reportedErrors` */
  private def markErrorAsReported(err: AbstractError): Unit = err match {
    case err: ErrorMessage => reportedErrors = reportedErrors + ((err, Some(err.offendingNode.pos)))
    case _ => reportedErrors = reportedErrors + ((err, None))
  }
  /** helper function to add errors to `reportedErrors` */
  private def markErrorsAsReported(errs: Seq[AbstractError]): Unit = errs.foreach(markErrorAsReported)
  /** helper function to check whether an error is contained in `reportedErrors` */
  private def hasAlreadyBeenReported(err: AbstractError): Boolean = err match {
    case err: ErrorMessage => reportedErrors.contains((err, Some(err.offendingNode.pos)))
    case _ => reportedErrors.contains((err, None))
  }

  override def receive: PartialFunction[Any, Unit] = {
    case m if task.is_aborting =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] ignoring message because we are aborting: $m")

    case PProgramReport(typeckSuccess, pProgram) =>
      // New project
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] got new pProgram for ${task.filename}")
      val files = pProgram.imports.flatMap(_.resolved).map(_.toUri().toString()).toSet
      // coordinator.logger.debug(s"pProgram.imports ${pProgram.imports.toString()}")
      task.setupProject(files)
      val parseSuccess = pProgram.errors.isEmpty
      val phase = if (typeckSuccess) VerificationPhase.TypeckEnd
        else if (parseSuccess) VerificationPhase.ParseEnd
        else VerificationPhase.ParseStart
      if (typeckSuccess || task.lastPhase.forall(_.order <= phase.order)) {
        task.lastPhase match {
          case Some(VerificationPhase.VerifyEnd) | Some(VerificationPhase.TypeckEnd) =>
            task.resetContainers(false)
          case Some(VerificationPhase.ParseEnd) | Some(VerificationPhase.ParseStart) =>
            task.resetContainers(true)
          case None => {
            task.resetContainers(true)
            task.resetContainers(false)
          }
        }
        task.lastPhase = Some(phase)
        val first = !typeckSuccess

        task.addCodeLens(first)(HasCodeLens(pProgram))
        task.addDocumentSymbol(first)(HasDocumentSymbol(pProgram))
        task.addHoverHint(first)(HasHoverHints(pProgram))
        task.addGotoDefinition(first)(HasGotoDefinitions(pProgram))
        task.addFindReferences(first)(HasReferenceTos(pProgram))
        task.addFoldingRange(first)(HasFoldingRanges(pProgram))
        task.addInlayHint(first)(HasInlayHints(pProgram))
        task.addSemanticHighlight(first)(HasSemanticHighlights(pProgram))
        task.addSignatureHelp(first)(HasSignatureHelps(pProgram))
        task.addSuggestionScopeRange(first)(HasSuggestionScopeRanges(pProgram))
        task.addCompletionProposal(first)(HasCompletionProposals(pProgram))
      }
    case StatisticsReport(m, f, p, _, _) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] StatisticsReport")
      task.progress = new ProgressCoordinator(coordinator, p, f, m)
      val params = lsp.StateChangeParams(VerificationRunning.id, progress = 0, filename = task.filename)
      coordinator.sendStateChangeNotification(params, Some(task))
    case AstConstructionFailureMessage(_, res) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] AstConstructionFailureMessage")
      markErrorsAsReported(res.errors)
      task.processErrors(backendClassName, res.errors, Some("Constructing the AST has failed:"))
    case ExceptionReport(e) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] ExceptionReport (${e.toString()}, ${e.getStackTrace().toString()})")
      task.processErrors(backendClassName, Seq(AbortedExceptionally(e)))
    case InvalidArgumentsReport(_, errors) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] InvalidArgumentsReport")
      markErrorsAsReported(errors)
      task.processErrors(backendClassName, errors, Some(s"Invalid arguments have been passed to the backend $backendClassName:"))
    case EntitySuccessMessage(_, concerning, _, _) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] EntitySuccessMessage")
      if (task.progress == null) {
        coordinator.logger.debug("The backend must send a VerificationStart message before the ...Verified message.")
      } else {
        val output = lsp.BackendOutput(lsp.BackendOutputType.FunctionVerified, name = concerning.name)
        task.progress.updateProgress(output)
        val progressPercentage = task.progress.toPercent
        val params = lsp.StateChangeParams(VerificationRunning.id, progress = progressPercentage, filename = task.filename)
        coordinator.sendStateChangeNotification(params, Some(task))
      }
    case EntityFailureMessage(_, _, _, res, _) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] EntityFailureMessage")
      println("EF Errors: " + res.errors.toString())
      markErrorsAsReported(res.errors)
      task.processErrors(backendClassName, res.errors)
    case OverallSuccessMessage(_, verificationTime) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] OverallSuccessMessage")
      task.state = VerificationReporting
      task.timeMs = verificationTime
      // the completion handler is not yet invoked (but as part of Status.Success)
    case OverallFailureMessage(_, verificationTime, failure) =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] OverallFailureMessage")
      task.state = VerificationReporting
      task.timeMs = verificationTime
      // we filter `failure.errors` such that only new errors are reported
      // this is important since Silicon provides errors as part of EntityFailureMessages and the OverallFailureMessage
      // where else Carbon does not produce EntityFailureMessages (except for cached members in which case
      // ViperServer produces EntitySuccessMessage and EntityFailureMessages)
      println("N Errors: " + failure.errors.toString())
      val newErrors = failure.errors.filterNot(hasAlreadyBeenReported)
      markErrorsAsReported(newErrors)
      task.processErrors(backendClassName, newErrors)
    case WarningsDuringParsing(warnings) =>
      markErrorsAsReported(warnings)
      task.processErrors(backendClassName, warnings)
    case WarningsDuringTypechecking(warnings) =>
      markErrorsAsReported(warnings)
      task.processErrors(backendClassName, warnings)
    case WarningsDuringVerification(warnings) =>
      markErrorsAsReported(warnings)
      task.processErrors(backendClassName, warnings)
    // the completion handler is not yet invoked (but as part of Status.Success)
    case QuantifierChosenTriggersMessage(qexp, triggers, oldTriggers) =>
      coordinator.logger.trace(s"[receive@${task.filename}/${backendClassName.isDefined}] QuantifierChosenTriggersMessage")
      task.handleQuantifierChosenTriggers(qexp, triggers, oldTriggers)
    case QuantifierInstantiationsMessage(quantifier, instantiations, magGen, maxCost) =>
      coordinator.logger.trace(s"[receive@${task.filename}/${backendClassName.isDefined}] QuantifierInstantiationsMessage")
      task.handleQuantifierInstantiations(quantifier, instantiations, magGen, maxCost)
    case m: Message =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] Message")
      coordinator.client.notifyUnhandledViperServerMessage(lsp.UnhandledViperServerMessageTypeParams(m.name, m.toString, lsp.LogLevel.Info.id))
    case Status.Success =>
      coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] Status.Success")
      // Success is sent when the stream is completed
      if (backendClassName.isDefined) task.completionHandler(0)
    case Status.Failure(cause) =>
      coordinator.logger.info(s"[receive@${task.filename}/${backendClassName.isDefined}] Streaming messages has failed in RelayActor with cause $cause")
      if (backendClassName.isDefined) task.completionHandler(-1) // no success
    case RelayActor.GetReportedErrors() => sender() ! reportedErrors.toSeq.map(_._1)
    case e: Throwable => coordinator.logger.debug(s"[receive@${task.filename}/${backendClassName.isDefined}] RelayActor received throwable: $e")
  }
}
