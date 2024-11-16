// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentSymbol, Position, PublishDiagnosticsParams, Range, SymbolKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast
import viper.silver.ast.{AbstractSourcePosition, Domain, Field, Function, HasLineColumn, Method, Predicate, SourcePosition}
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError, ErrorMessage}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future


class FileManager(coordinator: ClientCoordinator, file_uri: String)(implicit executor: VerificationExecutionContext) {
  // File information
  var uri: URI = URI.create(file_uri)
  var path: Path = Paths.get(uri)
  var filename: String = path.getFileName.toString
  var lastSuccess: VerificationSuccess = NA
  var internalErrorMessage: String = ""

  //state specific to one verification
  var is_aborting: Boolean = false
  var is_verifying: Boolean = false
  var global_failure: Boolean = false
  var state: VerificationState = Stopped
  var manuallyTriggered: Boolean = _

  //verification results
  var jid: Int = -1
  var timeMs: Long = 0
  var diagnostics: ArrayBuffer[Diagnostic] = ArrayBuffer.empty
  var parsingCompleted: Boolean = false
  var typeCheckingCompleted: Boolean = false
  var progress: ProgressCoordinator = _
  var symbolInformation: ArrayBuffer[DocumentSymbol] = ArrayBuffer()
  var definitions: ArrayBuffer[lsp.Definition] = ArrayBuffer()

  private var partialData: String = ""

  def resetDiagnostics(): Unit = {
    diagnostics = ArrayBuffer.empty
    val diagnosticParams = new PublishDiagnosticsParams()
    diagnosticParams.setUri(file_uri)
    diagnosticParams.setDiagnostics(diagnostics.asJava)
    coordinator.client.publishDiagnostics(diagnosticParams)
  }

  def resetLastSuccess(): Unit = {
    lastSuccess = NA
  }

  def prepareVerification(): Unit = {
    is_verifying = true
    is_aborting = false
    state = Stopped
    if (partialData.nonEmpty) {
      coordinator.logger.debug(s"Some unparsed output has been detected: $partialData")
      partialData = ""
    }
    timeMs = 0
    resetDiagnostics()
    parsingCompleted = true
    typeCheckingCompleted = true
    internalErrorMessage = ""
  }

  def stopVerification(): Future[Unit] = {
    coordinator.logger.trace(s"stop verification of $file_uri")
    if (!is_verifying) {
      coordinator.logger.trace(s"verification of $file_uri did not have to be stopped because there is no ongoing verification")
      return Future.unit
    }
    coordinator.logger.info("Aborting running verification.")
    is_aborting = true
    coordinator.server.stopVerification(VerJobId(jid), Some(coordinator.localLogger)).transform(
      _ => {
        is_verifying = false
        lastSuccess = Aborted
      },
      e => {
        coordinator.logger.debug(s"Error aborting verification of $filename: $e")
        e
      }
    )
  }

  /** the file that should be verified has to already be part of `customArgs` */
  def getVerificationCommand(backendClassName: String, customArgs: String): String = {
    s"$backendClassName $customArgs"
  }

  def reformatFile(): Option[String] = {
    coordinator.logger.info(s"reformatting the file $filename")
    coordinator.server.reformatFile(path.toString, Some(coordinator.localLogger))
  }

  def startVerification(backendClassName: String, customArgs: String, manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.manuallyTriggered = manuallyTriggered

    coordinator.logger.info(s"verify $filename using $backendClassName")

    val params = StateChangeParams(VerificationRunning.id, filename = filename)
    coordinator.sendStateChangeNotification(params, Some(this))

    val command = getVerificationCommand(backendClassName, customArgs)
    coordinator.logger.debug(s"verification command: $command")
    val handle = coordinator.server.verifyWithCommand(command, Some(coordinator.localLogger))
    jid = handle.id
    if (jid >= 0) {
      coordinator.server.startStreaming(handle, RelayActor.props(this, backendClassName), Some(coordinator.localLogger))
      true
    } else {
      false
    }
  }

  object RelayActor {
    def props(task: FileManager, backendClassName: String): Props = Props(new RelayActor(task, backendClassName))

    case class GetReportedErrors()
  }

  class RelayActor(task: FileManager, backendClassName: String) extends Actor {

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
      case m if is_aborting =>
        coordinator.logger.debug(s"ignoring message because we are aborting: $m")

      case ProgramOutlineReport(members) =>
        symbolInformation = ArrayBuffer()
        members.foreach(m => {
          val member_start = m.pos.asInstanceOf[SourcePosition].start
          val member_end = m.pos.asInstanceOf[SourcePosition].end.getOrElse(member_start)
          val range_start = new Position(member_start.line - 1, member_start.column - 1)
          val range_end = new Position(member_end.line - 1, member_end.column - 1)
          val range = new Range(range_start, range_end)

          val kind = m match {
            case _: Method => SymbolKind.Method
            case _: Function => SymbolKind.Function
            case _: Predicate => SymbolKind.Interface
            case _: Field => SymbolKind.Field
            case _: Domain => SymbolKind.Class
            case _ => SymbolKind.Enum
          }
          // for now, we use `range` as range & selectionRange. The latter one is supposed to be only a sub-range
          // that should be selected when the user selects the symbol.
          val info = new DocumentSymbol(m.name, kind, range, range)
          symbolInformation.append(info)
        })
      case ProgramDefinitionsReport(defs) =>
        definitions = ArrayBuffer()
        defs.foreach(d => {
          val start = d.scope match {
            case Some(s) => new Position(s.start.line - 1, s.start.column - 1)
            case _ => null
          }
          val end = d.scope match {
            case Some(s) if s.end.isDefined => new Position(s.end.get.line - 1, s.end.get.column - 1)
            case _ => null
          }
          val range: Range = if(start != null && end != null) {
            new Range(start, end)
          } else {
            null
          }
          val sourcePos = d.location.asInstanceOf[viper.silver.ast.SourcePosition]
          val location: Position = new Position(sourcePos.start.line - 1, sourcePos.start.column - 1)
          val definition: lsp.Definition = lsp.Definition(d.typ, d.name, location, range)
          definitions.append(definition)
        })
      case StatisticsReport(m, f, p, _, _) =>
        progress = new ProgressCoordinator(coordinator, p, f, m)
        val params = StateChangeParams(VerificationRunning.id, progress = 0, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(task))
      case AstConstructionFailureMessage(_, res) =>
        markErrorsAsReported(res.errors)
        processErrors(backendClassName, res.errors, Some("Constructing the AST has failed:"))
      case ExceptionReport(e) =>
        processErrors(backendClassName, Seq(AbortedExceptionally(e)))
      case InvalidArgumentsReport(_, errors) =>
        markErrorsAsReported(errors)
        processErrors(backendClassName, errors, Some(s"Invalid arguments have been passed to the backend $backendClassName:"))
      case WarningsDuringParsing(warnings) =>
        markErrorsAsReported(warnings)
        processErrors(backendClassName, warnings)
      case WarningsDuringTypechecking(warnings) =>
        markErrorsAsReported(warnings)
        processErrors(backendClassName, warnings)
      case WarningsDuringVerification(warnings) =>
        markErrorsAsReported(warnings)
        processErrors(backendClassName, warnings)
      case EntitySuccessMessage(_, concerning, _, _) =>
        if (progress == null) {
          coordinator.logger.debug("The backend must send a VerificationStart message before the ...Verified message.")
        } else {
          val output = BackendOutput(BackendOutputType.FunctionVerified, name = concerning.name)
          progress.updateProgress(output)
          val progressPercentage = progress.toPercent
          val params = StateChangeParams(VerificationRunning.id, progress = progressPercentage, filename = filename)
          coordinator.sendStateChangeNotification(params, Some(task))
        }
      case EntityFailureMessage(_, _, _, res, _) =>
        markErrorsAsReported(res.errors)
        processErrors(backendClassName, res.errors)
      case OverallSuccessMessage(_, verificationTime) =>
        state = VerificationReporting
        timeMs = verificationTime
        // the completion handler is not yet invoked (but as part of Status.Success)
      case OverallFailureMessage(_, verificationTime, failure) =>
        state = VerificationReporting
        timeMs = verificationTime
        // we filter `failure.errors` such that only new errors are reported
        // this is important since Silicon provides errors as part of EntityFailureMessages and the OverallFailureMessage
        // where else Carbon does not produce EntityFailureMessages (except for cached members in which case
        // ViperServer produces EntitySuccessMessage and EntityFailureMessages)
        val newErrors = failure.errors.filterNot(hasAlreadyBeenReported)
        markErrorsAsReported(newErrors)
        processErrors(backendClassName, newErrors)
        // the completion handler is not yet invoked (but as part of Status.Success)
      case m: Message => coordinator.client.notifyUnhandledViperServerMessage(UnhandledViperServerMessageTypeParams(m.name, m.toString, LogLevel.Info.id))
      case Status.Success =>
        // Success is sent when the stream is completed
        completionHandler(0)
      case Status.Failure(cause) =>
        coordinator.logger.info(s"Streaming messages has failed in RelayActor with cause $cause")
        completionHandler(-1) // no success
      case RelayActor.GetReportedErrors() => sender() ! reportedErrors.toSeq.map(_._1)
      case e: Throwable => coordinator.logger.debug(s"RelayActor received throwable: $e")
    }

    private def processErrors(backendClassName: String, errors: Seq[AbstractError], errorMsgPrefix: Option[String] = None): Unit = {
      val diags = errors.map(err => {
        var errorType: String = ""
        var severity = DiagnosticSeverity.Error
        if (err.fullId != null && err.fullId == "typechecker.error") {
          typeCheckingCompleted = false
          errorType = "Typechecker error"
        } else if (err.fullId != null && err.fullId == "parser.error") {
          parsingCompleted = false
          typeCheckingCompleted = false
          errorType = "Parser error"
        } else if (err.fullId != null && err.fullId == "typechecker.warning") {
          severity = DiagnosticSeverity.Warning
          errorType = "Typechecker warning"
        } else if (err.fullId != null && err.fullId == "parser.warning") {
          severity = DiagnosticSeverity.Warning
          errorType = "Parser warning"
        } else if (err.fullId != null && err.fullId == "verifier.warning") {
          severity = DiagnosticSeverity.Warning
          errorType = "Verification warning"
        } else {
          errorType = "Verification error"
        }

        val range = {
          val startPos = err.pos match {
            case p: HasLineColumn => new Position(p.line - 1, p.column - 1) // in case of SourcePosition, this indeed corresponds to the start position
            case _ => new Position(0, 0)
          }
          val endPos = err.pos match {
            // returning `startPos` instead of (0, 0) ensures that we do not create an end position that occurs before the start position
            case sp: AbstractSourcePosition => sp.end.map(end => new Position(end.line - 1, end.column - 1)).getOrElse(startPos)
            case _ => startPos
          }
          new Range(startPos, endPos)
        }

        val errMsgPrefixWithWhitespace = errorMsgPrefix.map(s => s"$s ").getOrElse("")
        coordinator.logger.debug(s"$errorType: [$backendClassName] " +
          s"${if(err.fullId != null) "[" + err.fullId + "] " else ""}" +
          s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1} $errMsgPrefixWithWhitespace${err.readableMessage}s")

        val cachFlag: String = if(err.cached) "(cached)" else ""
        new Diagnostic(range, errMsgPrefixWithWhitespace + err.readableMessage + cachFlag, severity, "")
      })

      diagnostics.appendAll(diags)
      val params = StateChangeParams(
        VerificationRunning.id, filename = filename,
        uri = file_uri, diagnostics = diagnostics.toArray)
      coordinator.sendStateChangeNotification(params, Some(task))
    }
  }

  private def determineSuccess(code: Int): VerificationSuccess = {
    if (code != 0) {
      Error
    } else if (!parsingCompleted) {
      ParsingFailed
    } else if (!typeCheckingCompleted) {
      TypecheckingFailed
    } else if (is_aborting) {
      Aborted
    } else if (diagnostics.nonEmpty) {
      if (diagnostics.exists(d => d.getSeverity == DiagnosticSeverity.Error)) {
        VerificationFailed
      } else {
        VerificationSuccess.Success
      }
    } else {
      VerificationSuccess.Success
    }
  }

  private def completionHandler(code: Int): Unit = {
    is_verifying = false
    try {
      coordinator.logger.debug(s"completionHandler is called with code $code")

      var params: StateChangeParams = null
      var success = NA
      val mt = if (this.manuallyTriggered) 1 else 0

      val isVerifyingStage = true

      // do we need to start a followUp Stage?
      if (isVerifyingStage) {
        if (partialData != null && partialData.nonEmpty) {
          coordinator.logger.debug(s"Some unparsed output was detected: $partialData")
          partialData = ""
        }

        // inform client about postProcessing
        success = determineSuccess(code)
        params = StateChangeParams(PostProcessing.id, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(this))

        // notify client about outcome of verification
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 1, uri = file_uri,
          error = internalErrorMessage, diagnostics = diagnostics.toArray)
        coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          coordinator.logger.debug(s"Verification Backend Terminated Abnormally: with code $code")
        }
      } else {
        success = if (is_aborting) Aborted else Success
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 0, uri = file_uri,
          error = internalErrorMessage, diagnostics = diagnostics.toArray)
        coordinator.sendStateChangeNotification(params, Some(this))
      }

      // reset for next verification
      lastSuccess = success
      timeMs = 0
    } catch {
      case e: Throwable =>
        is_verifying = false
        coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(file_uri))
        coordinator.logger.debug(s"Error handling verification completion: $e")
    }
  }
}
