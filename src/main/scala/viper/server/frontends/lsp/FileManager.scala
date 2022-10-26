// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position, PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast.{AbstractSourcePosition, Domain, Field, Function, HasLineColumn, Method, Predicate, SourcePosition}
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError}

import scala.collection.mutable
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
  var symbolInformation: ArrayBuffer[SymbolInformation] = ArrayBuffer()
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
    if (backendClassName != "silicon" && backendClassName != "carbon") {
      throw new Error(s"Invalid verification backend value. " +
        s"Possible values are [silicon | carbon] " +
        s"but found $backendClassName")
    }
    s"$backendClassName $customArgs"
  }

  def startVerification(backendClassName: String, customArgs: String, manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.manuallyTriggered = manuallyTriggered

    coordinator.logger.info(s"verify $filename")

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
      * errors that have already been reported to the client; This is used to identify errors in OverallFailureMessage
      * that have not been reported yet.
      */
    val reportedErrors: mutable.Set[AbstractError] = mutable.Set.empty

    override def receive: PartialFunction[Any, Unit] = {
      case ProgramOutlineReport(members) =>
        symbolInformation = ArrayBuffer()
        members.foreach(m => {
          val member_start = m.pos.asInstanceOf[SourcePosition].start
          val member_end = m.pos.asInstanceOf[SourcePosition].end.getOrElse(member_start)
          val range_start = new Position(member_start.line - 1, member_start.column - 1)
          val range_end = new Position(member_end.line - 1, member_end.column - 1)
          val range = new Range(range_start, range_end)
          val location: Location = new Location(file_uri, range)

          val kind = m match {
            case _: Method => SymbolKind.Method
            case _: Function => SymbolKind.Function
            case _: Predicate => SymbolKind.Interface
            case _: Field => SymbolKind.Field
            case _: Domain => SymbolKind.Class
            case _ => SymbolKind.Enum
          }
          val info: SymbolInformation = new SymbolInformation(m.name, kind, location)
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
        reportedErrors.addAll(res.errors)
        processErrors(backendClassName, res.errors, Some("Constructing the AST has failed:"))
      case ExceptionReport(e) =>
        processErrors(backendClassName, Seq(AbortedExceptionally(e)))
      case InvalidArgumentsReport(_, errors) =>
        reportedErrors.addAll(errors)
        processErrors(backendClassName, errors, Some(s"Invalid arguments have been passed to the backend $backendClassName:"))
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
        reportedErrors.addAll(res.errors)
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
        val newErrors = failure.errors.filter(err => reportedErrors.add(err)) // add returns true if `err` is not yet in the set
        processErrors(backendClassName, newErrors)
      // the completion handler is not yet invoked (but as part of Status.Success)
      case m: Message => coordinator.client.notifyUnhandledViperServerMessage(UnhandledViperServerMessageTypeParams(m.name, m.toString, LogLevel.Info.id))
      case Status.Success =>
        // Success is sent when the stream is completed
        completionHandler(0)
      case Status.Failure(cause) =>
        coordinator.logger.info(s"Streaming messages has failed in RelayActor with cause $cause")
        completionHandler(-1) // no success
      case RelayActor.GetReportedErrors() => sender() ! reportedErrors
      case e: Throwable => coordinator.logger.debug(s"RelayActor received throwable: $e")
    }

    private def processErrors(backendClassName: String, errors: Seq[AbstractError], errorMsgPrefix: Option[String] = None): Unit = {
      errors.foreach(err => {
        var errorType: String = ""
        if (err.fullId != null && err.fullId == "typechecker.error") {
          typeCheckingCompleted = false
          errorType = "Typechecker error"
        } else if (err.fullId != null && err.fullId == "parser.error") {
          parsingCompleted = false
          typeCheckingCompleted = false
          errorType = "Parser error"
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
        val diag = new Diagnostic(range, errMsgPrefixWithWhitespace + err.readableMessage + cachFlag, DiagnosticSeverity.Error, "")
        diagnostics.append(diag)

        val params = StateChangeParams(
          VerificationRunning.id, filename = filename,
          uri = file_uri, diagnostics = diagnostics.toArray)
        coordinator.sendStateChangeNotification(params, Some(task))
      })
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
      VerificationFailed
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
