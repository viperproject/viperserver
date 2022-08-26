// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.{CompletableFuture => CFuture}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position, PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind}
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast.{Domain, Field, Function, Method, Predicate, SourcePosition}
import viper.silver.reporter._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

class FileManager(coordinator: ClientCoordinator, file_uri: String) {
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
  var diagnostics: ArrayBuffer[Diagnostic] = _
  var parsingCompleted: Boolean = false
  var typeCheckingCompleted: Boolean = false
  var backendType: String = _
  var progress: Progress = _
  var symbolInformation: ArrayBuffer[SymbolInformation] = ArrayBuffer()
  var definitions: ArrayBuffer[lsp.Definition] = ArrayBuffer()

  private var partialData: String = ""

  def resetDiagnostics(): Unit = {
    diagnostics = ArrayBuffer()
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

  def stopVerification(): CFuture[Void] = {
    if (!is_verifying) {
      return CFuture.completedFuture(null)
    }
    coordinator.logger.info("Aborting running verification.")
    is_aborting = true
    coordinator.server.stopVerification(VerJobId(jid), Some(coordinator.logger)).thenAccept(_ => {
      is_verifying = false
      lastSuccess = Aborted
    }).exceptionally(e => {
      coordinator.logger.debug(s"Error aborting verification of $filename: $e")
      null
    })
  }

  def getVerificationCommand(fileToVerify: String): Option[String] = {
    try {
      val args: String = getViperBackendClassName + s" $fileToVerify"
      coordinator.logger.debug(args)
      Some(args)
    } catch {
      case e: Throwable =>
        coordinator.logger.debug(s"Error finding backend: $e")
        None
    }
  }

  private def getViperBackendClassName: String = {
    coordinator.backend match {
      case Some(backend) => backend.backend_type match {
        case "Silicon" => "silicon"
        case "Carbon" => "carbon"
        case backendTyp => throw new Error(s"Invalid verification backend value. " +
          s"Possible values are [silicon | carbon] " +
          s"but found $backendTyp")
      }
      case _ => throw new Error(s"backend is not yet set")
    }
  }

  def startVerification(manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.manuallyTriggered = manuallyTriggered

    coordinator.logger.info(s"verify $filename")

    val params = StateChangeParams(VerificationRunning.id, filename = filename)
    coordinator.sendStateChangeNotification(params, Some(this))

    val command = getVerificationCommand(path.toString).getOrElse(return false)
    coordinator.logger.info(s"Successfully generated command: $command")
    val handle = coordinator.server.verifyWithCommand(command, Some(coordinator.logger))
    jid = handle.id
    if (jid >= 0) {
      coordinator.server.startStreaming(handle, RelayActor.props(this), Some(coordinator.logger))
      true
    } else {
      false
    }
  }

  object RelayActor {
    def props(task: FileManager): Props = Props(new RelayActor(task))
  }

  class RelayActor(task: FileManager) extends Actor {

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
        progress = new Progress(coordinator, p, f, m)
        val params = StateChangeParams(VerificationRunning.id, progress = 0, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(task))
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
        res.errors.foreach(err => {
          if (err.fullId != null && err.fullId == "typechecker.error") {
            typeCheckingCompleted = false
          } else if (err.fullId != null && err.fullId == "parser.error") {
            parsingCompleted = false
            typeCheckingCompleted = false
          }
          val err_start = err.pos.asInstanceOf[SourcePosition].start
          val err_end = err.pos.asInstanceOf[SourcePosition].end
          val start_pos = new Position(err_start.line - 1, err_start.column - 1)
          val end_pos = if(err_end.isDefined) {
            new Position(err_end.get.line - 1, err_end.get.column - 1)
          } else {
            null
          }
          val range = new Range(start_pos, end_pos)
          val backendType = coordinator.backend match {
            case Some(backend) => backend.backend_type
            case _ => "unknown"
          }
          coordinator.logger.debug(s"Verification error: [$backendType] " +
            s"${if(err.fullId != null) "[" + err.fullId + "] " else ""}" +
            s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1} ${err.readableMessage}s")


          val cachFlag: String = if(err.cached) "(cached)" else ""
          val diag = new Diagnostic(range, err.readableMessage + cachFlag, DiagnosticSeverity.Error, "")
          diagnostics.append(diag)

          val params = StateChangeParams(
            VerificationRunning.id, filename = filename,
            uri = file_uri, diagnostics = diagnostics.toArray)
          coordinator.sendStateChangeNotification(params, Some(task))
        })
      case OverallSuccessMessage(_, verificationTime) =>
        state = VerificationReporting
        timeMs = verificationTime
        // the completion handler is not yet invoked (but as part of Status.Success)
      case OverallFailureMessage(_, verificationTime, _) =>
        state = VerificationReporting
        timeMs = verificationTime
      // the completion handler is not yet invoked (but as part of Status.Success)
      case m: Message => coordinator.client.notifyUnhandledViperServerMessage(m.name, m.toString, 2)
      case Status.Success =>
        // Success is sent when the stream is completed
        completionHandler(0)
      case Status.Failure(cause) =>
        coordinator.logger.info(s"Streaming messages has failed in RelayActor with cause $cause")
        completionHandler(-1) // no success
      case e: Throwable => coordinator.logger.debug(s"RelayActor received throwable: $e")
    }
  }

  private def determineSuccess(code: Int): VerificationSuccess = {
    if (diagnostics.isEmpty && code == 0) {
      VerificationSuccess.Success
    } else if (diagnostics.nonEmpty) {
      //use tag and backend trace as indicators for completed parsing
      if (!parsingCompleted) {
        ParsingFailed
      } else if (parsingCompleted && !typeCheckingCompleted) {
        TypecheckingFailed
      } else {
        VerificationFailed
      }
    } else if (is_aborting) {
      Aborted
    } else {
      Error
    }
  }

  private def completionHandler(code: Int): Unit = {
    try {
      coordinator.logger.debug(s"completionHandler is called with code $code")
      if (is_aborting) {
        is_verifying = false
        return
      }
      var params: StateChangeParams = null
      var success = NA

      val isVerifyingStage = true

      //do we need to start a followUp Stage?
      if (isVerifyingStage) {
        if (partialData != null && partialData.nonEmpty) {
          coordinator.logger.debug(s"Some unparsed output was detected: $partialData")
          partialData = ""
        }

        //inform client about postProcessing
        success = determineSuccess(code)
        params = StateChangeParams(PostProcessing.id, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(this))

        //notify client about outcome of verification
        val mt = if(this.manuallyTriggered) 1 else 0
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 1, uri = file_uri,
          error = internalErrorMessage, diagnostics = diagnostics.toArray)
        coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          coordinator.logger.debug(s"Verification Backend Terminated Abnormally: with code $code")
        }
      } else {
        success = Success
        val mt = if(this.manuallyTriggered) 1 else 0
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = timeMs.toDouble / 1000, verificationCompleted = 0, uri = file_uri,
          error = internalErrorMessage, diagnostics = diagnostics.toArray)
        coordinator.sendStateChangeNotification(params, Some(this))
      }

      //reset for next verification
      lastSuccess = success
      timeMs = 0
      is_verifying = false
    } catch {
      case e: Throwable =>
        is_verifying = false
        coordinator.client.notifyVerificationNotStarted(file_uri)
        coordinator.logger.debug(s"Error handling verification completion: $e")
    }
  }
}
