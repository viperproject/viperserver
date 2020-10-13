/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2020 ETH Zurich.
  */

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.{CompletableFuture => CFuture}

import akka.actor.{Actor, Props}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position, PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind}
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.JobID
import viper.silver.ast.{Domain, Field, Function, Method, Predicate, SourcePosition}
import viper.silver.reporter._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class FileManager(file_uri: String) {
  // file under verification
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
  var time: Long = 0
  var diagnostics: ArrayBuffer[Diagnostic] = _
//  var verifiables: Array[Verifiable] = _
  var parsingCompleted: Boolean = false
  var typeCheckingCompleted: Boolean = false
  var backendType: String = _
  var progress: Progress = _
//  var shownExecutionTrace: Array[ExecutionTrace] = _
  var symbolInformation: ArrayBuffer[SymbolInformation] = ArrayBuffer()
  var definitions: ArrayBuffer[lsp.Definition] = ArrayBuffer()

  //working variables
  private var lines: Array[String] = Array()
  private var wrongFormat: Boolean = false
  private var partialData: String = ""

  def resetDiagnostics(): Unit = {
    diagnostics = ArrayBuffer()
    val diagnosticParams = new PublishDiagnosticsParams()
    diagnosticParams.setUri(file_uri)
    diagnosticParams.setDiagnostics(diagnostics.asJava)
    Coordinator.client.publishDiagnostics(diagnosticParams)
  }

  def resetLastSuccess(): Unit = {
    lastSuccess = NA
  }

  def prepareVerification(): Unit = {
    is_verifying = true
    is_aborting = false
    state = Stopped
    lines = Array()
    wrongFormat = false
    if (partialData.length > 0) {
      Log.debug("Some unparsed output was detected:\n" + partialData)
      partialData = ""
    }
    time = 0
    resetDiagnostics()
//    verifiables = Array()
    parsingCompleted = true
    typeCheckingCompleted = true
    internalErrorMessage = ""
  }

  def abortVerification(): CFuture[Void] = {
    if (!is_verifying) {
      return CFuture.completedFuture(null)
    }
    Log.info("Aborting running verification.")
    is_aborting = true
    Coordinator.verifier.stopVerification(JobID(jid)).thenAccept(_ => {
      is_verifying = false
      lastSuccess = Aborted
    }).exceptionally(e => {
      Log.debug("Error aborting verification of " + filename + ": " + e)
      null
    })
  }

  def startStageProcess(fileToVerify: String): Option[String] = {
//  def startStageProcess(stage: Stage, fileToVerify: String): Option[String] = {
    try {
      Log.lowLevel("Start Stage Process")
//      Some(getStageCommand(fileToVerify, stage))
      Some("silicon " + fileToVerify)
    } catch {
      case e: Throwable =>
        Log.debug("Error starting stage process: " + e)
        None
    }
  }

  def getStageCommand(fileToVerify: String, stage: Stage): String = {
    val args: String = getViperBackendClassName(stage) + " " + stage.customArguments
//    val command = Settings.expandCustomArguments(args, stage, fileToVerify, Coordinator.backend)
    val command = ""
    Log.debug(command)
    command
  }

  def getViperBackendClassName(stage: Stage): String = {
    Coordinator.backend.backend_type match {
      case "silicon" => "silicon"
      case "carbon" => "carbon"
      case "other" => stage.mainMethod
      case _ => throw new Error(s"Invalid verification backend value. " +
        s"Possible values are [silicon | carbon | other] " +
        s"but found ${Coordinator.backend}")
    }
  }

  def startVerification(manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.manuallyTriggered = manuallyTriggered

    //TODO this should work with precisely one stage
    //This should have exactly one stage
//    if (Coordinator.backend.stages == null || Coordinator.backend.stages.head == null) {
////      Log.debug("backend " + Coordinator.backend.name + " has no " + Settings.VERIFY + " stage, even though the settigns were checked.")
//      println("no stage found!")
//      Log.debug("Should have exactly one stage")
//      return false
//    }
//    val stage = Coordinator.backend.stages.head

//    Coordinator.executedStages.append(stage)
    Log.info("verify " + filename)
    Log.info(Coordinator.backend.name + " verification started")

    val params = StateChangeParams(VerificationRunning.id, filename = filename)
    println("state change params created")
    Coordinator.sendStateChangeNotification(params, Some(this))
    println("state change params sent")

//    val command = startStageProcess(stage, path).getOrElse(return false)
    val command = startStageProcess(path.toString).getOrElse(return false)
    Log.info(s"Successfully generated command: $command")
    val handle = Coordinator.verifier.verify(command)
    jid = handle.id
    if (jid >= 0) {
      Coordinator.verifier.startStreaming(handle, RelayActor.props(this))
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
          val range_start = new Position(member_start.line, member_start.column)
          val range_end = new Position(member_end.line, member_end.column)
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
          task.symbolInformation.append(info)
        })
      case ProgramDefinitionsReport(defs) =>
        definitions = ArrayBuffer()
        defs.foreach(d => {
          val start = d.scope match {
            case Some(s) => new Position(s.start.line, s.start.column)
            case None => null
          }
          val end = d.scope match {
            case Some(s) if s.end.isDefined => new Position(s.end.get.line, s.end.get.column)
            case None => null
          }
          val range: Range = if(start != null && end != null) {
            new Range(start, end)
          } else {
            null
          }
          val sourcePos = d.location.asInstanceOf[viper.silver.ast.SourcePosition]
          val location: Position = new Position(sourcePos.start.line, sourcePos.start.column)
          val definition: lsp.Definition = lsp.Definition(d.typ, d.name, location, range)
          definitions.append(definition)
        })
    case StatisticsReport(m, f, p, _, _) =>
//      TODO: pass in task (probably as props to actor).
      progress = new Progress(p, f, m)
      val params = StateChangeParams(VerificationRunning.id, progress = 0, filename = filename)
      Coordinator.sendStateChangeNotification(params, Some(task))
    case EntitySuccessMessage(_, concerning, _, _) =>
      if (progress == null) {
        Log.debug("The backend must send a VerificationStart message before the ...Verified message.")
      } else {
        val output = BackendOutput(BackendOutputType.FunctionVerified, name = concerning.name)
        progress.updateProgress(output)
        val progressPercentage = progress.toPercent
        val params = StateChangeParams(VerificationRunning.id, progress = progressPercentage, filename = filename)
        Coordinator.sendStateChangeNotification(params, Some(task))
      }
    case EntityFailureMessage(_, _, _, res, _) =>
      res.errors.foreach(err => {
        if (err.fullId != null && err.fullId == "typechecker.error") {
          typeCheckingCompleted = false
        }
        else if (err.fullId != null && err.fullId == "parser.error") {
          parsingCompleted = false
          typeCheckingCompleted = false
        }
        val err_start = err.pos.asInstanceOf[SourcePosition].start
        val err_end = err.pos.asInstanceOf[SourcePosition].end
        val start_pos = new Position(err_start.line, err_start.column)
        val end_pos = if(err_end.isDefined) {
          new Position(err_end.get.line, err_end.get.column)
        } else {
          null
        }
        val range = new Range(start_pos, end_pos)
        Log.toLogFile(s"Error: [${Coordinator.backend.name}] " +
          s"${if(err.fullId != null) "[" + err.fullId + "] " else ""}" +
          s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1} ${err.readableMessage}s")


        val cachFlag: String = if(err.cached) "(cached)" else ""
        val diag = new Diagnostic(range, err.readableMessage + cachFlag, DiagnosticSeverity.Error, "")
        diagnostics.append(diag)

        val params = StateChangeParams(
                      VerificationRunning.id, filename = filename, nofErrors = diagnostics.length,
                      uri = file_uri, diagnostics = diagnostics.toArray)
        Coordinator.sendStateChangeNotification(params, Some(task))
        //Server.sendDiagnostics({ uri: this.fileUri, diagnostics: this.diagnostics })
      })
    case OverallSuccessMessage(_, verificationTime) =>
      state = VerificationReporting
      time = verificationTime
      completionHandler(0)
    case OverallFailureMessage(_, verificationTime, _) =>
      state = VerificationReporting
      time = verificationTime
      completionHandler(0)
    case m: Message => Coordinator.client.notifyUnhandledViperServerMessage(m.toString, 2)
    case e: Throwable =>
        //receiving an error means the promise can be finalized with failure.
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

  private def completionHandler(code: Int) {
    try {
      Log.debug(s"completionHandler is called with code ${code}")
      if (is_aborting) {
        is_verifying = false
        return
      }
      var params: StateChangeParams = null
      var success = NA

      //      val isVerifyingStage = Coordinator.stage.getOrElse(return).isVerification
      val isVerifyingStage = true

      //do we need to start a followUp Stage?
      if (isVerifyingStage) {
        if (partialData != null && partialData.length > 0) {
          Log.debug("Some unparsed output was detected:\n" + partialData)
          partialData = ""
        }

        // Send the computed diagnostics to VSCode.
        //Server.sendDiagnostics({ uri: this.fileUri, diagnostics: this.diagnostics })

        //inform client about postProcessing
        success = determineSuccess(code)
        params = StateChangeParams(PostProcessing.id, filename = filename)
        Coordinator.sendStateChangeNotification(params, Some(this));

        //notify client about outcome of verification
        val mt = if(this.manuallyTriggered) 1 else 0
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, nofErrors = diagnostics.length, time = time.toDouble,
          verificationCompleted = 1, uri = file_uri, error = internalErrorMessage)
        Coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          Log.debug("Verification Backend Terminated Abnormally: with code " + code)
        }
      } else {
        success = Success
        val mt = if(this.manuallyTriggered) 1 else 0
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, nofErrors = 0, time = time.toDouble,
          verificationCompleted = 0, uri = file_uri, error = internalErrorMessage)
        Coordinator.sendStateChangeNotification(params, Some(this))
      }

      //reset for next verification
      lastSuccess = success
      time = 0
      is_verifying = false
    } catch {
      case e: Throwable =>
        is_verifying = false
        Coordinator.client.notifyVerificationNotStarted(file_uri)
        Log.debug("Error handling verification completion: ", e)
    }
  }

//  private def startVerificationTimeout(verificationCount: Int) = {
//    if (Coordinator.backend.timeout > 0) {
//      Log.lowLevel("Set verification timeout to " + Coordinator.backend.timeout)
//
//      def timeout_callback(): Unit = { //aborts the verification on time out
//        if (is_running && this.verificationCount == verificationCount) {
//          Log.hint("The verification timed out after " + Coordinator.backend.timeout + "ms")
//          abortVerification().thenAccept(() => {
//            val params = StateChangeParams(Ready, verificationCompleted = false,
//              success = Timeout, verificationNeeded = false,
//              uri = fileUri)
//            Coordinator.sendStateChangeNotification(params, Some(this))
//          })
//        }
//        is_running = false
//      }
//      //TODO find timeout mechanism
//      setTimeout(timeout_callback(), Coordinator.backend.timeout)
//    } else {
//      Log.lowLevel("No verification timeout set")
//    }
//  }
}