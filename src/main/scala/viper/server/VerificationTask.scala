package viper.server

import java.nio.file.Paths
import java.util.concurrent.{CompletableFuture => CFuture}

import akka.actor.Actor
import akka.util.{Timeout => AkkaTimeout}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Location, Position, PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind}
import viper.server.VerificationState._
import viper.server.VerificationSuccess._
import viper.silver.ast.{Domain, Field, Method, Predicate, SourcePosition}
import viper.silver.reporter.{EntityFailureMessage, EntitySuccessMessage, OverallFailureMessage, OverallSuccessMessage, ProgramDefinitionsReport, ProgramOutlineReport, StatisticsReport}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

class VerificationTask(fileUri: String) {
  //state that is valid across verifications
  var verificationCount: Int = 0

  // file under verification
  var filename: String = _
  var path: String = _
  var lastSuccess: VerificationSuccess = NA
  var internalErrorMessage: String = ""

  //state specific to one verification
  var is_running: Boolean = false
  var global_failure: Boolean = false
  var is_aborting: Boolean = false
  var state: VerificationState = Stopped
  var manuallyTriggered: Boolean

  //verification results
  var time: Long = 0
  var diagnostics: mutable.ArrayBuffer[Diagnostic] = _
//  var verifiables: Array[Verifiable] = _
  var parsingCompleted: Boolean = false
  var typeCheckingCompleted: Boolean = false
  var backendType: String = _
  var progress: Progress = _
//  var shownExecutionTrace: Array[ExecutionTrace] = _
  var symbolInformation: ArrayBuffer[SymbolInformation] = _
  var definitions: ArrayBuffer[Definition] = _
  var manuallyTriggered: Boolean = _

  //working variables
  private var lines: Array[String] = Array()
  private var wrongFormat: Boolean = false
  private var partialData: String = ""

  def resetDiagnostics() = {
    diagnostics = ArrayBuffer()
    val diagnosticParams = new PublishDiagnosticsParams()
    diagnosticParams.setUri(fileUri)
    diagnosticParams.setDiagnostics(diagnostics.asJava)
    Coordinator.client.publishDiagnostics(diagnosticParams)
  }

  def resetLastSuccess() = {
    lastSuccess = NA
  }

  def prepareVerification() = {
    is_running = true
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

  def abortVerification(): CFuture[Unit] = {
    if (!is_running) {
      return CFuture.completedFuture()
    }
    Log.info("Aborting running verification.")
    is_aborting = true
    Coordinator.backendService.stopVerification().thenApply(_ => {
      is_running = false
      lastSuccess = Aborted
    }).exceptionally(e => {
      Log.debug("Error aborting verification of " + filename + ": " + e)
    })
  }

  def verify(manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.manuallyTriggered = manuallyTriggered

    // This should have exactly one stage
    val stage = Coordinator.backend.stages.head
    if (stage == null) {
      Log.debug("backend " + Coordinator.backend.name + " has no " + Settings.VERIFY + " stage, even though the settigns were checked.")
      return false
    }

    // why must this be variable?
    path = Common.uriToPath(fileUri)
    filename = Common.filenameFromPath(path)
    Log.toLogFile("verify " + filename)

    verificationCount += 1
    Coordinator.executedStages.append(stage)
    Log.info(Coordinator.backend.name + " verification started")

    val params = StateChangeParams(VerificationRunning, filename = filename)
    Coordinator.sendStateChangeNotification(params, Some(this))

    startVerificationTimeout(verificationCount)
    Coordinator.backendService.startStageProcess(stage, path, stdOutHandler, stdErrHandler, completionHandler)
    true
  }

  class RelayActor() extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case ProgramOutlineReport(members) =>
        symbolInformation = ArrayBuffer()
        members.foreach(m => {
          val location: Location = new Location(fileUri, null)
          val kind = m match {
            case Method => SymbolKind.Method
            case Function => SymbolKind.Function
            case Predicate => SymbolKind.Interface
            case Field => SymbolKind.Field
            case Domain => SymbolKind.Class
            case _ => SymbolKind.Enum
          }
          val info: SymbolInformation = new SymbolInformation(m.name, kind, location)
          symbolInformation.append(info)
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
          val definition: Definition = Definition(d.typ, d.name, location, range)
          definitions.append(definition)
          //Log.log("Definition: " + JSON.stringify(definition), LogLevel.LowLevelDebug)
        })
    case StatisticsReport(m, f, p, _, _) =>
//      TODO: pass in task (probably as props to actor).
      progress = new Progress(p, f, m)
      val params = StateChangeParams(VerificationRunning, progress = 0, filename = filename)
      Coordinator.sendStateChangeNotification(params, this)
    case EntitySuccessMessage(_, concerning, _, _) =>
      if (progress == null) {
        Log.debug("The backend must send a VerificationStart message before the ...Verified message.")
        return
      }
      val output = BackendOutput(BackendOutputType.FunctionVerified, name = concerning.name)
      progress.updateProgress(output)
      val progressPercentage = progress.toPercent()
      val params = StateChangeParams(VerificationRunning, progress = progressPercentage, filename = filename)
      Coordinator.sendStateChangeNotification(params, this)
    case EntityFailureMessage(ver, concerning, time, res, cached) =>
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

        val params = StateChangeParams(VerificationRunning, filename = filename, nofErrors = diagnostics.length, uri = fileUri, diagnostics)
        Coordinator.sendStateChangeNotification(params, this)
        //Server.sendDiagnostics({ uri: this.fileUri, diagnostics: this.diagnostics })
      })
    case OverallSuccessMessage(ver, verificationTime) =>
      state = VerificationReporting
      time = verificationTime
      Coordinator.backendService.isSessionRunning = false
      completionHandler(0)
    case OverallFailureMessage(ver, verificationTime, failure) =>
      state = VerificationReporting
      time = verificationTime
      Coordinator.backendService.isSessionRunning = false
      completionHandler(0)
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
      Log.debug("completionHandler is called with code ${code}")
      if (is_aborting) {
        is_running = false
        return
      }
      var success = NA

      val isVerifyingStage = Coordinator.stage.getOrElse(return).isVerification

      //do we need to start a followUp Stage?
      if (!is_aborting && isVerifyingStage) {
        success = determineSuccess(code)
      }

      if (!isVerifyingStage) {
        success = Success
        var params = StateChangeParams(
                  Ready, success = Success, manuallyTriggered = manuallyTriggered,
                  filename = filename, nofErrors = 0, time = time.toInt,
                  verificationCompleted = false, uri = fileUri,
                  error = internalErrorMessage)
        Coordinator.sendStateChangeNotification(params, Some(this))
      } else {
        if (partialData.length > 0) {
          Log.debug("Some unparsed output was detected:\n" + partialData)
          partialData = ""
        }

        // Send the computed diagnostics to VSCode.
        //Server.sendDiagnostics({ uri: this.fileUri, diagnostics: this.diagnostics })

        //inform client about postProcessing
        var params = StateChangeParams(PostProcessing, filename = filename)
        Coordinator.sendStateChangeNotification(params, Some(this))

        //notify client about outcome of verification
        params = StateChangeParams(Ready, success = Success, manuallyTriggered = manuallyTriggered,
          filename = filename, nofErrors = diagnostics.length, time = time.toInt,
          verificationCompleted = true, uri = fileUri, error = internalErrorMessage)
        Coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          Log.debug("Verification Backend Terminated Abnormaly: with code " + code)
        }
      }

      //reset for next verification
      lastSuccess = success
      time = 0
      is_running = false
    } catch {
      case e: Throwable =>
        is_running = false
        Coordinator.client.notifyVerificationNotStarted(fileUri)
        Log.debug("Error handling verification completion: ", e)
    }
  }

  private def startVerificationTimeout(verificationCount: Int) = {
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
  }
}