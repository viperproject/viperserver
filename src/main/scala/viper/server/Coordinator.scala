package viper.server

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.{Position, TextDocumentItem, Range}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object Coordinator {
  var port: Int = _
  var url: String = _
  var client: IdeLanguageClient = _

  var backend: Backend
  var tempDirectory: String = pathHelper.join(os.tmpdir(), ".vscode") = _
  var backendOutputDirectory: String = os.tmpdir() = _
  var executedStages: ArrayBuffer[Stage] = _
  var documents: TextDocumentItem = new TextDocumentItem()
  var verificationTasks = mutable.Map.empty[String, VerificationTask]
  var backendService: ViperServerService = _
  var startingOrRestarting: Boolean = false

  def getAddress: String = url + ":" + port

  def stage: Option[Stage] = {
    if (executedStages != null && this.executedStages.length > 0) {
      Some(executedStages(executedStages.length - 1))
    } else {
      None
    }
  }

  def canVerificationBeStarted(uri: String, manuallyTriggered: Boolean): Boolean = {
    //check if there is already a verification task for that file
    if(verificationTasks.get(uri).isEmpty){
      Log.debug("No verification task found for file: " + uri)
      false
    } else if (!backendService.isReady) {
      if (manuallyTriggered) {
        Log.hint("The verification backend is not ready yet")
      }
      Log.debug("The verification backend is not ready yet")
      false
    } else {
      true
    }
  }

  def stopAllRunningVerifications(): CompletableFuture[Void] = {
    if (Coordinator.verificationTasks.nonEmpty) {
      val promises = Coordinator.verificationTasks.values.map(task => task.abortVerification()).toSeq
      CompletableFuture.allOf(promises:_*)
    } else {
      CompletableFuture.completedFuture()
    }
  }

  //Communication requests and notifications sent to language client
  def sendStateChangeNotification(params: StateChangeParams, task: Option[VerificationTask]): Unit = {
    if (task.isDefined) task.get.state = params.newState
    client.notifyStateChanged(params)
  }

  def sendStepsAsDecorationOptions(decorations: StepsAsDecorationOptionsResult) = {
    Log.log("Update the decoration options (" + decorations.decorationOptions.length + ")", LogLevel.Debug)
    client.stepsAsDecorationOptions(decorations)
  }

  def sendStartBackendMessage(backend: String, forceRestart: Boolean, isViperServer: Boolean) {
    client.sendNotification(Commands.StartBackend, {backend: backend, forceRestart: forceRestart, isViperServer: isViperServer })
  }
}