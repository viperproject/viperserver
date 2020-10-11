package viper.server

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.TextDocumentItem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Coordinator {
  var port: Int = _
  var url: String = _
  var client: IdeLanguageClient = _

//  var tempDirectory: String = pathHelper.join(os.tmpdir(), ".vscode") = _
//  var backendOutputDirectory: String = os.tmpdir() = _
  var executedStages: ArrayBuffer[Stage] = _
  var documents: TextDocumentItem = new TextDocumentItem()
  var files = mutable.Map.empty[String, FileManager]

  var startingOrRestarting: Boolean = false
  var backend: BackendProperties = _
  var verifier: ViperServerService = _

  def getAddress: String = url + ":" + port

  def stage: Option[Stage] = {
    if (executedStages != null && this.executedStages.nonEmpty) {
      Some(executedStages(executedStages.length - 1))
    } else {
      None
    }
  }

  def canVerificationBeStarted(uri: String, manuallyTriggered: Boolean): Boolean = {
    //check if there is already a verification task for that file
    if(files.get(uri).isEmpty){
      Log.debug("No verification task found for file: " + uri)
      false
    } else if (!verifier.isReady) {
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
    if (files.nonEmpty) {
      val promises = files.values.map(task => task.abortVerification()).toSeq
      CompletableFuture.allOf(promises:_*)
    } else {
      CompletableFuture.completedFuture(null)
    }
  }

  //Communication requests and notifications sent to language client
  def sendStateChangeNotification(params: StateChangeParams, task: Option[FileManager]): Unit = {
    if (task.isDefined) task.get.state = VerificationState.apply(params.newState.toInt)
    try {
      client.notifyStateChanged(params)
    } catch {
      case e: Throwable => println(e)
    }
  }

//  def sendStepsAsDecorationOptions(decorations: StepsAsDecorationOptionsResult) = {
//    Log.log("Update the decoration options (" + decorations.decorationOptions.length + ")", LogLevel.Debug)
//    client.stepsAsDecorationOptions(decorations)
//  }
//
//  def sendStartBackendMessage(backend: String, forceRestart: Boolean, isViperServer: Boolean) {
//    client.sendNotification(Commands.StartBackend, {backend: backend, forceRestart: forceRestart, isViperServer: isViperServer })
//  }
}