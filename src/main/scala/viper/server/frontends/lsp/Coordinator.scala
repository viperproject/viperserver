/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2020 ETH Zurich.
  */

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.TextDocumentItem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Coordinator {
  var port: Int = _
  var url: String = _
  var client: IdeLanguageClient = _

  var files = mutable.Map.empty[String, FileManager]
  var backend: BackendProperties = null
  var verifier: ViperServerService = null

  def getAddress: String = url + ":" + port

  def canVerificationBeStarted(uri: String, manuallyTriggered: Boolean): Boolean = {
    //check if there is already a verification task for that file
    if(files.get(uri).isEmpty){
      Log.debug("No verification task found for file: " + uri)
      false
    } else if (!verifier.is_ready) {
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
}