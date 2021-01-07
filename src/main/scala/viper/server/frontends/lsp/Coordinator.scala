// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture

import scala.collection.mutable

object Coordinator {
  var port: Int = _
  var url: String = _
  var client: IdeLanguageClient = _

  var files = mutable.Map.empty[String, FileManager]    //Each file is managed individually.
  var verifier: ViperServerService = null   // verification enginge. Set once on start.
  var backend: BackendProperties = null     // Backend the engine uses. Can be swapped throughout

  def getAddress: String = url + ":" + port

  /** Checks if verification can be started for a given file.
    *
    * Informs client differently depending on whether or not verification attempt was triggered manually
    * */
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

  /** Stops all running verifications.
    *
    * Returns a CF that is successfully completed if all running verifications were stopped
    * successfully. Otherwise, a failed CF is returned
    * */
  def stopAllRunningVerifications(): CompletableFuture[Void] = {
    if (files.nonEmpty) {
      val promises = files.values.map(task => task.abortVerification()).toSeq
      CompletableFuture.allOf(promises:_*)
    } else {
      CompletableFuture.completedFuture(null)
    }
  }

  /** Notifies the client about a state change
    *
    * If state change is related to a particular file, its manager's state is also updated.
    * */
  def sendStateChangeNotification(params: StateChangeParams, task: Option[FileManager]): Unit = {
    if (task.isDefined) task.get.state = VerificationState.apply(params.newState)
    try {
      client.notifyStateChanged(params)
    } catch {
      case e: Throwable => Log.debug("Error while changing state: ", e)
    }
  }
}