/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2020 ETH Zurich.
  */

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.services._
import org.eclipse.lsp4j.services.LanguageClient


trait IdeLanguageClient extends LanguageClient {
  @JsonNotification(S2C_Commands.FileOpened)
  def notifyFileOpened(uri: String): Unit

  @JsonNotification(S2C_Commands.FileClosed)
  def notifyFileClosed(uri: String): Unit

  @JsonRequest(C2S_Commands.GetIdentifier)
  def requestIdentifier(pos: Position): CompletableFuture[String]

  @JsonRequest(C2S_Commands.GetViperFileEndings)
  def requestVprFileEndings(): CompletableFuture[Array[String]]

  @JsonNotification(S2C_Commands.BackendReady)
  def notifyBackendReady(param: BackendReadyParams): Unit

  @JsonNotification(S2C_Commands.BackendStarted)
  def notifyBackendStarted(params: BackendStartedParams): Unit

  @JsonNotification(S2C_Commands.BackendChange)
  def notifyBackendChanged(name: String): Unit

  @JsonNotification(S2C_Commands.Progress)
  def notifyProgress(progress: ProgressReport, logLevel: Int): Unit

  @JsonNotification(S2C_Commands.Log)
  def notifyLog(msg: String, logLevel: Int): Unit

  @JsonNotification(S2C_Commands.Hint)
  def notifyHint(msg: String, hint: Hint): Unit

  @JsonNotification(S2C_Commands.UnhandledViperServerMessageType)
  def notifyUnhandledViperServerMessage(msg: String, logLevel: Int): Unit

  @JsonNotification(S2C_Commands.VerificationNotStarted)
  def notifyVerificationNotStarted(uri: String): Unit

  @JsonNotification(S2C_Commands.StateChange)
  def notifyStateChanged(params: StateChangeParams): Unit
}