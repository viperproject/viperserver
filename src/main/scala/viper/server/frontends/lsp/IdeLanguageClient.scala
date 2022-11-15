// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.jsonrpc.services._
import org.eclipse.lsp4j.services.LanguageClient


trait IdeLanguageClient extends LanguageClient {

  @JsonRequest(C2S_Commands.GetIdentifier)
  def requestIdentifier(pos: Position): CompletableFuture[GetIdentifierResponse]

  @JsonRequest(C2S_Commands.GetViperFileEndings)
  def requestVprFileEndings(): CompletableFuture[GetViperFileEndingsResponse]

  @JsonNotification(S2C_Commands.Log)
  def notifyLog(param: LogParams): Unit

  @JsonNotification(S2C_Commands.Hint)
  def notifyHint(param: HintMessage): Unit

  @JsonNotification(S2C_Commands.UnhandledViperServerMessageType)
  def notifyUnhandledViperServerMessage(params: UnhandledViperServerMessageTypeParams): Unit

  @JsonNotification(S2C_Commands.VerificationNotStarted)
  def notifyVerificationNotStarted(params: VerificationNotStartedParams): Unit

  @JsonNotification(S2C_Commands.StateChange)
  def notifyStateChanged(params: StateChangeParams): Unit
}
