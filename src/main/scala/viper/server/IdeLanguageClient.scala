package viper.server

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

  @JsonNotification(C2S_Commands.StartBackend)
  def notifyBackendStarted(name: String, forceRestart: Boolean, isViperServer: Boolean): Unit

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