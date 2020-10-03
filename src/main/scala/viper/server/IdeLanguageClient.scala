package viper.server

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{Diagnostic, Position}
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}


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
  def notifyBackendReady(uri: String)

  @JsonNotification(S2C_Commands.BackendChange)
  def notifyBackendChanged(name: String)

  @JsonNotification(S2C_Commands.Progress)
  def notifyProgress(progress: Progress, logLevel: Int)

  @JsonNotification(S2C_Commands.Log)
  def notifyLog(msg: String, logLevel: Int)

  @JsonNotification(S2C_Commands.Hint)
  def notifyHint(msg: String, logLevel: Int)

  @JsonNotification(S2C_Commands.VerificationNotStarted)
  def notifyVerificationNotStarted(uri: String)

  @JsonNotification(S2C_Commands.StateChange)
  def notifyStateChanged(params: StateChangeParams)


  //  @JsonNotification("gobraServer/noVerificationInformation")
//  def noVerificationInformation(): Unit
//
//  @JsonNotification("gobraServer/overallResult")
//  def overallResult(params: String): Unit
//
//  @JsonNotification("gobraServer/verificationProgress")
//  def verificationProgress(fileUri: String, progress: Int): Unit
//
//  @JsonNotification("gobraServer/verificationException")
//  def verificationException(fileUri: String): Unit
//
//
//  @JsonNotification("gobraServer/finishedGoifying")
//  def finishedGoifying(fileUri: String, success: Boolean): Unit
//
//  @JsonNotification("gobraServer/finishedGobrafying")
//  def finishedGobrafying(oldFilePath: String, newFilePath: String, success: Boolean): Unit
//
//  @JsonNotification("gobraServer/finishedViperCodePreview")
//  def finishedViperCodePreview(ast: String, highlighted: String): Unit
//
//  @JsonNotification("gobraServer/finishedInternalCodePreview")
//  def finishedInternalCodePreview(internal: String, highlighted: String): Unit
}