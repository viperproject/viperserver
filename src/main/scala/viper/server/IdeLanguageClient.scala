package viper.server

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification


trait IdeLanguageClient extends LanguageClient {
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