package viper.server.core

import org.eclipse.lsp4j.{MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, Range, ShowMessageRequestParams}
import viper.server.frontends.lsp.{BranchFailureDetails, GetIdentifierResponse, GetRangeResponse, GetViperFileEndingsResponse, HintMessage, IdeLanguageClient, LogParams, SetupProjectParams, StateChangeParams, UnhandledViperServerMessageTypeParams, VerificationNotStartedParams}

import java.util.concurrent.CompletableFuture

class MockClient extends IdeLanguageClient {
  override def requestIdentifier(pos: Position): CompletableFuture[GetIdentifierResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
  override def requestRange(range: Range): CompletableFuture[GetRangeResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
  override def requestVprFileEndings(): CompletableFuture[GetViperFileEndingsResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
  override def requestSetupProject(param: SetupProjectParams): CompletableFuture[Unit] = CompletableFuture.failedFuture(new UnsupportedOperationException())
  override def notifyLog(param: LogParams): Unit = {}
  override def notifyHint(param: HintMessage): Unit = {}
  override def notifyUnhandledViperServerMessage(params: UnhandledViperServerMessageTypeParams): Unit = {}
  override def notifyVerificationNotStarted(params: VerificationNotStartedParams): Unit = {}
  override def notifyStateChanged(params: StateChangeParams): Unit = {}
  override def sendBranchFailureInfo(params: BranchFailureDetails): Unit = {}
  override def telemetryEvent(`object`: Any): Unit = {}
  override def publishDiagnostics(diagnostics: PublishDiagnosticsParams): Unit = {}
  override def showMessage(messageParams: MessageParams): Unit = {}
  override def showMessageRequest(requestParams: ShowMessageRequestParams): CompletableFuture[MessageActionItem] = CompletableFuture.failedFuture(new UnsupportedOperationException())
  override def logMessage(message: MessageParams): Unit = {}
}
