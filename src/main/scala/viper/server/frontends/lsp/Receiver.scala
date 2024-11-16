// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.{CompletableFuture, CompletionStage}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware}
import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializeResult, InitializedParams, ServerCapabilities, TextDocumentSyncKind}
import viper.server.ViperConfig
import viper.server.core.VerificationExecutionContext
import viper.viperserver.BuildInfo

import scala.annotation.unused
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.jdk.FutureConverters._
import scala.language.postfixOps


abstract class StandardReceiver(server: ViperServerService)(implicit executor: VerificationExecutionContext) extends LanguageClientAware {
  val coordinator = new ClientCoordinator(server)

  @JsonRequest("initialize")
  def onInitialize(@unused params: InitializeParams): CompletionStage[InitializeResult] = {
    coordinator.logger.info("initialize")
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    // capabilities.setDefinitionProvider(true)
    // capabilities.setDocumentSymbolProvider(true)
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonNotification("initialized")
  def onInitialized(@unused params: InitializedParams): Unit = {
    coordinator.logger.info("initialized")
  }

  @JsonNotification("textDocument/didOpen")
  def onDidOpenDocument(params: DidOpenTextDocumentParams): Unit = {
    val uri: String = params.getTextDocument.getUri
    coordinator.logger.info(s"On opening document $uri")
    try {
      coordinator.isViperSourceFile(uri).map(_ => {
        //create new task for opened file
        coordinator.addFileIfNecessary(uri)
      })
    } catch {
      case e: Throwable => coordinator.logger.debug(s"Error handling TextDocument opened: $e")
    }
  }

  @JsonNotification("workspace/didChangeConfiguration")
  def onDidChangeConfig(@unused params: DidChangeConfigurationParams): Unit = {
    coordinator.logger.info("On config change")
  }

  @JsonNotification("textDocument/didChange")
  def onDidChangeDocument(params: DidChangeTextDocumentParams): Unit = {
    coordinator.logger.info("On changing document")
    coordinator.resetFile(params.getTextDocument.getUri)
  }

  @JsonNotification("workspace/didChangeWatchedFiles")
  def onDidChangeWatchesFiles(params: DidChangeWatchedFilesParams): Unit = {
    coordinator.logger.info("On changing watched files")
    params.getChanges.forEach(ev => coordinator.resetFile(ev.getUri))
  }

  @JsonNotification("textDocument/didSave")
  def onDidSaveDocument(params: DidSaveTextDocumentParams): Unit = {
    coordinator.logger.info("On saving document")
    coordinator.resetFile(params.getTextDocument.getUri)
  }
  /*
  @JsonRequest("textDocument/documentSymbol")
  def onGetDocumentSymbol(params: DocumentSymbolParams): CompletionStage[Array[SymbolInformation]] = {
    coordinator.logger.info("On getting document symbol")
    CompletableFuture.completedFuture(coordinator.getSymbolsForFile(params.getTextDocument.getUri))
  }

  @JsonRequest("textDocument/definition")
  def onGetDefinition(params: TextDocumentPositionParams): CompletionStage[Location] = {
    coordinator.logger.debug(s"Handling definitions request for params: $params}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val definitions = coordinator.getDefinitionsForFile(uri)

    def hasGlobalScope(d: Definition) = d.scope == null
    def isPosInScope(d: Definition) = hasGlobalScope(d) ||
      (Common.comparePosition(d.scope.getStart, pos) <= 0 &&
        Common.comparePosition(d.scope.getEnd, pos) >= 0)

    // TODO double check whether this algorithm makes any sense when we support definition lookup
    coordinator.client.requestIdentifier(pos).thenApply(word => {
      coordinator.logger.trace(s"Got word: $word")
      val defs_in_scope = definitions.filter(isPosInScope)
      val matching_def = defs_in_scope.find(d => word == d.name)
      val optLocation = matching_def.map(definition => new Location(uri, new Range(definition.code_location, definition.code_location)))
      optLocation.orNull
    })
  }
  */
  @JsonNotification("textDocument/didClose")
  def onDidCloseDocument(params: DidCloseTextDocumentParams): Unit = {
    coordinator.logger.info("On closing document")
    try {
      val uri = params.getTextDocument.getUri
      coordinator.removeFileIfExists(uri)
    } catch {
      case _: Throwable => coordinator.logger.debug("Error handling TextDocument opened")
    }
  }

  // 'shutdown' is called before 'exit'
  @JsonRequest(value = "shutdown")
  def onShutdown(): CompletionStage[AnyRef] = {
    coordinator.logger.debug("shutdown")
    // we instruct the server to stop all verifications but we only wait for 2s for their completion since
    // the LSP client expects a response within 3s
    val stopVerifications = coordinator.stopAllRunningVerifications()
    val timeout = akka.pattern.after(2 seconds)(Future.unit)(executor.actorSystem)
    Future.firstCompletedOf(Seq(stopVerifications, timeout)).asJava.thenApply(_ => null)
  }

  @JsonNotification(value = "exit")
  def onExit(): Unit = {
    coordinator.logger.debug("exit")
    coordinator.exit()
  }
}

class CustomReceiver(config: ViperConfig, server: ViperServerService, serverUrl: String)(implicit executor: VerificationExecutionContext) extends StandardReceiver(server) {

  val MIN_CLIENT_VERSION = "4.0.0"

  // receives version of client to perform some checks before replying with the server's version
  @JsonRequest(C2S_Commands.GetVersion)
  def onGetVersion(request: GetVersionRequest): CompletionStage[GetVersionResponse] = {
    val supported = if (config.disableClientVersionCheck()) {
      true
    } else {
      val res = Common.compareSemVer(request.clientVersion, MIN_CLIENT_VERSION)
      res >= 0
    }
    if (supported) {
      CompletableFuture.completedFuture(GetVersionResponse(BuildInfo.projectVersion))
    } else {
      val errorMsg = s"Client is not compatible with server - expected at least client version $MIN_CLIENT_VERSION but is ${request.clientVersion}"
      CompletableFuture.completedFuture(GetVersionResponse(BuildInfo.projectVersion, errorMsg))
    }
  }

  @JsonRequest(C2S_Commands.RemoveDiagnostics)
  def onRemoveDiagnostics(request: RemoveDiagnosticsRequest): CompletionStage[RemoveDiagnosticsResponse] = {
    coordinator.logger.debug("On removing diagnostics")
    coordinator.resetDiagnostics(request.uri)
    CompletableFuture.completedFuture(RemoveDiagnosticsResponse(true))
  }

  @JsonRequest(C2S_Commands.GetLanguageServerUrl)
  def onGetServerUrl(): CompletionStage[GetLanguageServerUrlResponse] = {
    coordinator.logger.debug("On getting server URL")
    CompletableFuture.completedFuture(GetLanguageServerUrlResponse(serverUrl))
  }

  @JsonRequest(C2S_Commands.Reformat)
  def onReformat(data: ReformatParams): CompletionStage[Option[String]] = {
    coordinator.logger.info("On reformat")
    val result = coordinator.reformatFile(data.uri);
    CompletableFuture.completedFuture(result)
  }

  @JsonNotification(C2S_Commands.Verify)
  def onVerify(data: VerifyParams): Unit = {
    coordinator.logger.debug("On verifying")
    if (coordinator.canVerificationBeStarted(data.uri, data.manuallyTriggered)) {
      // stop all other verifications because the backend crashes if multiple verifications are run in parallel
      coordinator.logger.trace("verification can be started - all running verifications are now going to be stopped")
      coordinator.stopAllRunningVerifications().map(_ => {
        coordinator.logger.info("start or restart verification")

        val verificationStarted = coordinator.startVerification(data.backend, data.customArgs, data.uri, data.manuallyTriggered)
        if (verificationStarted) {
          coordinator.logger.info("Verification Started")
        } else {
          coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(data.uri))
        }
      }).recover(e => {
        coordinator.logger.debug(s"Error handling verify request: $e")
        coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(data.uri))
      })
    } else {
      coordinator.logger.info("The verification cannot be started.")
      coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(data.uri))
    }
  }

  @JsonRequest(C2S_Commands.FlushCache)
  def onFlushCache(params: FlushCacheParams): CompletionStage[Unit] = {
    coordinator.logger.debug("flushing cache...")
    val uriOpt = Option(params.uri)
    val backendOpt = Option(params.backend)
    coordinator.flushCache(uriOpt, backendOpt).asJava
  }

  @JsonRequest(C2S_Commands.StopVerification)
  def onStopVerification(request: StopVerificationRequest): CompletionStage[StopVerificationResponse] = {
    coordinator.logger.debug("on stopping verification")
    try {
      coordinator.stopRunningVerification(request.uri)
        .map(success => StopVerificationResponse(success))
        .asJava
    } catch {
      case e: Throwable =>
        coordinator.logger.debug(s"Error handling stop verification request (critical): $e")
        CompletableFuture.completedFuture(StopVerificationResponse(false))
    }
  }

  override def connect(client: LanguageClient): Unit = {
    val c = client.asInstanceOf[IdeLanguageClient]
    coordinator.setClient(c)
  }

  /** called when stream to the client is closed. No messages should be sent afterwards */
  def disconnected(): Unit = {
    coordinator.exit()
  }
}
