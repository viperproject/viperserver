// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.{CompletableFuture, CompletionStage}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware}
import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbolParams, InitializeParams, InitializeResult, InitializedParams, Location, Range, ServerCapabilities, SymbolInformation, TextDocumentPositionParams, TextDocumentSyncKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.VerificationState._

import scala.annotation.unused
import scala.jdk.FutureConverters._


abstract class StandardReceiver(server: ViperServerService)(implicit executor: VerificationExecutionContext) extends LanguageClientAware {
  val coordinator = new ClientCoordinator(server)

  @JsonRequest("initialize")
  def onInitialize(@unused params: InitializeParams): CompletionStage[InitializeResult] = {
    coordinator.logger.info("initialize")
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setDefinitionProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonNotification("initialized")
  def onInitialized(@unused params: InitializedParams): Unit = {
    coordinator.logger.info("initialized")
  }

  @JsonNotification("textDocument/didOpen")
  def onDidOpenDocument(params: DidOpenTextDocumentParams): Unit = {
    coordinator.logger.info("On opening document")
    try {
      val uri: String = params.getTextDocument.getUri
      coordinator.isViperSourceFile(uri).map(_ => {
        // notify client
        coordinator.client.notifyFileOpened(uri)
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
    try {
      coordinator.logger.trace("check if restart needed")
      if (coordinator.backend.isEmpty) {
        coordinator.logger.info("Change Backend: from 'No Backend' to Silicon")
        val backend = "Silicon"
        coordinator.changeBackendIfNecessary(BackendProperties(name = s"Viper$backend", backend_type = backend))
      } else {
        coordinator.logger.debug("No need to restart backend. It is still the same")
      }
    } catch {
      case e: Throwable => coordinator.logger.debug(s"Error handling swap backend request: $e")
    }
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

  @JsonNotification("textDocument/didClose")
  def onDidCloseDocument(params: DidCloseTextDocumentParams): Unit = {
    coordinator.logger.info("On closing document")
    try {
      val uri = params.getTextDocument.getUri
      coordinator.isViperSourceFile(uri).map(isViperFile => {
        if (isViperFile) coordinator.client.notifyFileClosed(uri)
      })
    } catch {
      case _: Throwable => coordinator.logger.debug("Error handling TextDocument opened")
    }
  }

  // 'shutdown' is called before 'exit'
  @JsonRequest(value = "shutdown")
  def onShutdown(): CompletionStage[AnyRef] = {
    coordinator.logger.debug("shutdown")
    coordinator.stopAllRunningVerifications().asJava.thenApply(_ => null) // .thenApply(_ => null)
  }

  @JsonNotification(value = "exit")
  def onExit(): Unit = {
    coordinator.logger.debug("exit")
    coordinator.exit()
  }
}

class CustomReceiver(server: ViperServerService, serverUrl: String)(implicit executor: VerificationExecutionContext) extends StandardReceiver(server) {

  @JsonNotification(S2C_Commands.FileClosed)
  def onFileClosed(uri: String): Unit = {
    coordinator.logger.debug("On closing file")
    coordinator.removeFileIfExists(uri)
  }

  @JsonRequest(C2S_Commands.RemoveDiagnostics)
  def onRemoveDiagnostics(uri: String): CompletionStage[Boolean] = {
    coordinator.logger.debug("On removing diagnostics")
    coordinator.resetDiagnostics(uri)
    CompletableFuture.completedFuture(true)
  }

  @JsonRequest("GetLanguageServerUrl")
  def onGetServerUrl(): CompletionStage[String] = {
    coordinator.logger.debug("On getting server URL")
    CompletableFuture.completedFuture(serverUrl)
  }

  @JsonNotification(C2S_Commands.StartBackend)
  def onStartBackend(backend: String): Unit = {
    coordinator.logger.debug("On starting backend")
    if (backend == "Silicon" || backend == "Carbon") {
      val backendProps = BackendProperties(name = s"Viper$backend", backend_type = backend)
      coordinator.changeBackendIfNecessary(backendProps)
    } else {
      coordinator.hint(s"cannot start unknown backend $backend")
    }
  }

  @JsonNotification(C2S_Commands.SwapBackend)
  def onSwapBackend(backend: String): Unit = {
    coordinator.logger.debug("on swapping backend")
    if (backend == "Silicon" || backend == "Carbon") {
      val backendProps = BackendProperties(name = s"Viper$backend", backend_type = backend)
      coordinator.changeBackendIfNecessary(backendProps)
    } else {
      coordinator.hint(s"cannot swap backend to unknown backend $backend")
    }
  }

  @JsonRequest(C2S_Commands.RequestBackendNames)
  def onGetNames(backendName: String): CompletionStage[Array[String]] = {
    coordinator.logger.debug(s"on getting backend names $backendName")
    CompletableFuture.completedFuture(Array("Silicon", "Carbon"))
  }

  @JsonNotification(C2S_Commands.StopBackend)
  def onBackendStop(backendName: String): Unit= {
    coordinator.logger.debug(s"on stopping backend $backendName")
    val params = StateChangeParams(Stopped.id)
    coordinator.sendStateChangeNotification(params, None)
  }

  @JsonNotification(C2S_Commands.Verify)
  def onVerify(data: VerifyRequest): Unit = {
    coordinator.logger.debug("On verifying")
    if (coordinator.canVerificationBeStarted(data.uri, data.manuallyTriggered)) {
      // stop all other verifications because the backend crashes if multiple verifications are run in parallel
      coordinator.stopAllRunningVerifications().map(_ => {
        coordinator.logger.info("start or restart verification")

        val verificationStarted = coordinator.startVerification(data.uri, data.manuallyTriggered)
        if (verificationStarted) {
          coordinator.logger.info("Verification Started")
        } else {
          coordinator.client.notifyVerificationNotStarted(data.uri)
        }
      }).recover(e => {
        coordinator.logger.debug(s"Error handling verify request: $e")
        coordinator.client.notifyVerificationNotStarted(data.uri)
      })
    } else {
      coordinator.logger.info("The verification cannot be started.")
      coordinator.client.notifyVerificationNotStarted(data.uri)
    }
  }

  @JsonNotification(C2S_Commands.FlushCache)
  def onFlushCache(file: String): Unit = {
    coordinator.logger.debug("flushing cache...")
    val file_arg: Option[String] = Option(file)
    coordinator.flushCache(file_arg)
  }

  @JsonNotification(C2S_Commands.StopVerification)
  def onStopVerification(uri: String): CompletionStage[Boolean] = {
    coordinator.logger.debug("on stopping verification")
    try {
      coordinator.stopRunningVerification(uri).asJava
    } catch {
      case e: Throwable =>
        coordinator.logger.debug(s"Error handling stop verification request (critical): $e")
        CompletableFuture.completedFuture(false)
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
