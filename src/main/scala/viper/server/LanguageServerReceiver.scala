package viper.server

import java.util.concurrent.{CompletableFuture => CFuture}

import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware}
import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams, InitializeParams, InitializeResult, Location, Range, ServerCapabilities, SymbolInformation, TextDocumentPositionParams}
import viper.server.LogLevel._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class LanguageServerReceiver extends LanguageClientAware {

  @JsonRequest(value = "initialize")
  def initialize(params: InitializeParams): CFuture[InitializeResult] = {
    println("initialize")
    val capabilities = new ServerCapabilities()
    //    OG
    //    always send full text document for each notification:
    //    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    //    capabilities.setCompletionProvider(new CompletionOptions(true, null))

    capabilities.setDefinitionProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    CFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonNotification("textDocument/didChange")
  def onDidOpenDocument(params: DidOpenTextDocumentParams): Unit = {
    try {
      val uri = params.getTextDocument.getUri
      Common.isViperSourceFile(uri).thenAccept(isViperFile => {
        if (isViperFile) {
          //notify client
          Coordinator.client.notifyFileOpened(uri)
          if (!Coordinator.files.contains(uri)) {
            //create new task for opened file
            val manager = new FileManager(uri)
            Coordinator.files += (uri -> manager)
          }
        }
      })
    } catch {
      case e: Throwable => Log.debug("Error handling TextDocument opened")
    }
  }

  @JsonNotification("textDocument/didChange")
  def onDidChangeDocument(params: DidChangeTextDocumentParams): Unit = {
    val manager_opt = Coordinator.files.get(params.getTextDocument.getUri)
    val manager: FileManager = manager_opt.getOrElse(return)
    manager.symbolInformation = ArrayBuffer()
    manager.definitions = ArrayBuffer()
  }

  @JsonNotification("textDocument/didClose")
  def onDidCloseDocument(params: DidCloseTextDocumentParams): Unit = {
    try {
      val uri = params.getTextDocument.getUri
      Common.isViperSourceFile(uri).thenAccept(isViperFile => {
        if (isViperFile) Coordinator.client.notifyFileClosed(uri)
      })
    } catch {
      case e: Throwable => Log.debug("Error handling TextDocument opened")
    }
  }

  @JsonNotification(S2C_Commands.FileClosed)
  def onFileClosed(uri: String): Unit = {
    val manager_opt = Coordinator.files.get(uri)
    val manager = manager_opt.getOrElse(return)
    manager.resetDiagnostics()
    Coordinator.files -= uri
  }

  @JsonRequest("textDocument/documentSymbol")
  def onGetDocumentSymbol(params: DocumentSymbolParams): CFuture[List[SymbolInformation]] = {
    var symbolInfo_list: List[SymbolInformation] = List()
    val manager_opt = Coordinator.files.get(params.getTextDocument.getUri)
    val manager = manager_opt.getOrElse(return CFuture.completedFuture(symbolInfo_list))
    symbolInfo_list = manager.symbolInformation.toList
    CFuture.completedFuture(symbolInfo_list)
  }

  @JsonRequest("textDocument/definition")
  def onGetDefinition(params: TextDocumentPositionParams): CFuture[List[Location]] = {
    Log.log("Handling definitions request for params: " + params.toString, LogLevel.Debug)
    val document = params.getTextDocument
    val pos = params.getPosition
    val manager_opt = Coordinator.files.get(params.getTextDocument.getUri)

    manager_opt match {
      case Some(manager) =>
        Log.log("Found verification task for URI " + document.getUri, LogLevel.LowLevelDebug)
        Coordinator.client.requestIdentifier(pos).thenApply(word => {
          Log.log("Got word: " + word, LowLevelDebug)
          manager.definitions.filter(d => (d.scope.getStart == null) //is global
                            || (Common.comparePosition(d.scope.getStart, pos) <= 0
                            && Common.comparePosition(d.scope.getEnd, pos) >= 0)) // in scope
                          .filter(d => word == d.name)
                          .map(d => new Location(document.getUri, new Range(d.code_location, d.code_location)))
                          .toList
        })
      case None =>
        // No definition found - maybe it's a keyword.
        val e = s"Verification task not found for URI ${document.getUri}"
        Log.debug(e)
        CFuture.failedFuture(new Throwable(e)) // needs to return some CF.
    }
  }

  @JsonRequest(C2S_Commands.RemoveDiagnostics)
  def onRemoveDiagnostics(uri: String): CFuture[Boolean] = {
    val manager_opt = Coordinator.files.get(uri)
    val manager = manager_opt.getOrElse(return CFuture.completedFuture(false))
    manager.resetDiagnostics()
    CFuture.completedFuture(true)
  }

  @JsonRequest("GetLanguageServerUrl")
  def onGetServerUrl(): CFuture[String] = {
    CFuture.completedFuture(Coordinator.getAddress)
  }

  @JsonNotification(C2S_Commands.SwapBackend)
  def onSwapBackend(backendName: String): Unit = {
    try {
      val b = BackendProperties(
              "new Backend", backendName, null, null,
                      5000, null, 5000, null)
      Coordinator.verifier.swapBackend(b)
    } catch {
      case e: Throwable => Log.debug("Error handling swap backend request: " + e)
    }
  }

  @JsonNotification(C2S_Commands.Verify)
  def onVerify(data: VerifyRequest): Unit = {
    //it does not make sense to reverify if no changes were made and the verification is already running
    if (Coordinator.canVerificationBeStarted(data.uri, data.manuallyTriggered)) {
//      Settings.workspace = data.workspace
      Log.log("start or restart verification", LogLevel.Info)
      //stop all other verifications because the backend crashes if multiple verifications are run in parallel
      Coordinator.stopAllRunningVerifications().thenAccept(_ => {
        //start verification
        Coordinator.executedStages = ArrayBuffer()
        val hasVerificationstarted = Coordinator.files
                                                .getOrElse(data.uri, return)
                                                .verify(data.manuallyTriggered)
        if (!hasVerificationstarted) {
          Coordinator.client.notifyVerificationNotStarted(data.uri)
        }
      }).exceptionally(_ => {
        Log.debug("Error handling verify request")
        Coordinator.client.notifyVerificationNotStarted(data.uri)
        null //java void
      })
    } else {
      Log.log("The verification cannot be started.", LogLevel.Info)
      Coordinator.client.notifyVerificationNotStarted(data.uri)
    }
  }

  @JsonNotification(C2S_Commands.FlushCache)
  def flushCache(file: String): Unit = {
    println("flushing cache...")
  }

  @JsonRequest(value = "shutdown")
  def shutdown(): CFuture[AnyRef] = {
    println("shutdown")
    CFuture.completedFuture(null)
  }

  @JsonNotification(value = "exit")
  def exit(): Unit = {
    println("exit")
    sys.exit()
  }

  @JsonRequest("textDocument/completion")
  def completion(params: CompletionParams): CFuture[CompletionList] = {
    val tsItem = new CompletionItem("TypeScript")
    tsItem.setKind(CompletionItemKind.Text)
    tsItem.setData(1)
    val jsItem = new CompletionItem("JavaScript")
    jsItem.setKind(CompletionItemKind.Text)
    jsItem.setData(2)
    val completions = new CompletionList(List(tsItem, jsItem).asJava)
    CFuture.completedFuture(completions)
  }

  @JsonRequest("completionItem/resolve")
  def completionItemResolve(item: CompletionItem): CFuture[CompletionItem] = {
    val data: Object = item.getData
    data match {
      case n: JsonPrimitive if n.getAsInt == 1 =>
        item.setDetail("TypeScript details")
        item.setDocumentation("TypeScript documentation")
      case n: JsonPrimitive if n.getAsInt == 2 =>
        item.setDetail("JavaScript details")
        item.setDocumentation("JavaScript documentation")
      case _ =>
        item.setDetail(s"${data.toString} is instance of ${data.getClass}")
    }
    CFuture.completedFuture(item)
  }

  override def connect(client: LanguageClient): Unit = {
    println("Connecting plugin client.")
    val c = client.asInstanceOf[IdeLanguageClient]
    Coordinator.client = c
  }
}
