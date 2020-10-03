package viper.server

import LogLevel._
import java.util.concurrent.{CompletableFuture => CFuture}

import scala.collection.JavaConverters._
import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionList, CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbolParams, InitializeParams, InitializeResult, Location, MessageParams, MessageType, Range, ServerCapabilities, ShowMessageRequestParams, SymbolInformation, TextDocumentPositionParams, TextDocumentSyncKind}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

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
          //notify client;
          Coordinator.client.notifyFileOpened(uri)
          if (!Coordinator.verificationTasks.contains(uri)) {
            //create new task for opened file
            val task = new VerificationTask(uri)
            Coordinator.verificationTasks += (uri -> task)
          }
        }
      })
    } catch {
      case e: Throwable => Log.debug("Error handling TextDocument opened")
    }
  }

  @JsonNotification("textDocument/didChange")
  def onDidChangeDocument(params: DidChangeTextDocumentParams): Unit = {
    val task_opt = Coordinator.verificationTasks.get(params.getTextDocument.getUri)
    val task: VerificationTask = task_opt.getOrElse(return)
    task.symbolInformation = ArrayBuffer()
    task.definitions = ArrayBuffer()
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
    val task_opt = Coordinator.verificationTasks.get(uri)
    val task = task_opt.getOrElse(return)
    task.resetDiagnostics()
    Coordinator.verificationTasks -= uri
  }

  @JsonRequest("textDocument/documentSymbol")
  def onGetDocumentSymbol(params: DocumentSymbolParams): CFuture[SymbolInformation] = {
    val task_opt = Coordinator.verificationTasks.get(params.getTextDocument.getUri)
    val task = task_opt.getOrElse(return CFuture.completedFuture(Array()))
    CFuture.completedFuture(task.symbolInformation)
  }

  @JsonRequest("textDocument/definition")
  def onGetDefinition(params: TextDocumentPositionParams): CFuture[List[Location]] = {
    Log.log("Handling definitions request for params: " + params.toString, LogLevel.Debug)
    val document = params.getTextDocument
    val pos = params.getPosition
    val task_opt = Coordinator.verificationTasks.get(params.getTextDocument.getUri)

    task_opt match {
      case Some(task) =>
        Log.log("Found verification task for URI " + document.getUri, LogLevel.LowLevelDebug)
        Coordinator.client.requestIdentifier(pos).thenApply(word => {
          Log.log("Got word: " + word, LowLevelDebug)
          task.definitions.filter(d => (d.scope.getStart == null) //is global
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
    val task_opt = Coordinator.verificationTasks.get(uri)
    val task = task_opt.getOrElse(return CFuture.completedFuture(false))
    task.resetDiagnostics()
    CFuture.completedFuture(true)
  }

  @JsonRequest("GetLanguageServerUrl")
  def onGetServerUrl(): CFuture[String] = {
    CFuture.completedFuture(Coordinator.getAddress())
  }

  @JsonNotification(C2S_Commands.SwapBackend)
  def onSwapBackend(backendName: String): Unit = {
    try {
      Coordinator.backendService.swapBackend(Settings.getBackend(backendName));
    } catch {
      case e: Throwable => Log.debug("Error handling swap backend request: " + e);
    }
  }

  @JsonNotification(C2S_Commands.Verify)
  def onVerify(data: VerifyRequest): Unit = {
    //it does not make sense to reverify if no changes were made and the verification is already running
    if (Coordinator.canVerificationBeStarted(data.uri, data.manuallyTriggered)) {
      Settings.workspace = data.workspace
      Log.log("start or restart verification", LogLevel.Info);
      //stop all other verifications because the backend crashes if multiple verifications are run in parallel
      Coordinator.stopAllRunningVerifications().thenAccept(_ => {
        //start verification
        Coordinator.executedStages = ArrayBuffer()
        val hasVerificationstarted = Coordinator.verificationTasks
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
//    _client match {
//      case Some(c) => c.showMessage(new MessageParams(MessageType.Info, s"$file got flushed :>"))
//      case _ =>
//    }
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
      case n: JsonPrimitive if n.getAsInt == 1 => {
        item.setDetail("TypeScript details")
        item.setDocumentation("TypeScript documentation")
      }
      case n: JsonPrimitive if n.getAsInt == 2 => {
        item.setDetail("JavaScript details")
        item.setDocumentation("JavaScript documentation")
      }
      case _ => item.setDetail(s"${data.toString} is instance of ${data.getClass}")
    }
    CFuture.completedFuture(item)
  }

  override def connect(client: LanguageClient): Unit = {
    println("Connecting plugin client.")
    val c = client.asInstanceOf[IdeLanguageClient]
    Coordinator.client = c
  }
}
