package viper.server

import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters._

import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionList, CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializeResult, MessageParams, MessageType, ServerCapabilities, TextDocumentSyncKind}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}

class ViperLanguageServer extends LanguageClientAware {
  private var _client: Option[LanguageClient] = None

  //==========================================================================//
  //            ADD MORE METHODS TO UNDERSTAND CLIENT HERE                    //
  //==========================================================================//

  @JsonNotification("sbi/hi")
  def saidHi(params: DidSaveTextDocumentParams): Unit = {
    println("said hi")
    _client match {
      case Some(c) => c.showMessage(new MessageParams(MessageType.Info, "Hi back <3"))
      case _ =>
    }
  }

  @JsonNotification("textDocument/didChange")
  def didChange(params: DidChangeTextDocumentParams): Unit = {
    println("didChange")
    _client match {
      case Some(c) =>
        c.showMessage(new MessageParams(MessageType.Log, s"Huuuhuuuuu"))
      case _ =>
    }
  }

  @JsonRequest(value = "initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    println("initialize")
    val capabilities = new ServerCapabilities()
    // always send full text document for each notification:
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setCompletionProvider(new CompletionOptions(true, null))
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonRequest(value = "shutdown")
  def shutdown(): CompletableFuture[AnyRef] = {
    println("shutdown")
    CompletableFuture.completedFuture(null)
  }

  @JsonNotification(value = "exit")
  def exit(): Unit = {
    println("exit")
    sys.exit()
  }


  @JsonRequest("textDocument/completion")
  def completion(params: CompletionParams): CompletableFuture[CompletionList] = {
    val tsItem = new CompletionItem("TypeScript")
    tsItem.setKind(CompletionItemKind.Text)
    tsItem.setData(1)
    val jsItem = new CompletionItem("JavaScript")
    jsItem.setKind(CompletionItemKind.Text)
    jsItem.setData(2)
    val completions = new CompletionList(List(tsItem, jsItem).asJava)
    CompletableFuture.completedFuture(completions)
  }

  @JsonRequest("completionItem/resolve")
  def completionItemResolve(item: CompletionItem): CompletableFuture[CompletionItem] = {
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
    CompletableFuture.completedFuture(item)
  }

  override def connect(client: LanguageClient): Unit = {
    println("Connecting plugin client.")
    _client = Some(client)
  }
}
