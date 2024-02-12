// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.jsonrpc.messages.{Either, Either3}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j._
import viper.server.ViperConfig
import viper.server.core.VerificationExecutionContext
import viper.viperserver.BuildInfo

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._
import scala.language.postfixOps

trait StandardReceiver {
  val coordinator: ClientCoordinator
  implicit def executor: VerificationExecutionContext
}

trait LanguageReceiver extends StandardReceiver with LanguageServer {

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    coordinator.logger.debug(s"[Req: initialize] ${params.toString()}")
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    // Features https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures
    // Go to Declaration:       [N/A]
    // Go to Definition:
    capabilities.setDefinitionProvider(true)
    // Go to Type Definition:   [N/A]
    // Go to Implementation:    [N/A]
    // Find References:
    capabilities.setReferencesProvider(true)
    // Prepare Call Hierarchy:  [N/A]
      // Incoming Calls:        [N/A]
      // Outgoing Calls:        [N/A]
    // Prepare Type Hierarchy:  [N/A]
      // Super Types:           [N/A]
      // Sub Types:             [N/A]
    // Document Highlight:
    capabilities.setDocumentHighlightProvider(true)
    // Document Link (disabled Resolve):
    capabilities.setDocumentLinkProvider(new DocumentLinkOptions(false))
    // Hover:
    capabilities.setHoverProvider(true)
    // Code Lens (disabled Resolve):
    capabilities.setCodeLensProvider(new CodeLensOptions(false))
    // Folding Range:
    capabilities.setFoldingRangeProvider(true)
    // Selection Range:         [N/A]
        // could try extracting from document symbols,
        // but this req doesn't seem to ever be sent
    // Document Symbols:
    capabilities.setDocumentSymbolProvider(true)
    // Semantic Tokens:
    val legend = new SemanticTokensLegend(
      Seq(SemanticTokenTypes.Namespace, SemanticTokenTypes.Type, SemanticTokenTypes.Class, SemanticTokenTypes.Enum,
          SemanticTokenTypes.Interface, SemanticTokenTypes.Struct, SemanticTokenTypes.TypeParameter, SemanticTokenTypes.Parameter,
          SemanticTokenTypes.Variable, SemanticTokenTypes.Property, SemanticTokenTypes.EnumMember, SemanticTokenTypes.Event,
          SemanticTokenTypes.Function, SemanticTokenTypes.Method, SemanticTokenTypes.Macro, SemanticTokenTypes.Keyword,
          SemanticTokenTypes.Modifier, SemanticTokenTypes.Comment, SemanticTokenTypes.String, SemanticTokenTypes.Number,
          SemanticTokenTypes.Regexp, SemanticTokenTypes.Operator, SemanticTokenTypes.Decorator, "constant").asJava,
      Seq(SemanticTokenModifiers.Declaration, SemanticTokenModifiers.Definition, SemanticTokenModifiers.Readonly, SemanticTokenModifiers.Static,
          SemanticTokenModifiers.Deprecated, SemanticTokenModifiers.Abstract, SemanticTokenModifiers.Async, SemanticTokenModifiers.Modification,
          SemanticTokenModifiers.Documentation, SemanticTokenModifiers.DefaultLibrary, "controlFlow").asJava
    )
    capabilities.setSemanticTokensProvider(new SemanticTokensWithRegistrationOptions(legend, true))
    // Inline Value:            [N/A]
    // Inlay Hint (disabled Resolve):
    capabilities.setInlayHintProvider(true)
    // Moniker:                 [N/A]
    // Completion Proposals:
    capabilities.setCompletionProvider(new CompletionOptions(false, Seq(".", ":", "(", "[", "{", "\n", "\t").asJava))
    // Pull Diagnostics:        DISABLED (we use `publishDiagnostics` instead)
    // capabilities.setDiagnosticProvider(new DiagnosticRegistrationOptions(true, false))
    // Signature Help:
        // Allow a `,` to try and restart the signature help even after it has ended
    capabilities.setSignatureHelpProvider(new SignatureHelpOptions(Seq("(", ",").asJava, Seq().asJava))
    // Code Action:
    capabilities.setCodeActionProvider(true)
    // Document Color:          [N/A]
    // Color Presentation:      [N/A]
    // Formatting:              TODO
    // capabilities.setDocumentFormattingProvider(true)
    // Range Formatting:        TODO
    // On type Formatting:      [N/A]
    // Rename & Prepare Rename:
    capabilities.setRenameProvider(new RenameOptions(true))
    // Linked Editing Range:    [N/A]
    // Disabled for now, in VSCode requires that `editor.linkedEditing` is enabled.
    // This acts like "Change All Occurrences" but without any explicit action. With the
    // current implementation this would mean that renaming a definition would
    // automatically cause all references to be renamed. This would probably be too annoying.
    capabilities.setLinkedEditingRangeProvider(false)

    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def initialized(params: InitializedParams): Unit = {
    coordinator.logger.trace(s"[Req: initialized] ${params.toString()}")
  }

  override def exit(): Unit = {
    coordinator.logger.trace("[Req: exit]")
    coordinator.exit()
  }

  // 'shutdown' is called before 'exit'
  override def shutdown(): CompletableFuture[AnyRef] = {
    coordinator.logger.trace("[Req: shutdown]")
    // we instruct the server to stop all verifications but we only wait for 2s for their completion since
    // the LSP client expects a response within 3s
    val stopVerifications = coordinator.stopAllRunningVerifications()
    val timeout = akka.pattern.after(2 seconds)(Future.unit)(executor.actorSystem)
    Future.firstCompletedOf(Seq(stopVerifications, timeout)).asJava.thenApply(_ => null.asInstanceOf[AnyRef]).toCompletableFuture()
  }

  override def setTrace(params: SetTraceParams): Unit = {
    coordinator.logger.trace(s"[Req: $$/setTrace] ${params.toString()}")
  }

  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = {
    coordinator.logger.trace(s"[Req: window/workDoneProgress/cancel] ${params.toString()}")
  }
}

trait TextDocumentReceiver extends StandardReceiver with TextDocumentService {

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    // This sends the entire file contents, so `trace` to avoid spam
    coordinator.logger.trace(s"[Req: textDocument/didOpen] ${params.toString()}")
    val uri: String = params.getTextDocument.getUri
    try {
      coordinator.isViperSourceFile(uri).map(_ => {
        //create new task for opened file
        coordinator.ensureFmExists(uri, params.getTextDocument.getText)
      })
    } catch {
      case e: Throwable => coordinator.logger.debug(s"Error handling TextDocument opened: $e")
    }
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    coordinator.logger.trace(s"[Req: textDocument/didChange] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    for (cc <- params.getContentChanges.asScala) {
      val (range, text) = (cc.getRange(), cc.getText())
      coordinator.handleChange(uri, range, text)
    }
    coordinator.startParseTypecheck(uri)
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    coordinator.logger.trace(s"[Req: textDocument/didClose] ${params.toString()}")
    try {
      val uri = params.getTextDocument.getUri
      coordinator.closeFile(uri)
    } catch {
      case _: Throwable => coordinator.logger.debug("Error handling TextDocument opened")
    }
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    coordinator.logger.trace(s"[Req: textDocument/didSave] ${params.toString()}")
    // coordinator.resetFileInfo(params.getTextDocument.getUri)
  }

  //////////////////
  // Optional
  //////////////////

  override def documentSymbol(params: DocumentSymbolParams) = {
    // This happens for every edit, so `trace` to avoid spam
    coordinator.logger.trace(s"[Req: textDocument/documentSymbol] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val ds = coordinator.getRoot(uri).getDocumentSymbols(uri)
    val symbolsEither = ds.map(_.map(Either.forRight[SymbolInformation, DocumentSymbol](_)).asJava)
    symbolsEither.asJava.toCompletableFuture
  }

  override def definition(params: DefinitionParams) = {
    coordinator.logger.trace(s"[Req: textDocument/definition] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    coordinator.getRoot(uri).getGotoDefinitions(uri, pos).map(defns => {
        Either.forRight[java.util.List[_ <: Location], java.util.List[_ <: LocationLink]](defns.asJava)
    }).asJava.toCompletableFuture
  }

  override def hover(params: HoverParams) = {
    // This happens for every hover, so `trace` to avoid spam
    coordinator.logger.trace(s"[Req: textDocument/hover] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    coordinator.getRoot(uri)
      .getHoverHints(uri, pos)
      .map(_.head)
      .asJava.toCompletableFuture
  }

  override def foldingRange(params: FoldingRangeRequestParams) = {
    coordinator.logger.trace(s"[Req: textDocument/foldingRange] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val fr = coordinator.getRoot(uri).getFoldingRanges(uri)
    val foldingRanges = fr.map(_.asJava)
    foldingRanges.asJava.toCompletableFuture
  }

  override def semanticTokensFull(params: SemanticTokensParams) = {
    coordinator.logger.trace(s"[Req: textDocument/semanticTokens/full] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val st = coordinator.getRoot(uri).getSemanticHighlights(uri)
    val semanticTokens = st.map(st => new SemanticTokens(st.flatMap(_.toSeq()).asJava))
    semanticTokens.asJava.toCompletableFuture
  }

  override def inlayHint(params: InlayHintParams) = {
    // This happens for every scroll, so `trace` to avoid spam
    coordinator.logger.trace(s"[Req: textDocument/inlayHint] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val ih = coordinator.getRoot(uri).getInlayHints(uri)
    val range = params.getRange
    val inlayHints = ih.map(_.filter(ih => Common.containsPosition(range, ih.getPosition) == 0).asJava)
    inlayHints.asJava.toCompletableFuture
  }

  override def codeLens(params: CodeLensParams) = {
    coordinator.logger.trace(s"[Req: textDocument/codeLens] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val cl = coordinator.getRoot(uri).getCodeLens(uri)
    val CodeLens = cl.map(_.asJava.asInstanceOf[java.util.List[_ <: CodeLens]])
    CodeLens.asJava.toCompletableFuture
  }

  override def signatureHelp(params: SignatureHelpParams) = {
    coordinator.logger.trace(s"[Req: textDocument/signatureHelp] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val ctx = Option(params.getContext)
    val isRetrigger = ctx.map(_.isRetrigger).getOrElse(false)
    val help = coordinator.getRoot(uri).getSignatureHelp(uri, params.getPosition, isRetrigger)
    ctx.flatMap(ctx => Option(ctx.getActiveSignatureHelp)).flatMap(active => help.map((active, _))).foreach {
      case (active, help) =>
        if (active.getActiveSignature < help.getSignatures.size() && active.getActiveSignature != active.getSignatures.size() - 1) {
          help.setActiveSignature(active.getActiveSignature)
        }
    }
    CompletableFuture.completedFuture(help.orNull)
  }

  override def references(params: ReferenceParams) = {
    coordinator.logger.trace(s"[Req: textDocument/references] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val id = params.getContext.isIncludeDeclaration
    val refs = coordinator.getRoot(uri).getFindReferences(uri, params.getPosition, id)
    refs.map(_.asJava.asInstanceOf[java.util.List[_ <: Location]]).asJava.toCompletableFuture
  }

  override def prepareRename(params: PrepareRenameParams) = {
    coordinator.logger.trace(s"[Req: textDocument/prepareRename] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    coordinator.getRoot(uri).getFindReferences(uri, pos, true).map(refs => {
      refs.filter(_.getUri == uri).find(r => Common.containsPosition(r.getRange, pos) == 0).map(r =>
        Either3.forFirst[Range, PrepareRenameResult, PrepareRenameDefaultBehavior](r.getRange)
      ).orNull
    }).asJava.toCompletableFuture
  }

  override def rename(params: RenameParams) = {
    coordinator.logger.trace(s"[Req: textDocument/rename] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val newName = params.getNewName
    val rename = coordinator.getRoot(uri).getRename(uri, pos, newName)
    rename.asJava.toCompletableFuture
  }

  override def documentHighlight(params: DocumentHighlightParams) = {
    coordinator.logger.trace(s"[Req: textDocument/documentHighlight] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    coordinator.getRoot(uri).getFindReferences(uri, pos, true).map(refs =>
      refs.filter(_.getUri == uri).map(r => new DocumentHighlight(r.getRange, DocumentHighlightKind.Read))
        .asJava.asInstanceOf[java.util.List[_ <: DocumentHighlight]]
    ).asJava.toCompletableFuture
  }

  override def documentLink(params: DocumentLinkParams) = {
    coordinator.logger.trace(s"[Req: textDocument/documentLink] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val links = coordinator.getRoot(uri).getDocumentLinks(uri)
    links.map(_.asJava).asJava.toCompletableFuture
  }

  override def completion(params: CompletionParams) = {
    coordinator.logger.trace(s"[Req: textDocument/completion] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val char = params.getContext.getTriggerCharacter
    val proposals = coordinator.getRoot(uri).getCompletionProposal(uri, pos, Option(char))
    val jProposals = proposals.map(p => Either.forLeft[java.util.List[CompletionItem], CompletionList](p.asJava))
    jProposals.asJava.toCompletableFuture
  }

  override def codeAction(params: CodeActionParams) = {
    // TODO
    CompletableFuture.completedFuture(Nil.asJava)
  }

  // --- DISABLED, see comment in `initialize` ---
  override def diagnostic(params: DocumentDiagnosticParams) = {
    coordinator.logger.trace(s"[Req: textDocument/diagnostic] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val ds = coordinator.getRoot(uri).getDiagnostics(uri)
    ds.map(ds => {
      val diagnostics = new RelatedFullDocumentDiagnosticReport(ds.asJava)
      // Maybe use this in future LSP versions?
      // diagnostics.setRelatedDocuments
      new DocumentDiagnosticReport(diagnostics)
    }).asJava.toCompletableFuture
  }

  // --- DISABLED, see comment in `initialize` ---
  override def linkedEditingRange(params: LinkedEditingRangeParams) = {
    coordinator.logger.trace(s"[Req: textDocument/linkedEditingRange] ${params.toString()}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    coordinator.getRoot(uri).getFindReferences(uri, pos, true, false).map(refs =>
      new LinkedEditingRanges(refs.filter(_.getUri == uri).map(_.getRange).asJava)
    ).asJava.toCompletableFuture
  }
}

trait WorkspaceReceiver extends StandardReceiver with WorkspaceService {

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    coordinator.logger.trace(s"[Req: workspace/didChangeConfiguration] ${params.toString()}")
  }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    coordinator.logger.trace(s"[Req: workspace/didChangeWatchedFiles] ${params.toString()}")
  }
}

class CustomReceiver(config: ViperConfig, server: ViperServerService, serverUrl: String)(implicit _executor: VerificationExecutionContext)
    extends LanguageClientAware with LanguageReceiver with WorkspaceReceiver with TextDocumentReceiver {

  override val coordinator = new ClientCoordinator(server)
  override def executor: VerificationExecutionContext = _executor

  override def getTextDocumentService(): TextDocumentService = this

  override def getWorkspaceService(): WorkspaceService = this

  val MIN_CLIENT_VERSION = "5.0.0"

  // receives version of client to perform some checks before replying with the server's version
  @JsonRequest(C2S_Commands.GetVersion)
  def onGetVersion(request: GetVersionRequest): CompletableFuture[GetVersionResponse] = {
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
  def onRemoveDiagnostics(request: RemoveDiagnosticsRequest): CompletableFuture[RemoveDiagnosticsResponse] = {
    coordinator.logger.debug("On removing diagnostics")
    coordinator.resetDiagnostics(request.uri)
    CompletableFuture.completedFuture(RemoveDiagnosticsResponse(true))
  }

  @JsonRequest(C2S_Commands.GetLanguageServerUrl)
  def onGetServerUrl(): CompletableFuture[GetLanguageServerUrlResponse] = {
    coordinator.logger.debug("On getting server URL")
    CompletableFuture.completedFuture(GetLanguageServerUrlResponse(serverUrl))
  }

  @JsonNotification(C2S_Commands.Verify)
  def onVerify(data: VerifyParams): Unit = {
    coordinator.logger.debug("On verifying")
    if (coordinator.canVerificationBeStarted(data.uri, data.content, data.manuallyTriggered)) {
      // stop all other verifications because the backend crashes if multiple verifications are run in parallel
      coordinator.logger.trace("verification can be started - all running verifications are now going to be stopped")
      coordinator.stopAllRunningVerifications().map(_ => {
        coordinator.logger.info("start or restart verification")

        coordinator.startVerification(data.backend, data.customArgs, data.uri, data.manuallyTriggered).map(verificationStarted => {
          if (verificationStarted) {
            coordinator.logger.info("Verification Started")
          } else {
            coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(data.uri))
          }
        })
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
  def onFlushCache(params: FlushCacheParams): CompletableFuture[Unit] = {
    coordinator.logger.debug("flushing cache...")
    val uriOpt = Option(params.uri)
    val backendOpt = Option(params.backend)
    coordinator.flushCache(uriOpt, backendOpt).asJava.toCompletableFuture()
  }

  @JsonRequest(C2S_Commands.StopVerification)
  def onStopVerification(request: StopVerificationRequest): CompletableFuture[StopVerificationResponse] = {
    coordinator.logger.debug("on stopping verification")
    try {
      coordinator.stopRunningVerification(request.uri)
        .map(success => StopVerificationResponse(success))
        .asJava.toCompletableFuture()
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
