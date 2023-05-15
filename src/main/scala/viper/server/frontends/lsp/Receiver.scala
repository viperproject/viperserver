// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j._
import viper.server.ViperConfig
import viper.server.core.VerificationExecutionContext
import viper.viperserver.BuildInfo

import scala.annotation.unused
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

  override def initialize(@unused params: InitializeParams): CompletableFuture[InitializeResult] = {
    coordinator.logger.info("initialize")
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setHoverProvider(true)
    capabilities.setFoldingRangeProvider(true)
    // Allow a `,` to try and restart the signature help even after it has ended
    capabilities.setSignatureHelpProvider(new SignatureHelpOptions(Seq("(", ",").asJava, Seq().asJava))
    val legend = new SemanticTokensLegend(
      Seq(SemanticTokenTypes.Namespace, SemanticTokenTypes.Type, SemanticTokenTypes.Class, SemanticTokenTypes.Enum,
          SemanticTokenTypes.Interface, SemanticTokenTypes.Struct, SemanticTokenTypes.TypeParameter, SemanticTokenTypes.Parameter,
          SemanticTokenTypes.Variable, SemanticTokenTypes.Property, SemanticTokenTypes.EnumMember, SemanticTokenTypes.Event,
          SemanticTokenTypes.Function, SemanticTokenTypes.Method, SemanticTokenTypes.Macro, SemanticTokenTypes.Keyword,
          SemanticTokenTypes.Modifier, SemanticTokenTypes.Comment, SemanticTokenTypes.String, SemanticTokenTypes.Number,
          SemanticTokenTypes.Regexp, SemanticTokenTypes.Operator, SemanticTokenTypes.Decorator).asJava,
      Seq(SemanticTokenModifiers.Declaration, SemanticTokenModifiers.Definition, SemanticTokenModifiers.Readonly, SemanticTokenModifiers.Static,
          SemanticTokenModifiers.Deprecated, SemanticTokenModifiers.Abstract, SemanticTokenModifiers.Async, SemanticTokenModifiers.Modification,
          SemanticTokenModifiers.Documentation, SemanticTokenModifiers.DefaultLibrary).asJava
    )
    capabilities.setSemanticTokensProvider(new SemanticTokensWithRegistrationOptions(legend, true))
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def initialized(@unused params: InitializedParams): Unit = {
    coordinator.logger.info("initialized")
  }

  override def exit(): Unit = {
    coordinator.logger.debug("exit")
    coordinator.exit()
  }

  // 'shutdown' is called before 'exit'
  override def shutdown(): CompletableFuture[AnyRef] = {
    coordinator.logger.debug("shutdown")
    // we instruct the server to stop all verifications but we only wait for 2s for their completion since
    // the LSP client expects a response within 3s
    val stopVerifications = coordinator.stopAllRunningVerifications()
    val timeout = akka.pattern.after(2 seconds)(Future.unit)(executor.actorSystem)
    Future.firstCompletedOf(Seq(stopVerifications, timeout)).asJava.thenApply(_ => null.asInstanceOf[AnyRef]).toCompletableFuture()
  }
}

trait TextDocumentReceiver extends StandardReceiver with TextDocumentService {

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
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

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    coordinator.logger.info("On changing document")
    val uri = params.getTextDocument.getUri
    for (cc <- params.getContentChanges.asScala) {
      val (range, text) = (cc.getRange(), cc.getText())
      coordinator.handleChange(uri, range, text)
    }
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    coordinator.logger.info("On closing document")
    try {
      val uri = params.getTextDocument.getUri
      coordinator.removeFileIfExists(uri)
    } catch {
      case _: Throwable => coordinator.logger.debug("Error handling TextDocument opened")
    }
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    coordinator.logger.info("On saving document")
    coordinator.resetFileInfo(params.getTextDocument.getUri)
  }

  //////////////////
  // Optional
  //////////////////

  override def documentSymbol(params: DocumentSymbolParams) = {
    coordinator.logger.info("On getting document symbol")
    val symbols = coordinator.getSymbolsForFile(params.getTextDocument.getUri)
    val list = symbols.map(Either.forRight[SymbolInformation, DocumentSymbol](_)).asJava
    CompletableFuture.completedFuture(list)
  }

  override def definition(params: DefinitionParams) = {
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val definitions = coordinator.getDefinitionsForPos(uri, pos)
    coordinator.logger.debug(s"Handling definitions request for params: ${params}. Have ${definitions.size} possible definitions")
    if (definitions.isEmpty) {
      CompletableFuture.completedFuture(null)
    } else {
      // TODO double check whether this algorithm makes any sense when we support definition lookup
      coordinator.client.requestIdentifier(pos).thenApply(word => {
        coordinator.logger.trace(s"Got word: $word")
        val matching_def = definitions.find(d => word.identifier == d.name)
        val optLocation = matching_def.map(d => {
          val start = d.code_location.getStart()
          new Location(d.uri, new Range(start, start))
        })
        Either.forLeft(optLocation.toList.asJava)
      })
    }
  }

  override def hover(params: HoverParams) = {
    // coordinator.logger.debug(s"Handling hover request for params: ${params}")
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val definitions = coordinator.getDefinitionsForPos(uri, pos)
    if (definitions.isEmpty) {
      CompletableFuture.completedFuture(null)
    } else {
      coordinator.client.requestIdentifier(pos).thenApply(word => {
        // coordinator.logger.trace(s"Got word: $word")
        val matching_def = definitions.find(d => word.identifier == d.name)
        val hover = matching_def.map(d => new Hover(new MarkupContent("markdown", s"```\n${d.hover}\n```")))
        hover.getOrElse(null)
      })
    }
  }

  override def foldingRange(params: FoldingRangeRequestParams) = {
    coordinator.logger.info("On getting folding ranges")
    val foldingRanges = coordinator.getFoldingRangesForFile(params.getTextDocument.getUri)
    CompletableFuture.completedFuture(foldingRanges.asJava)
  }

  override def semanticTokensFull(params: SemanticTokensParams) = {
    coordinator.logger.info("On getting semantic tokens")
    val uri = params.getTextDocument.getUri
    var (currLine, currColumn) = (0, 0)
    val semanticTokens = coordinator.getSemanticTokens(uri).flatMap {
      case SemanticToken(start, len, typ, mod) => {
        if (start.getLine != currLine) {
          currColumn = 0
        }
        val (line, column, length): (Integer, Integer, Integer) = (start.getLine - currLine, start.getCharacter - currColumn, len)
        currLine += line
        currColumn += column
        Seq(line, column, length, typ, mod)
      }
    }
    CompletableFuture.completedFuture(new SemanticTokens(semanticTokens.asJava))
  }

  override def signatureHelp(params: SignatureHelpParams): CompletableFuture[SignatureHelp] = {
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val ctx = params.getContext
    val active = ctx.getActiveSignatureHelp
    coordinator.logger.info(s"On getting signature help, kind ${ctx.getTriggerKind}, char ${ctx.getTriggerCharacter}, active ${active == null}")

    val couldBeStart = ctx.getTriggerKind() == SignatureHelpTriggerKind.TriggerCharacter && ctx.getTriggerCharacter() == "(" && active == null
    val startPos = if (couldBeStart) {
      Some(new Position(pos.getLine, 0))
    } else {
      coordinator.getSignatureHelpStart(uri)
    }
    startPos.flatMap(startPos => if (Common.comparePosition(startPos, pos) < 0) Some(startPos) else None) match {
      case None => CompletableFuture.completedFuture(null)
      case Some(startPos) => {
        val definitions = coordinator.getDefinitionsForPos(uri, startPos).filter(_.signatureHelp.isDefined)
        if (definitions.isEmpty) {
          CompletableFuture.completedFuture(null)
        } else {
          coordinator.client.requestRange(new Range(startPos, pos)).thenApply(res => {
            coordinator.logger.trace(s"Got range: ${res.range}")
            val callsAll = Common.findCallsIn(res.range)
            coordinator.logger.trace(s"Got callsAll: ${callsAll.toString()}")
            val calls = callsAll.flatMap(c =>
              definitions.find(_.name == c._3).map(d => (c._1, c._2, d.signatureHelp.get))
            )
            // coordinator.logger.trace(s"Got calls: ${calls.toString()} (${definitions})")
            if (calls.length == 0) {
              null
            } else {
              if (couldBeStart) {
                val firstCallPos = calls.last._1
                startPos.setLine(startPos.getLine + firstCallPos.getLine)
                // startPos.getCharacter is 0
                startPos.setCharacter(firstCallPos.getCharacter)
                coordinator.registerSignatureHelpStart(uri, startPos)
              }
              val result = new SignatureHelp(Seq().asJava, 0, 0)
              val sigInfos = calls.map { case (_, commas, sigHelp) => {
                val sigInfo = new SignatureInformation(sigHelp.label, "", sigHelp.args.asJava)
                // Can be more than `sigHelp.args.length - 1`, but that is fine:
                // no parameter is highlighted in such a case
                sigInfo.setActiveParameter(commas)
                sigInfo
              }}
              result.setSignatures(sigInfos.asJava)
              var activeSig = 0
              if (active != null && active.getActiveSignature() != 0) {
                val newActiveSig = active.getActiveSignature() + sigInfos.length - active.getSignatures().size()
                activeSig = newActiveSig.max(0)
              }
              result.setActiveSignature(new Integer(activeSig))
              result
            }
          })
        }
      }
    }
  }
}

trait WorkspaceReceiver extends StandardReceiver with WorkspaceService {

  override def didChangeConfiguration(@unused params: DidChangeConfigurationParams): Unit = {
    coordinator.logger.info("On config change")
  }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    coordinator.logger.info("On changing watched files")
    // Even though we get notifications in the following order for the active file:
    //   1. didSave
    //   2. onVerify
    //   3. didChangeWatchedFiles
    // It is ok to call `resetFileInfo` since this has no effect when verifying a file.
    // We do want to call this on any files that change without being active in the IDE.
    params.getChanges.forEach(ev => coordinator.resetFileInfo(ev.getUri))
  }
}

class CustomReceiver(config: ViperConfig, server: ViperServerService, serverUrl: String)(implicit _executor: VerificationExecutionContext)
    extends LanguageClientAware with LanguageReceiver with WorkspaceReceiver with TextDocumentReceiver {

  override val coordinator = new ClientCoordinator(server)
  override def executor: VerificationExecutionContext = _executor

  override def getTextDocumentService(): TextDocumentService = this

  override def getWorkspaceService(): WorkspaceService = this

  val MIN_CLIENT_VERSION = "4.0.0"

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
