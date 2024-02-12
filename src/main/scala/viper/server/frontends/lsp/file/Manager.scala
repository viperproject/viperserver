// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import java.nio.file.Path
import org.eclipse.lsp4j.{PublishDiagnosticsParams, Range}
import viper.server.frontends.lsp.ClientCoordinator

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import viper.server.frontends.lsp.file.FileContent
import viper.server.frontends.lsp.Lsp4jSemanticHighlight
import VerificationPhase._

import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp
import viper.server.frontends.lsp.Common

case class Diagnostic(backendClassName: Option[String], position: lsp.RangePosition, message: String, severity: lsp4j.DiagnosticSeverity, cached: Boolean, errorMsgPrefix: Option[String]) extends lsp.HasRangePositions with lsp.BelongsToFile {
  override def rangePositions: Seq[lsp.RangePosition] = Seq(position)
  override def file: Path = position.file
}

trait Manager {
  val file: PathInfo
  val content: FileContent
  val coordinator: ClientCoordinator
  implicit def logger = coordinator.logger
  def addContainer(c: utility.LspContainer[_, _, _, _, _]): Unit
  def resetContainers(first: Boolean): Unit

  var lastPhase: Option[VerificationPhase] = None

  def getCodeLens(): Seq[lsp4j.CodeLens]
  def addCodeLens(first: Boolean)(vs: Seq[lsp.CodeLens]): Unit

  def getDiagnostic(): Seq[lsp4j.Diagnostic]
  def addDiagnostic(first: Boolean)(vs: Seq[Diagnostic]): Unit

  def getDocumentSymbol(): Seq[lsp4j.DocumentSymbol]
  def addDocumentSymbol(first: Boolean)(vs: Seq[lsp.DocumentSymbol]): Unit

  def getDocumentLink(): Seq[lsp4j.DocumentLink]

  def getFoldingRange(): Seq[lsp4j.FoldingRange]
  def addFoldingRange(first: Boolean)(vs: Seq[lsp.FoldingRange]): Unit

  def getGotoDefinition(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]): Seq[lsp4j.LocationLink]
  def addGotoDefinition(first: Boolean)(vs: Seq[lsp.GotoDefinition]): Unit

  def getHoverHint(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]): Seq[lsp4j.Hover]
  def addHoverHint(first: Boolean)(vs: Seq[lsp.HoverHint]): Unit

  def getInlayHint(): Seq[lsp4j.InlayHint]
  def addInlayHint(first: Boolean)(vs: Seq[lsp.InlayHint]): Unit

  def getSemanticHighlight(): Seq[Lsp4jSemanticHighlight]
  def addSemanticHighlight(first: Boolean)(vs: Seq[lsp.SemanticHighlight]): Unit

  def getSignatureHelp(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]): Seq[lsp4j.SignatureInformation]
  def addSignatureHelp(first: Boolean)(vs: Seq[lsp.SignatureHelp]): Unit

  def getSuggestionScopeRange(pos: lsp4j.Position): Seq[lsp.SuggestionScope]
  def addSuggestionScopeRange(first: Boolean)(vs: Seq[lsp.SuggestionScopeRange]): Unit

  def getCompletionProposal(scope: lsp.SuggestionScope, pos: Option[lsp4j.Position], char: Char): Seq[lsp4j.CompletionItem]
  def addCompletionProposal(first: Boolean)(vs: Seq[lsp.CompletionProposal]): Unit
}

trait StandardManager extends Manager {
  private val containers: ArrayBuffer[utility.LspContainer[_, _, _, _, _]] = ArrayBuffer()

  def addContainer(c: utility.LspContainer[_, _, _, _, _]): Unit = containers.addOne(c)
  def resetContainers(first: Boolean): Unit = {
    containers.foreach(_.reset(first))
  }
  def handleContentChange(range: Range, text: String): Unit = {
    content.handleChange(range, text)
    containers.foreach(_.resetModifiedFlag())
    containers.foreach(_.updatePositions(range, text))
  }

  private def add[V <: lsp.HasRangePositions](c: utility.LspContainer[_, _, V, _, _], first: Boolean, vs: Seq[V]): Unit = {
    vs foreach (v => c.receive(first, v))
    c.onUpdate()
  }

  // CodeLens
  type CodeLensContainer = utility.StageArrayContainer.ArrayContainer[lsp.CodeLens, lsp4j.CodeLens]
  val codeLensContainer: CodeLensContainer = utility.LspContainer(utility.CodeLensTranslator, coordinator.client.refreshCodeLenses)
  containers.addOne(codeLensContainer)
  def getCodeLens() = codeLensContainer.get(())
  def addCodeLens(first: Boolean)(vs: Seq[lsp.CodeLens]): Unit = add(codeLensContainer, first, vs)

  // Diagnostic
  type DiagnosticContainer = utility.StageArrayContainer.ArrayContainer[Diagnostic, lsp4j.Diagnostic]
  val diagnosticContainer: DiagnosticContainer = utility.LspContainer(utility.DiagnosticTranslator, publishDiags)
  private def publishDiags(): Unit = {
    val diagnosticParams = new PublishDiagnosticsParams(file.file_uri, getDiagnostic().asJava)
    coordinator.client.publishDiagnostics(diagnosticParams)
  }
  containers.addOne(diagnosticContainer)
  def getDiagnostic() = diagnosticContainer.get(())
  def addDiagnostic(first: Boolean)(vs: Seq[Diagnostic]): Unit = add(diagnosticContainer, first, vs)
  def removeDiagnostics() = diagnosticContainer.resetAll()

  // DocumentSymbol
  type DocumentSymbolContainer = utility.StageArrayContainer.ArrayContainer[lsp.DocumentSymbol, lsp4j.DocumentSymbol]
  val documentSymbolContainer: DocumentSymbolContainer = utility.LspContainer(utility.DocumentSymbolTranslator)
  containers.addOne(documentSymbolContainer)
  def getDocumentSymbol() = documentSymbolContainer.get(())
  def addDocumentSymbol(first: Boolean)(vs: Seq[lsp.DocumentSymbol]): Unit = add(documentSymbolContainer, first, vs)

  // DocumentLink
  def getDocumentLink() = documentSymbolContainer.all(_.imports.isDefined).map(ds =>
      new lsp4j.DocumentLink(Common.toRange(ds.selectionRange), ds.imports.get.toUri().toString())
    )

  // FoldingRange
  type FoldingRangeContainer = utility.StageArrayContainer.ArrayContainer[lsp.FoldingRange, lsp4j.FoldingRange]
  val foldingRangeContainer: FoldingRangeContainer = utility.LspContainer(utility.FoldingRangeTranslator)
  containers.addOne(foldingRangeContainer)
  def getFoldingRange() = foldingRangeContainer.get(())
  def addFoldingRange(first: Boolean)(vs: Seq[lsp.FoldingRange]): Unit = add(foldingRangeContainer, first, vs)

  // GotoDefinition
  type GotoDefinitionContainer = utility.StageRangeContainer.RangeContainer[lsp.GotoDefinition, lsp4j.LocationLink]
  val gotoDefinitionContainer: GotoDefinitionContainer = utility.LspContainer(utility.GotoDefinitionTranslator)
  containers.addOne(gotoDefinitionContainer)
  def getGotoDefinition(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]) = gotoDefinitionContainer.get((pos, keyword, false))
  def addGotoDefinition(first: Boolean)(vs: Seq[lsp.GotoDefinition]): Unit = add(gotoDefinitionContainer, first, vs)

  // HoverHint
  type HoverHintContainer = utility.StageRangeContainer.RangeContainer[lsp.HoverHint, lsp4j.Hover]
  val hoverHintContainer: HoverHintContainer = utility.LspContainer(utility.HoverHintTranslator)
  containers.addOne(hoverHintContainer)
  def getHoverHint(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]) = hoverHintContainer.get((pos, keyword, true))
  def addHoverHint(first: Boolean)(vs: Seq[lsp.HoverHint]): Unit = add(hoverHintContainer, first, vs)

  // InlayHint
  type InlayHintContainer = utility.StageArrayContainer.ArrayContainer[lsp.InlayHint, lsp4j.InlayHint]
  val inlayHintContainer: InlayHintContainer = utility.LspContainer(utility.InlayHintTranslator, coordinator.client.refreshInlayHints)
  containers.addOne(inlayHintContainer)
  def getInlayHint() = inlayHintContainer.get(())
  def addInlayHint(first: Boolean)(vs: Seq[lsp.InlayHint]): Unit = add(inlayHintContainer, first, vs)

  // SemanticHighlight
  type SemanticHighlightContainer = utility.StageArrayContainer.ArrayContainer[lsp.SemanticHighlight, Lsp4jSemanticHighlight]
  val semanticHighlightContainer: SemanticHighlightContainer = utility.LspContainer(utility.SemanticHighlightTranslator, coordinator.client.refreshSemanticTokens)
  containers.addOne(semanticHighlightContainer)
  def getSemanticHighlight() = semanticHighlightContainer.get(())
  def addSemanticHighlight(first: Boolean)(vs: Seq[lsp.SemanticHighlight]): Unit = add(semanticHighlightContainer, first, vs)

  // SignatureHelp
  type SignatureHelpContainer = utility.StageRangeContainer.RangeContainer[lsp.SignatureHelp, lsp4j.SignatureInformation]
  val signatureHelpContainer: SignatureHelpContainer = utility.LspContainer(utility.SignatureHelpTranslator)
  containers.addOne(signatureHelpContainer)
  def getSignatureHelp(pos: Option[lsp4j.Position], keyword: Option[(String, lsp4j.Range)]) = signatureHelpContainer.get((pos, keyword, false))
  def addSignatureHelp(first: Boolean)(vs: Seq[lsp.SignatureHelp]): Unit = add(signatureHelpContainer, first, vs)

  // SuggestionScopeRange
  type SuggestionScopeRangeContainer = utility.StageRangeContainer.RangeContainer[lsp.SuggestionScopeRange, lsp.SuggestionScope]
  val suggestionScopeRangeContainer: SuggestionScopeRangeContainer = utility.LspContainer(utility.SuggestionScopeRangeTranslator, expandOnBorder = true)
  containers.addOne(suggestionScopeRangeContainer)
  def getSuggestionScopeRange(pos: lsp4j.Position) = suggestionScopeRangeContainer.get((Some(pos), None, false))
  def addSuggestionScopeRange(first: Boolean)(vs: Seq[lsp.SuggestionScopeRange]): Unit = add(suggestionScopeRangeContainer, first, vs)


  // CompletionProposal
  type CompletionProposalContainer = utility.StageSuggestableContainer.SuggestableContainer[lsp.CompletionProposal, lsp4j.CompletionItem]
  val completionProposalContainer: CompletionProposalContainer = utility.LspContainer(utility.CompletionProposalTranslator, expandOnBorder = true)
  containers.addOne(completionProposalContainer)
  def getCompletionProposal(scope: lsp.SuggestionScope, pos: Option[lsp4j.Position], char: Char) = completionProposalContainer.get((scope, pos, char))
  def addCompletionProposal(first: Boolean)(vs: Seq[lsp.CompletionProposal]): Unit = add(completionProposalContainer, first, vs)
}

trait FullManager extends StandardManager with FindReferencesManager

case class LeafManager(file: PathInfo, coordinator: ClientCoordinator, content: FileContent) extends FullManager {
}
object LeafManager {
  def apply(uri: String, content: String, coordinator: ClientCoordinator): LeafManager = {
    val file = PathInfo(uri)
    new LeafManager(file, coordinator, FileContent(file.path, content))
  }
}
