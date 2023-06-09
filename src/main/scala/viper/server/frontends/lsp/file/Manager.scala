// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentSymbol, FoldingRange, Position, PublishDiagnosticsParams, Range, SymbolKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.ClientCoordinator
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError, ErrorMessage}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.concurrent.Future
import org.eclipse.lsp4j.ParameterInformation
import org.eclipse.lsp4j.jsonrpc.messages.Tuple.Two
import viper.server.vsi.AstJobId
import viper.server.frontends.lsp.file.FileContent
import viper.server.frontends.lsp.file.ProgressCoordinator
import viper.server.frontends.lsp.Lsp4jSemanticHighlight
import VerificationPhase._

import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp
import viper.server.frontends.lsp.file.utility.LspContainer
import ch.qos.logback.classic.Logger
import viper.server.frontends.lsp.file.utility.StageContainer
import viper.silver.ast.utility.lsp.GotoDefinition
import viper.server.frontends.lsp.Common

case class Diagnostic(backendClassName: Option[String], position: lsp.RangePosition, message: String, cached: Boolean, errorMsgPrefix: Option[String]) extends lsp.HasRangePositions with lsp.BelongsToFile {
  override def rangePositions: Seq[lsp.RangePosition] = Seq(position)
  override def file: Path = position.file
}

// trait ContainerGet[K, U] {
//   def get(k: K): Seq[U]
// }
// trait ContainerAdd[V <: lsp.HasRangePositions] {
//   def add(phase: VerificationPhase, vs: Seq[V]): Unit
// }
// case class ContainerWrapper[K, V <: lsp.HasRangePositions, U](c: LspContainer[_ <: StageContainer[K, V], K, V, U])(implicit val logger: Logger) extends ContainerGet[K, U] with ContainerAdd[V] {
//   override def get(k: K): Seq[U] = c.get(k)
//   override def add(phase: VerificationPhase, vs: Seq[V]) = {
//     vs foreach (v => c.receive(phase, v))
//     c.onUpdate()
//   }
//   // def addNoUpdate(phase: VerificationPhase, v: V) = c.receive(phase, v)
// }

// object ContainerType {
//   sealed trait ContainerType[K, V <: lsp.HasRangePositions, U]
//   case object CodeLens extends ContainerType[Unit, lsp.CodeLens, lsp4j.CodeLens]
//   case object Diagnostic extends ContainerType[Unit, Diagnostic, lsp4j.Diagnostic]
//   case object DocumentSymbol extends ContainerType[Unit, lsp.DocumentSymbol, lsp4j.DocumentSymbol]
//   case object FoldingRange extends ContainerType[Unit, lsp.FoldingRange, lsp4j.FoldingRange]
//   case object GotoDefinition extends ContainerType[(String, lsp4j.Position, lsp4j.Range), lsp.GotoDefinition, lsp4j.LocationLink]
//   case object HoverHint extends ContainerType[(String, lsp4j.Position, lsp4j.Range), lsp.HoverHint, lsp4j.Hover]
//   case object SemanticHighlight extends ContainerType[Unit, lsp.SemanticHighlight, Lsp4jSemanticHighlight]
// }

trait Manager {
  val coordinator: ClientCoordinator
  implicit def logger = coordinator.logger
  protected val containers: ArrayBuffer[utility.LspContainer[_, _, _, _, _]]
}

trait StandardManager extends Manager {
  val file: PathInfo
  val content: FileContent
  protected val containers: ArrayBuffer[utility.LspContainer[_, _, _, _, _]] = ArrayBuffer()

  def reset(phase: VerificationPhase.VerificationPhase): Unit = {
    containers.foreach(_.reset(phase))
  }
  def handleContentChange(range: Range, text: String): Unit = {
    content.handleChange(range, text)
    containers.foreach(_.resetModifiedFlag())
    containers.foreach(_.updatePositions(range, text))
  }

  private def add[V <: lsp.HasRangePositions](c: utility.LspContainer[_, _, V, _, _], phase: VerificationPhase, vs: Seq[V]): Unit = {
    vs foreach (v => c.receive(phase, v))
    c.onUpdate()
  }

  // CodeLens
  type CodeLensContainer = utility.StageArrayContainer.ArrayContainer[lsp.CodeLens, lsp4j.CodeLens]
  val codeLensContainer: CodeLensContainer = utility.LspContainer(utility.CodeLensTranslator, coordinator.client.refreshCodeLenses)
  containers.addOne(codeLensContainer)
  def getCodeLens() = codeLensContainer.get(())
  def addCodeLens(phase: VerificationPhase)(vs: Seq[lsp.CodeLens]): Unit = add(codeLensContainer, phase, vs)

  // Diagnostic
  type DiagnosticContainer = utility.StageArrayContainer.ArrayContainer[Diagnostic, lsp4j.Diagnostic]
  val diagnosticContainer: DiagnosticContainer = utility.LspContainer(utility.DiagnosticTranslator, publishDiags)
  private def publishDiags(): Unit = {
    val diagnosticParams = new PublishDiagnosticsParams(file.file_uri, getDiagnostic().asJava)
    coordinator.client.publishDiagnostics(diagnosticParams)
  }
  containers.addOne(diagnosticContainer)
  def getDiagnostic() = diagnosticContainer.get(())
  def addDiagnostic(phase: VerificationPhase)(vs: Seq[Diagnostic]): Unit = add(diagnosticContainer, phase, vs)
  def removeDiagnostics() = diagnosticContainer.resetAll()

  // DocumentSymbol
  type DocumentSymbolContainer = utility.StageArrayContainer.ArrayContainer[lsp.DocumentSymbol, lsp4j.DocumentSymbol]
  val documentSymbolContainer: DocumentSymbolContainer = utility.LspContainer(utility.DocumentSymbolTranslator)
  containers.addOne(documentSymbolContainer)
  def getDocumentSymbol() = documentSymbolContainer.get(())
  def addDocumentSymbol(phase: VerificationPhase)(vs: Seq[lsp.DocumentSymbol]): Unit = add(documentSymbolContainer, phase, vs)

  // DocumentLink
  def getDocumentLink() = documentSymbolContainer.all(_.imports.isDefined).map(ds =>
      new lsp4j.DocumentLink(Common.toRange(ds.selectionRange), ds.imports.get.toUri().toString())
    )

  // FoldingRange
  type FoldingRangeContainer = utility.StageArrayContainer.ArrayContainer[lsp.FoldingRange, lsp4j.FoldingRange]
  val foldingRangeContainer: FoldingRangeContainer = utility.LspContainer(utility.FoldingRangeTranslator)
  containers.addOne(foldingRangeContainer)
  def getFoldingRange() = foldingRangeContainer.get(())
  def addFoldingRange(phase: VerificationPhase)(vs: Seq[lsp.FoldingRange]): Unit = add(foldingRangeContainer, phase, vs)

  // GotoDefinition
  type GotoDefinitionContainer = utility.StageRangeContainer.RangeContainer[lsp.GotoDefinition, lsp4j.LocationLink]
  val gotoDefinitionContainer: GotoDefinitionContainer = utility.LspContainer(utility.GotoDefinitionTranslator)
  containers.addOne(gotoDefinitionContainer)
  def getGotoDefinition(keyword: String, pos: Option[lsp4j.Position], range: lsp4j.Range) = gotoDefinitionContainer.get((keyword, pos, range))
  def addGotoDefinition(phase: VerificationPhase)(vs: Seq[lsp.GotoDefinition]): Unit = add(gotoDefinitionContainer, phase, vs)

  // HoverHint
  type HoverHintContainer = utility.StageRangeContainer.RangeContainer[lsp.HoverHint, lsp4j.Hover]
  val hoverHintContainer: HoverHintContainer = utility.LspContainer(utility.HoverHintTranslator)
  containers.addOne(hoverHintContainer)
  def getHoverHint(keyword: String, pos: Option[lsp4j.Position], range: lsp4j.Range) = hoverHintContainer.get((keyword, pos, range))
  def addHoverHint(phase: VerificationPhase)(vs: Seq[lsp.HoverHint]): Unit = add(hoverHintContainer, phase, vs)

  // InlayHint
  type InlayHintContainer = utility.StageArrayContainer.ArrayContainer[lsp.InlayHint, lsp4j.InlayHint]
  val inlayHintContainer: InlayHintContainer = utility.LspContainer(utility.InlayHintTranslator, coordinator.client.refreshInlayHints)
  containers.addOne(inlayHintContainer)
  def getInlayHint() = inlayHintContainer.get(())
  def addInlayHint(phase: VerificationPhase)(vs: Seq[lsp.InlayHint]): Unit = add(inlayHintContainer, phase, vs)

  // SemanticHighlight
  type SemanticHighlightContainer = utility.StageArrayContainer.ArrayContainer[lsp.SemanticHighlight, Lsp4jSemanticHighlight]
  val semanticHighlightContainer: SemanticHighlightContainer = utility.LspContainer(utility.SemanticHighlightTranslator, coordinator.client.refreshSemanticTokens)
  containers.addOne(semanticHighlightContainer)
  def getSemanticHighlight() = semanticHighlightContainer.get(())
  def addSemanticHighlight(phase: VerificationPhase)(vs: Seq[lsp.SemanticHighlight]): Unit = add(semanticHighlightContainer, phase, vs)

  // SignatureHelp
  type SignatureHelpContainer = utility.StageRangeContainer.RangeContainer[lsp.SignatureHelp, lsp4j.SignatureInformation]
  val signatureHelpContainer: SignatureHelpContainer = utility.LspContainer(utility.SignatureHelpTranslator)
  containers.addOne(signatureHelpContainer)
  def getSignatureHelp(keyword: String, pos: Option[lsp4j.Position], range: lsp4j.Range) = signatureHelpContainer.get((keyword, pos, range))
  def addSignatureHelp(phase: VerificationPhase)(vs: Seq[lsp.SignatureHelp]): Unit = add(signatureHelpContainer, phase, vs)
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
