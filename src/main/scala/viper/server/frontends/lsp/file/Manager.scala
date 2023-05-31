package viper.server.frontends.lsp.file

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentSymbol, FoldingRange, Position, PublishDiagnosticsParams, Range, SymbolKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
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
import viper.server.frontends.lsp.file.{DocumentSymbolManager}
import viper.server.frontends.lsp.file.FileContent
import viper.server.frontends.lsp.file.ProgressCoordinator
import viper.silver.ast.utility.lsp.SemanticHighlight

trait Manager[A <: Manager[A]] extends CodeLensManager[A] with DiagnosticManager[A] with DocumentSymbolManager[A] with FoldingRangeManager[A] with GotoDefinitionManager[A] with HoverHintManager[A] with InlayHintManager[A] with SemanticHighlightManager[A] with SignatureHelpManager[A] { this: A =>
}

case class LeafManager(file: PathInfo, coordinator: lsp.ClientCoordinator, content: lsp.file.FileContent)(implicit executor: VerificationExecutionContext) extends Manager[LeafManager] {
  override implicit def ec: VerificationExecutionContext = executor
  // override def thisInFile(uri: String): LeafManager = throw new Exception("Internal error in `LeafManager`")
}
object LeafManager {
  def apply(uri: String, content: String, coordinator: lsp.ClientCoordinator)(implicit executor: VerificationExecutionContext): LeafManager = {
    val file = PathInfo(uri)
    new LeafManager(file, coordinator, FileContent(file.path, content))
  }
}
