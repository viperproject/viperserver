// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2023 ETH Zurich.

package viper.server.frontends.lsp.file

import java.net.URI
import java.nio.file.{Path, Paths}
import org.eclipse.lsp4j
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.silver.ast
import scala.concurrent.Future

case class PathInfo(file_uri: String) {
  val uri: URI = URI.create(file_uri)
  val path: Path = Paths.get(uri)
  val filename: String = path.getFileName.toString
}

// TODO: change this to contain a `MessageHandler` rather than be a
case class FileManager(file: PathInfo, coordinator: lsp.ClientCoordinator, content: lsp.file.FileContent)(implicit executor: VerificationExecutionContext) extends MessageHandler {
  override val ec: VerificationExecutionContext = executor
  var isOpen: Boolean = true

  // Can force a refresh in the future if we get new ones, so return immediately
  def getCodeLens(uri: String): Future[Seq[lsp4j.CodeLens]] =
    Future.successful(getInProject(uri).getCodeLens())
  // Currently unused
  def getDiagnostics(uri: String): Future[Seq[lsp4j.Diagnostic]] =
    Future.successful(getInProject(uri).getDiagnostic())
  def getInlayHints(uri: String): Future[Seq[lsp4j.InlayHint]] =
    Future.successful(getInProject(uri).getInlayHint())
  def getSemanticHighlights(uri: String): Future[Seq[lsp.Lsp4jSemanticHighlight]] =
    Future.successful(getInProject(uri).getSemanticHighlight())

  // Even though we may be returning a stale request
  def getGotoDefinitions(uri: String, pos: lsp4j.Position): Future[Seq[lsp4j.LocationLink]] =
    getInFuture(getGotoDefinitionProject(uri, pos))
  def getHoverHints(uri: String, pos: lsp4j.Position): Future[Seq[lsp4j.Hover]] =
    Future.successful(getHoverHintProject(uri, pos))
  def getFindReferences(uri: String, pos: lsp4j.Position, includeDeclaration: Boolean, fromReferences: Boolean = true): Future[Seq[lsp4j.Location]] =
    getInFuture(getFindReferencesProject(uri, pos, includeDeclaration, fromReferences))
  def getRename(uri: String, pos: lsp4j.Position, newName: String): Future[lsp4j.WorkspaceEdit] =
    getInFuture({
      val references = getFindReferencesProject(uri, pos, true, true)
      getInProject(uri).getRename(references, newName).orNull
    })

  def getDocumentSymbols(uri: String): Future[Seq[lsp4j.DocumentSymbol]] =
    getInFuture(getInProject(uri).getDocumentSymbol())
  def getDocumentLinks(uri: String): Future[Seq[lsp4j.DocumentLink]] =
    getInFuture(getInProject(uri).getDocumentLink())
  def getFoldingRanges(uri: String): Future[Seq[lsp4j.FoldingRange]] =
    getInFuture(getInProject(uri).getFoldingRange())
  def getCompletionProposal(uri: String, pos: lsp4j.Position, char: Option[String]): Future[Seq[lsp4j.CompletionItem]] =
    getInFuture(getCompletionProposal(uri, pos, char, ()))
}

object FileManager {
  def apply(file_uri: String, coordinator: lsp.ClientCoordinator, c: Option[String])(implicit executor: VerificationExecutionContext): FileManager = {
    val file = PathInfo(file_uri)
    // In some cases the IDE doesn't send the content to use, then load it ourselves
    val fileContent = c.getOrElse(ast.utility.DiskLoader.loadContent(file.path).get)
    val content = lsp.file.FileContent(file.path, fileContent)
    new FileManager(file, coordinator, content)
  }
}
