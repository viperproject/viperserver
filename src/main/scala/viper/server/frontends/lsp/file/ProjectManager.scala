// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

// import scala.collection.mutable.HashSet
import viper.server.frontends.lsp.SetupProjectParams
import scala.collection.mutable.ArrayBuffer
import org.eclipse.lsp4j.Range

import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp
import viper.silver.ast.utility.lsp.SelectionBoundScopeTrait
import viper.server.frontends.lsp.ClientCoordinator
import scala.annotation.unused

case class LeafInfo(private val roots: ArrayBuffer[String]) {
  def lastRoot: String = roots.head
  def addRoot(root: String) = {
    roots -= root
    roots += root
  }
  def removeRoot(root: String): Boolean = {
    roots -= root
    roots.isEmpty
  }
  def rootsIter: Iterator[String] = roots.iterator
}
object LeafInfo {
  def apply(root: String): LeafInfo = new LeafInfo(ArrayBuffer(root))
}

trait ProjectAware {
  val coordinator: ClientCoordinator
  def getInProject(uri: String): FullManager
  def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation]
}

trait ProjectManager extends FullManager with ProjectAware {
  var project: Either[Map[String, LeafManager], LeafInfo] = Left(Map())
  def removeFromProject(root: String) = {
    this.removeDiagnostics()
    if (project.map(li => li.removeRoot(root)).getOrElse(false)) {
      project = Left(Map())
    }
  }
  def addToProject(root: String, getContents: Boolean): (Option[String], Option[Set[String]]) = {
    this.removeDiagnostics()
    val oldProject = project match {
      case Left(p) => {
        project = Right(LeafInfo(root))
        Some(p.keySet)
      }
      case Right(li) => {
        li.addRoot(root)
        None
      }
    }
    val contents = if (getContents) Some(content.fileContent.mkString("\n")) else None
    (contents, oldProject)
  }

  def projectRoot: Option[String] = project.toOption.map(_.lastRoot)
  def projectLeaves: Option[Set[String]] = project.left.toOption.map(_.keySet)
  def isRoot: Boolean = project.isLeft

  def getInProject(uri: String): FullManager = {
    if (uri == file.file_uri) this else project.left.toOption.get.get(uri).get
  }
  def getLeaf(uri: String): LeafManager = project.left.toOption.get(uri)

  private def addBtf[V <: lsp.HasRangePositions with lsp.BelongsToFile](toC: String => Seq[V] => Unit, vs: Seq[V]): Unit = {
    val grouped = vs.groupBy(_.file.toUri().toString())
    grouped foreach (g => toC(g._1)(g._2))
  }

  private def addSib[V <: lsp.HasRangePositions with lsp.SelectableInBound](toC: String => Seq[V] => Unit, vs: Seq[V]): Unit = {
    val grouped = vs.groupBy(_.bound match {
      case bound: SelectionBoundScopeTrait => bound.scope.file.toUri().toString()
      case _ => file.file_uri
    })
    grouped foreach (g => toC(g._1)(g._2))
  }

  override def addCodeLens(first: Boolean)(vs: Seq[lsp.CodeLens]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addCodeLens(first) else getInProject(uri).addCodeLens(first), vs)

  override def addDiagnostic(first: Boolean)(vs: Seq[Diagnostic]): Unit = {
    addBtf(uri => if (uri == file.file_uri) super.addDiagnostic(first) else getInProject(uri).addDiagnostic(first), vs)
  }

  override def addDocumentSymbol(first: Boolean)(vs: Seq[lsp.DocumentSymbol]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addDocumentSymbol(first) else getInProject(uri).addDocumentSymbol(first), vs)

  override def addFoldingRange(first: Boolean)(vs: Seq[lsp.FoldingRange]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addFoldingRange(first) else getInProject(uri).addFoldingRange(first), vs)

  override def addGotoDefinition(first: Boolean)(vs: Seq[lsp.GotoDefinition]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addGotoDefinition(first) else getInProject(uri).addGotoDefinition(first), vs)
  def getGotoDefinitionProject(uri: String, pos: lsp4j.Position): Seq[lsp4j.LocationLink] = {
    val keyword = getIdentAtPos(uri, pos)
    if (uri == file.file_uri) super.getGotoDefinition(Some(pos), keyword)
    else super.getGotoDefinition(None, keyword) ++ getInProject(uri).getGotoDefinition(Some(pos), keyword)
  }

  override def addHoverHint(first: Boolean)(vs: Seq[lsp.HoverHint]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addHoverHint(first) else getInProject(uri).addHoverHint(first), vs)
  def getHoverHintProject(uri: String, pos: lsp4j.Position): Seq[lsp4j.Hover] = {
    val keyword = getIdentAtPos(uri, pos)
    if (uri == file.file_uri) super.getHoverHint(Some(pos), keyword)
    else super.getHoverHint(None, keyword) ++ getInProject(uri).getHoverHint(Some(pos), keyword)
  }

  override def addInlayHint(first: Boolean)(vs: Seq[lsp.InlayHint]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addInlayHint(first) else getInProject(uri).addInlayHint(first), vs)

  override def addSemanticHighlight(first: Boolean)(vs: Seq[lsp.SemanticHighlight]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addSemanticHighlight(first) else getInProject(uri).addSemanticHighlight(first), vs)

  override def addSignatureHelp(first: Boolean)(vs: Seq[lsp.SignatureHelp]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addSignatureHelp(first) else getInProject(uri).addSignatureHelp(first), vs)
  def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation] =
    if (uri == file.file_uri) super.getSignatureHelp(Some(pos), Some((keyword, range)))
    else super.getSignatureHelp(None, Some((keyword, range))) ++ getInProject(uri).getSignatureHelp(Some(pos), Some((keyword, range)))


  override def addFindReferences(first: Boolean)(vs: Seq[lsp.ReferenceTo]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addFindReferences(first) else getInProject(uri).addFindReferences(first), vs)
  def getFindReferencesProject(uri: String, pos: lsp4j.Position, includeDeclaration: Boolean, fromReferences: Boolean): Seq[lsp4j.Location] = {
    val refs = getInProject(uri).getFindReferences(pos, includeDeclaration)
    if (!refs.isEmpty || !fromReferences) refs else {
      val defns = getGotoDefinitionProject(uri, pos)
      if (defns.length != 1) Nil
      else getInProject(defns(0).getTargetUri).getFindReferences(defns(0).getTargetSelectionRange.getStart, includeDeclaration)
    }
  }

  override def addSuggestionScopeRange(first: Boolean)(vs: Seq[lsp.SuggestionScopeRange]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addSuggestionScopeRange(first) else getInProject(uri).addSuggestionScopeRange(first), vs)

  override def addCompletionProposal(first: Boolean)(vs: Seq[lsp.CompletionProposal]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addCompletionProposal(first) else getInProject(uri).addCompletionProposal(first), vs)
  def getCompletionProposal(uri: String, pos: lsp4j.Position, @unused char: Option[String], @unused u: Unit) = {
    val m = getInProject(uri)
    val scope: lsp.SuggestionScope = m.getSuggestionScopeRange(pos).head
    val c = m.content
    val ident = c.getIdentAtPos(pos)
    val start = ident.map(_._2.getStart).getOrElse(pos)
    // Get character
    val char = c.iterBackward(start).drop(1).find{ case (c, _) => c != ' ' }.map(_._1).getOrElse('\n')
    if (uri == file.file_uri) super.getCompletionProposal(scope, Some(pos), char)
    else super.getCompletionProposal(scope, None, char) ++ getInProject(uri).getCompletionProposal(scope, Some(pos), char)
  }

  def getIdentAtPos(uri: String, pos: lsp4j.Position): Option[(String, Range)] =
    getInProject(uri).content.getIdentAtPos(pos)

  def setupProject(newProject: Set[String]) = {
    val oldProject = project.left.toOption.getOrElse(Map())
    val toRemove = oldProject.keySet.diff(newProject)
    for (p <- toRemove) {
      coordinator.removeFromProject(p, file.file_uri)
    }
    val np = newProject.map(uri => uri -> coordinator.addToProject(uri, file.file_uri, !oldProject.contains(uri))).toMap
    for ((root, (_, leavesOpt)) <- np; leaves <- leavesOpt; leaf <- leaves) {
      coordinator.removeFromProject(leaf, root)
    }

    def getLeafManager(uri: String): LeafManager = oldProject.getOrElse(uri, LeafManager(uri, np(uri)._1.get, coordinator))
    this.project = Left(newProject.map(uri => uri -> getLeafManager(uri)).toMap)

    val setupProject = SetupProjectParams(file.file_uri, newProject.toArray)
    coordinator.client.requestSetupProject(setupProject)
  }

  def handleChangeInLeaf(leaf: String, range: Range, text: String): Unit = {
    project match {
      case Left(project) => project.get(leaf) match {
        case None => coordinator.logger.error(s"handleChangeInLeaf called on project without leaf (${leaf})")
        case Some(v) => v.handleContentChange(range, text)
      }
      case _ => coordinator.logger.error("handleChangeInLeaf called on non-root")
    }
  }

  override def handleContentChange(range: Range, text: String): Unit = {
    super.handleContentChange(range, text)
    project.foreach(_.rootsIter.foreach(root => {
      coordinator.handleChangeInLeaf(root, file.file_uri, range, text)
    }))
  }
}
