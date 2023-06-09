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
import VerificationPhase._
import viper.server.frontends.lsp.file.utility.{LspContainer, StageArrayContainer}
import viper.silver.ast.utility.lsp.SelectionBound
import viper.silver.ast.utility.lsp.SelectionBoundKeywordTrait
import viper.silver.ast.utility.lsp.SelectionBoundScopeTrait
import viper.server.frontends.lsp.ClientCoordinator
import viper.silver.ast.utility.lsp.GotoDefinition
import viper.silver.parser.PIdnUse
import viper.silver.ast.FilePosition
import viper.silver.parser.PIdnDef

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

// case class BelongsToFileWrapper[V <: lsp.HasRangePositions with lsp.BelongsToFile, U](c: String => ContainerInterface[Unit, V, U]) extends ContainerInterface[String, V, U] {
//   override def get(uri: String): Seq[U] = c(uri).get(())
//   override def add(phase: VerificationPhase, vs: Seq[V]) = {
//     val grouped = vs.groupBy(_.file.toUri().toString())
//     grouped foreach (g => c(g._1).add(phase, g._2))
//   }
//   // def addNoUpdate(phase: VerificationPhase, v: V) = c(v.file.toUri().toString()).addNoUpdate(phase, v)
// }

// case class SelectableInBoundWrapper[K, V <: lsp.HasRangePositions with lsp.SelectableInBound, U](g: ContainerWrapper[K, V, U], c: String => ContainerInterface[K, V, U]) extends ContainerInterface[(String, K), V, U] {
//   override def get(k: (String, K)): Seq[U] = c(k._1).get(k._2)
//   override def add(phase: VerificationPhase, vs: Seq[V]) = {
//     val grouped = vs.groupBy(fileBound)
//     grouped foreach {
//       case (None, vs) => g.add(phase, vs)
//       case (Some(uri), vs) => c(uri).add(phase, vs)
//     }
//   }
//   // def addNoUpdate(phase: VerificationPhase, v: V) = c(v.file.toUri().toString()).addNoUpdate(phase, v)
//   private def fileBound(v: V): Option[String] = v.bound match {
//       case bound: SelectionBoundScopeTrait => Some(bound.scope.file.toUri().toString())
//       case _ => None
//     }
// }

trait ProjectAware {
  val coordinator: ClientCoordinator
  def getInProject(uri: String): FullManager
  def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation]
}

trait ProjectManager extends FullManager with ProjectAware {
  // val manager: FileManager

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

  def getInProject(uri: String): FullManager = if (uri == file.file_uri) this else project.left.toOption.get(uri)
  def getLeaf(uri: String): LeafManager = project.left.toOption.get(uri)
  // override def add[T <: lsp.BelongsToFile](phase: VerificationPhase, v: T)

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

  override def addCodeLens(phase: VerificationPhase)(vs: Seq[lsp.CodeLens]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addCodeLens(phase) else getInProject(uri).addCodeLens(phase), vs)

  override def addDiagnostic(phase: VerificationPhase)(vs: Seq[Diagnostic]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addDiagnostic(phase) else getInProject(uri).addDiagnostic(phase), vs)

  override def addDocumentSymbol(phase: VerificationPhase)(vs: Seq[lsp.DocumentSymbol]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addDocumentSymbol(phase) else getInProject(uri).addDocumentSymbol(phase), vs)

  override def addFoldingRange(phase: VerificationPhase)(vs: Seq[lsp.FoldingRange]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addFoldingRange(phase) else getInProject(uri).addFoldingRange(phase), vs)

  override def addGotoDefinition(phase: VerificationPhase)(vs: Seq[lsp.GotoDefinition]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addGotoDefinition(phase) else getInProject(uri).addGotoDefinition(phase), vs)
  def getGotoDefinitionProject(uri: String, pos: lsp4j.Position): Option[Seq[lsp4j.LocationLink]] =
    getIdentAtPos(uri, pos).map {
      case (keyword, range) =>
        if (uri == file.file_uri) super.getGotoDefinition(keyword, Some(pos), range)
        else super.getGotoDefinition(keyword, None, range) ++ getInProject(uri).getGotoDefinition(keyword, Some(pos), range)
    }

  override def addHoverHint(phase: VerificationPhase)(vs: Seq[lsp.HoverHint]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addHoverHint(phase) else getInProject(uri).addHoverHint(phase), vs)
  def getHoverHintProject(uri: String, pos: lsp4j.Position): Option[Seq[lsp4j.Hover]] =
    getIdentAtPos(uri, pos).map {
      case (keyword, range) =>
        if (uri == file.file_uri) super.getHoverHint(keyword, Some(pos), range)
        else super.getHoverHint(keyword, None, range) ++ getInProject(uri).getHoverHint(keyword, Some(pos), range)
    }

  override def addInlayHint(phase: VerificationPhase)(vs: Seq[lsp.InlayHint]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addInlayHint(phase) else getInProject(uri).addInlayHint(phase), vs)

  override def addSemanticHighlight(phase: VerificationPhase)(vs: Seq[lsp.SemanticHighlight]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addSemanticHighlight(phase) else getInProject(uri).addSemanticHighlight(phase), vs)

  override def addSignatureHelp(phase: VerificationPhase)(vs: Seq[lsp.SignatureHelp]): Unit =
    addSib(uri => if (uri == file.file_uri) super.addSignatureHelp(phase) else getInProject(uri).addSignatureHelp(phase), vs)
  def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation] =
    if (uri == file.file_uri) super.getSignatureHelp(keyword, Some(pos), range)
    else super.getSignatureHelp(keyword, None, range) ++ getInProject(uri).getSignatureHelp(keyword, Some(pos), range)


  override def addFindReferences(phase: VerificationPhase)(vs: Seq[lsp.ReferenceTo]): Unit =
    addBtf(uri => if (uri == file.file_uri) super.addFindReferences(phase) else getInProject(uri).addFindReferences(phase), vs)
  def getFindReferencesProject(uri: String, pos: lsp4j.Position, includeDeclaration: Boolean, fromReferences: Boolean): Seq[lsp4j.Location] = {
    val refs = getInProject(uri).getFindReferences(pos, includeDeclaration)
    if (!refs.isEmpty || !fromReferences) refs else {
      getGotoDefinitionProject(uri, pos).toSeq.flatMap(defns => {
        if (defns.length != 1) Nil
        else getInProject(defns(0).getTargetUri).getFindReferences(defns(0).getTargetSelectionRange.getStart, includeDeclaration)
      })
    }
  }

  def getIdentAtPos(uri: String, pos: lsp4j.Position): Option[(String, Range)] =
    getInProject(uri).content.getIdentAtPos(pos)

  // val codeLensProject = BelongsToFileWrapper(getInProject(_).codeLens)
  // val diagnosticProject = BelongsToFileWrapper(getInProject(_).diagnostic)
  // val documentSymbolProject = BelongsToFileWrapper(getInProject(_).documentSymbol)
  // val foldingRangeProject = BelongsToFileWrapper(getInProject(_).foldingRange)

  // // GotoDefinitionGlobal
  // private val gotoDefinitionGlobalContainer: GotoDefinitionContainer = utility.LspContainer(utility.GotoDefinitionTranslator)
  // containers.addOne(gotoDefinitionGlobalContainer)
  // val gotoDefinitionProject = SelectableInBoundWrapper(ContainerWrapper(gotoDefinitionGlobalContainer), getInProject(_).gotoDefinition)

  // // HoverHintGlobal
  // private val hoverHintGlobalContainer: HoverHintContainer = utility.LspContainer(utility.HoverHintTranslator)
  // containers.addOne(hoverHintGlobalContainer)
  // val hoverHintProject = SelectableInBoundWrapper(ContainerWrapper(hoverHintGlobalContainer), getInProject(_).hoverHint)

  // val inlayHintProject = BelongsToFileWrapper(getInProject(_).inlayHint)
  // val semanticTokenProject = BelongsToFileWrapper(getInProject(_).semanticToken)

  // def root: FileManager = if (isLeaf) {
  //   coordinator.getFile(project.toSeq(0))
  // } else manager
  // def isLeaf: Boolean = project.size == 1 && !project(file_uri)
  // def getProject: Seq[FileManager] = 

  // def toUnitProject(): Unit = {
  //   for (p <- project) {

  //   }
  // }

  // def removeFromProject(root: ProjectManager) = {
  //   project match {
  //     case Right(currProject) if currProject eq root => project = Left(HashMap())
  //     case _ => () // Shouldn't happen
  //   }
  // }
  // def removeFromProject(leaf: String) = {
  //   project match {
  //     case Left(project) => project.remove(leaf)
  //     case _ => () // Shouldn't happen
  //   }
  // }

  // def addToProject(root: FileManager) = {
  //   // Kill Project
  //   project match {
  //     case Left(currProject) if currProject.size > 0 =>
  //       coordinator.setupProject(file_uri, Array())
  //     case Right(value) => 
  //     case _ => ()
  //   }
  //   project = Right(root)
  // }

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

    def getLeafManager(uri: String): LeafManager = oldProject.getOrElse(uri, LeafManager(file.file_uri, np(uri)._1.get, coordinator))
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
