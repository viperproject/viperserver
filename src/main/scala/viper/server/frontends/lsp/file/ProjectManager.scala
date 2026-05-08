// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import viper.server.frontends.lsp.{SetupProjectParams, Lsp4jSemanticHighlight}
import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Map}
import org.eclipse.lsp4j.Range

import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp
import viper.silver.ast.utility.lsp.SelectionBoundScopeTrait
import scala.annotation.unused
import scala.concurrent.Future

case class LeafInfo(private val roots: ArrayBuffer[String]) {
  def lastRoot: String = roots.last
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

trait ProjectAware extends ManagesLeaf {
  def getInProjectOpt(uri: String): Option[LeafManager]
  def getInProject(uri: String): LeafManager
  def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation]
}

/** Manages a Viper project consisting of the root file and all imported files.
 * Note that any Viper file can be either of the two: a root of a project or as
 * a leaf in other project(s). See documentation of `ClientCoordinator`.
 *
 * Concurrency: reads and writes of `project` (and the inner `Map` /
 * `LeafInfo`) are guarded by the FileManager's intrinsic monitor (the same
 * lock used by `Verification` mutators per S3). Methods that fan out to
 * other FileManagers via `coordinator.{addToOther,removeFromOther,handleChangeInLeaf}`
 * release the lock before making those cross-instance calls — otherwise
 * concurrent root↔leaf interactions (e.g. `setupProject` on a root vs.
 * `handleContentChange` on one of its leaves) would deadlock through
 * inverted lock acquisition.
 *
 * The protocol is:
 *  1. Read or mutate `project` under `synchronized(this)`.
 *  2. Capture any plan that requires cross-instance calls (a list of leaves
 *     to notify, etc.).
 *  3. Release the lock and execute the cross-instance calls.
 *  4. If a follow-up local mutation depends on the result, re-acquire the
 *     lock and double-check before installing.
 *
 * This protects against torn reads/writes; it does not by itself make
 * compound updates atomic across cross-instance call windows. */
trait ProjectManager extends ProjectAware {
  /** The current project for which this is a root (Left), or the set of project
   * roots for which this file is a leaf (Right). If this is a leaf, this
   * `ProjectManager` will not handle any lsp features: that will instead be
   * done by the `LeafManager` duplicated in the corresponding roots. */
  var project: Either[Map[String, LeafManager], LeafInfo] = Left(Map())
  private def getRootOpt: Option[Map[String, LeafManager]] = project.left.toOption

  def removeFromOtherProject(toRemove: String): Boolean = synchronized {
    val becomeRoot = project.map(li => li.removeRoot(toRemove)).getOrElse(false);
    if (becomeRoot) {
      project = Left(Map())
    }
    becomeRoot
  }
  def addToOtherProject(newRoot: String, getContents: Boolean): Option[String] = {
    // teardownProject performs cross-instance calls and manages its own
    // locking; do it before we reacquire to install the new leaf state.
    teardownProject()
    synchronized {
      project match {
        case Left(_) =>
          project = Right(LeafInfo(newRoot))
        case Right(li) =>
          li.addRoot(newRoot)
      }
    }
    if (getContents) Some(root.content.text) else None
  }

  def addToThisProject(uri: String): LeafManager = {
    val existing = synchronized { getRootOpt.get.get(uri) }
    // Cross-instance call without holding our lock: the leaf may want its
    // own monitor, and one of its callbacks could otherwise deadlock with
    // ours.
    val file = coordinator.addToOtherProject(uri, file_uri, existing.isEmpty)
    existing.getOrElse {
      synchronized {
        // Re-read in case `project` was replaced or another thread already
        // installed a manager for this uri while we were unlocked.
        val pm = getRootOpt.get
        pm.get(uri).getOrElse {
          coordinator.logger.error("Creating new LeafManager for " + uri)
          val l = LeafManager(uri, file.get, coordinator)
          pm.put(uri, l)
          l
        }
      }
    }
  }

  def teardownProject(): Unit = {
    val leaves = synchronized {
      removeDiagnostics()
      projectLeaves.getOrElse(Set.empty).toList
    }
    leaves.foreach(leaf => coordinator.removeFromOtherProject(leaf, file_uri))
  }
  def setupProject(imports: Set[String]): Unit = {
    // Phase 1: under the lock, compute the diff, mutate the inner map, and
    // republish `project`. We do NOT make cross-instance calls here.
    val toRemove = synchronized {
      val oldProject = getRootOpt.getOrElse(Map())
      val tr = oldProject.keySet.diff(imports).toList
      tr.foreach(oldProject.remove)
      project = Left(oldProject)
      tr
    }
    // Phase 2: notify removed leaves outside the lock.
    toRemove.foreach(coordinator.removeFromOtherProject(_, file_uri))
    // Phase 3: addToThisProject manages its own locking.
    for (p <- imports) {
      addToThisProject(p)
    }
    coordinator.client.map(_.requestSetupProject(SetupProjectParams(file_uri, imports.toArray)))
  }

  def projectRoot: Option[String] = synchronized { project.toOption.map(_.lastRoot) }
  def projectLeaves: Option[Set[String]] = synchronized { getRootOpt.map(_.keySet.toSet) }
  def projectManagers: Option[Iterator[LeafManager]] = synchronized {
    // Materialize the iterator under the lock so callers iterate a stable
    // snapshot rather than a live view of the mutable map.
    getRootOpt.map(p => (Iterator(root) ++ p.valuesIterator).toList.iterator)
  }
  def isRoot: Boolean = synchronized { project.isLeft }

  override def getInProjectOpt(uri: String): Option[LeafManager] = synchronized {
    if (unescape(uri) == unescape(file_uri)) Some(root) else getRootOpt.get.get(uri)
  }
  /** Gets a file in the current project, or adds it if missing. The latter can
   * happen when, e.g. we get errors in imported files before we get the
   * `PProgram` itself (to setup the project).
  */
  override def getInProject(uri: String): LeafManager =
    getInProjectOpt(uri).getOrElse(addToThisProject(uri))

  def removeDiagnostics(): Unit = {
    projectManagers.foreach(_.foreach(_.removeDiagnostics()))
  }

  override def resetDiagnostics(first: Boolean): Unit = {
    projectManagers.foreach(_.foreach(_.resetDiagnostics(first)))
  }
  override def resetContainers(first: Boolean): Unit = {
    projectManagers.foreach(_.foreach(_.resetContainers(first)))
  }

  private def addBtf[V <: lsp.HasRangePositions with lsp.BelongsToFile](toC: String => Seq[V] => Unit, vs: Seq[V]): Unit = {
    val grouped = vs.groupBy(_.file.toUri().toString())
    grouped foreach (g => toC(g._1)(g._2))
  }

  private def addSib[V <: lsp.HasRangePositions with lsp.SelectableInBound](toC: String => Seq[V] => Unit, vs: Seq[V]): Unit = {
    val grouped = vs.groupBy(_.bound match {
      case bound: SelectionBoundScopeTrait => bound.scope.file.toUri().toString()
      case _ => file_uri
    })
    grouped foreach (g => toC(g._1)(g._2))
  }

  def addCodeLens(first: Boolean)(vs: Seq[lsp.CodeLens]): Unit =
    addBtf(uri => getInProject(uri).addCodeLens(first), vs)

  override def addDiagnostic(first: Boolean)(vs: Seq[Diagnostic]): Unit =
    addBtf(uri => getInProject(uri).addDiagnostic(first), vs)

  def addDocumentSymbol(first: Boolean)(vs: Seq[lsp.DocumentSymbol]): Unit =
    addBtf(uri => getInProject(uri).addDocumentSymbol(first), vs)

  def addFoldingRange(first: Boolean)(vs: Seq[lsp.FoldingRange]): Unit =
    addBtf(uri => getInProject(uri).addFoldingRange(first), vs)

  def addGotoDefinition(first: Boolean)(vs: Seq[lsp.GotoDefinition]): Unit =
    addSib(uri => getInProject(uri).addGotoDefinition(first), vs)
  def getGotoDefinitionProject(uri: String, pos: lsp4j.Position): Seq[lsp4j.LocationLink] = {
    val keyword = getIdentAtPos(uri, pos)
    (if (uri != file_uri) root.getGotoDefinition(None, keyword) else Seq()) ++
      getInProject(uri).getGotoDefinition(Some(pos), keyword)
  }

  def addHoverHint(first: Boolean)(vs: Seq[lsp.HoverHint]): Unit =
    addSib(uri => getInProject(uri).addHoverHint(first), vs)
  def getHoverHintProject(uri: String, pos: lsp4j.Position): Seq[lsp4j.Hover] = {
    val keyword = getIdentAtPos(uri, pos)
    (if (uri != file_uri) root.getHoverHint(None, keyword) else Seq()) ++
      getInProject(uri).getHoverHint(Some(pos), keyword)
  }

  def addInlayHint(first: Boolean)(vs: Seq[lsp.InlayHint]): Unit =
    addBtf(uri => getInProject(uri).addInlayHint(first), vs)

  def addSemanticHighlight(first: Boolean)(vs: Seq[lsp.SemanticHighlight]): Unit =
    addBtf(uri => getInProject(uri).addSemanticHighlight(first), vs)

  def addSignatureHelp(first: Boolean)(vs: Seq[lsp.SignatureHelp]): Unit =
    addSib(uri => getInProject(uri).addSignatureHelp(first), vs)
  override def getSignatureHelpProject(uri: String, keyword: String, pos: lsp4j.Position, range: Range): Seq[lsp4j.SignatureInformation] =
    (if (uri == file_uri) root.getSignatureHelp(None, Some((keyword, range))) else Seq()) ++
      getInProject(uri).getSignatureHelp(Some(pos), Some((keyword, range)))


  def addFindReferences(first: Boolean)(vs: Seq[lsp.ReferenceTo]): Unit =
    addBtf(uri => getInProject(uri).addFindReferences(first), vs)
  def getFindReferencesProject(uri: String, pos: lsp4j.Position, includeDeclaration: Boolean, fromReferences: Boolean): Seq[lsp4j.Location] = {
    val refs = getInProject(uri).getFindReferences(pos, includeDeclaration)
    if (!refs.isEmpty || !fromReferences) refs else {
      val defns = getGotoDefinitionProject(uri, pos)
      if (defns.length != 1) Nil
      else getInProject(defns(0).getTargetUri).getFindReferences(defns(0).getTargetSelectionRange.getStart, includeDeclaration)
    }
  }

  def addSuggestionScopeRange(first: Boolean)(vs: Seq[lsp.SuggestionScopeRange]): Unit =
    addSib(uri => getInProject(uri).addSuggestionScopeRange(first), vs)

  def addCompletionProposal(first: Boolean)(vs: Seq[lsp.CompletionProposal]): Unit =
    addSib(uri => getInProject(uri).addCompletionProposal(first), vs)
  def getCompletionProposal(uri: String, pos: lsp4j.Position, @unused char: Option[String], @unused u: Unit) = {
    val m = getInProject(uri)
    val scope: lsp.SuggestionScope = m.getSuggestionScopeRange(pos).head
    val c = m.content
    val ident = c.getIdentAtPos(pos)
    val start = ident.map(_._2.getStart).getOrElse(pos)
    // Get character
    val char = c.iterBackward(start).drop(1).find{ case (c, _) => c != ' ' }.map(_._1).getOrElse('\n')
    ((if (uri == file_uri) root.getCompletionProposal(scope, None, char) else Seq()) ++
      getInProject(uri).getCompletionProposal(scope, Some(pos), char)).distinct
  }

  def getIdentAtPos(uri: String, pos: lsp4j.Position): Option[(String, Range)] =
    getInProject(uri).content.getIdentAtPos(pos)

  /** The content of my file changed. Update the text in my local `LeafManager`
   * but also forward the change to all other roots whose project I am a part
   * of, so that they can update their local `LeafManager`s for this file */
  def handleContentChange(range: Range, text: String): Unit = {
    root.handleContentChange(range, text)
    val rootsToNotify = synchronized {
      project.toOption.fold(List.empty[String])(_.rootsIter.toList)
    }
    rootsToNotify.foreach(r => coordinator.handleChangeInLeaf(r, file_uri, range, text))
  }

  /** The `ProjectManager` of one of my leaves received a content change
   * message, update my local `LeafManager` instance to reflect that. */
  def handleChangeInLeaf(leaf: String, range: Range, text: String): Unit = {
    val target = synchronized {
      project match {
        case Left(p) => Right(p.get(leaf))
        case Right(_) => Left(())
      }
    }
    target match {
      case Left(_) =>
        coordinator.logger.error("handleChangeInLeaf called on non-root")
      case Right(None) =>
        coordinator.logger.error(s"handleChangeInLeaf called on project without leaf (${leaf})")
      case Right(Some(v)) =>
        v.handleContentChange(range, text)
    }
  }

  // Can force a refresh in the future if we get new ones, so return immediately
  def getCodeLens(uri: String): Future[Seq[lsp4j.CodeLens]] =
    Future.successful(getInProject(uri).getCodeLens())
  // Currently unused
  def getDiagnostics(uri: String): Future[Seq[lsp4j.Diagnostic]] =
    Future.successful(getInProject(uri).getDiagnostic())
  def getInlayHints(uri: String): Future[Seq[lsp4j.InlayHint]] =
    Future.successful(getInProject(uri).getInlayHint())
  def getSemanticHighlights(uri: String): Future[Seq[Lsp4jSemanticHighlight]] =
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
