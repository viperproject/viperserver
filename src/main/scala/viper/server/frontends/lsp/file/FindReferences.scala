// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j
import viper.silver.ast.utility.lsp
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import viper.server.frontends.lsp.Common

case class FindReferences(
  bound: lsp.SelectionBoundScope,
  references: Seq[lsp.RangePosition]
) extends lsp.HasRangePositions with lsp.SelectableInBound {
  override def rangePositions: Seq[lsp.RangePosition] = bound.scope +: references
}

trait FindReferencesManager extends Manager {
  // FindReferences
  type FindReferencesContainer = utility.StageRangeContainer.RangeContainer[FindReferences, lsp4j.Location]
  val findReferencesContainer: FindReferencesContainer = utility.LspContainer(utility.FindReferencesTranslator)
  addContainer(findReferencesContainer)
  def getFindReferences(pos: lsp4j.Position, includeDeclaration: Boolean): Seq[lsp4j.Location] =
    findReferencesContainer.get((Some(pos), None, false))
      .filter(l => includeDeclaration || Common.containsPosition(l.getRange, pos) != 0)

  def addFindReferences(first: Boolean)(vs: Seq[lsp.ReferenceTo]): Unit = {
    val definitions = HashMap[lsp.RangePosition, ArrayBuffer[lsp.RangePosition]]()
    vs.foreach(v => definitions.getOrElseUpdate(v.to, ArrayBuffer()) += v.from)
    val findReferences = definitions.map(e => FindReferences(lsp.SelectionBoundScope(e._1), e._2.toSeq))
    findReferences.foreach(findReferencesContainer.receive(first, _))
    findReferencesContainer.onUpdate()
  }

  def getRename(references: Seq[lsp4j.Location], newName: String): Option[lsp4j.WorkspaceEdit] = {
    if (references.isEmpty) return None
    val edits = references.groupBy(_.getUri)
      .map(e => e._1 -> e._2.map(l => new lsp4j.TextEdit(l.getRange, newName)).asJava)
      .asJava
    Some(new lsp4j.WorkspaceEdit(edits))
  }
}
