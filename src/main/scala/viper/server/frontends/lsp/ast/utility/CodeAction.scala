// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.silver.ast.utility.lsp

import org.eclipse.lsp4j
import viper.server.frontends.lsp.file.branchTree.BranchTree

trait HasCodeActions {
  def getCodeActions: Seq[CodeAction]
}

case class CodeAction(
                       title: String,
                       edit: Option[(String, lsp4j.Range)],
                       cmd: Option[(String, Seq[AnyRef])],
                       bound: SelectionBoundScopeTrait,
                       kind: String,
                       resolvedDiags: Seq[lsp4j.Diagnostic] = Seq.empty,
                       branchTree : Option[BranchTree] = None
                     ) extends SelectableInBound with HasRangePositions {
  override def rangePositions: Seq[RangePosition] = bound.rangePositions
}
