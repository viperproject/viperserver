// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j
import scala.collection.mutable.{ArrayBuffer, HashMap}

import scala.jdk.CollectionConverters._
import viper.silver.reporter.Import
import viper.silver.ast.utility.lsp._
import viper.server.frontends.lsp.Common
// import viper.server.frontends.lsp.file.RangeSelector
import viper.silver.ast.utility.lsp.{HoverHint, SelectionBoundTrait}
import viper.server.frontends.lsp.file.utility.{StageArrayContainer, FoldingRangeTranslator, LspContainer}
import scala.concurrent.Future

trait FoldingRangeManager[A <: FoldingRangeManager[A]] extends CommonFileInfo[A] { this: A =>
  type FoldingRangeContainer = StageArrayContainer.ArrayContainer[FoldingRange, lsp4j.FoldingRange]
  val foldingRange: FoldingRangeContainer =
    LspContainer(FoldingRangeTranslator)

  containers.addOne(foldingRange)

  // private val foldingRanges: ArrayBuffer[FoldingRange] = ArrayBuffer()

  // def handleFoldingRanges(all: Seq[FoldingRange]) = {
  //   val perFile = all.groupBy(_.range.file.toUri().toString())
  //   perFile.get(file_uri).foreach(setFoldingRanges)
  //   for ((uri, foldRanges) <- perFile) {
  //     if (uri != file_uri) {
  //       coordinator.setFoldingRanges(uri, foldRanges)
  //     }
  //   }
  // }
  // def setFoldingRanges(symbols: Seq[FoldingRange]): Unit = {
  //   foldingRanges.clear()
  //   foldingRanges.addAll(symbols)
  // }
  // def getFoldingRanges(): Seq[lsp4j.FoldingRange] = foldingRanges.map(fold => {
  //   val range = Common.toRange(fold.range)
  //   val fr = new lsp4j.FoldingRange(range.getStart().getLine(), range.getEnd().getLine())
  //   if (!fold.ignoreStartColumn) fr.setStartCharacter(range.getStart().getCharacter())
  //   if (!fold.ignoreEndColumn) fr.setEndCharacter(range.getEnd().getCharacter() - 1)
  //   fr.setKind(fold.kind)
  //   fr
  // }).toSeq
}
