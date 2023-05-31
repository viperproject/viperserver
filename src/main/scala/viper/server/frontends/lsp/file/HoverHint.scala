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
import viper.silver.ast.utility.lsp.{HoverHint, SelectionBoundTrait}
// import viper.server.frontends.lsp.file.utility.RangeSelector
import viper.server.frontends.lsp.file.utility.{StageRangeContainer, HoverHintTranslator, LspContainer}
import viper.server.frontends.lsp.file.utility.Translates

trait HoverHintManager[A <: HoverHintManager[A]] extends CommonFileInfo[A] { this: A =>
  type HoverHintContainer = StageRangeContainer.RangeContainer[HoverHint, lsp4j.Hover]
  val hoverHints: HoverHintContainer = LspContainer(HoverHintTranslator)(StageRangeContainer.default)

  containers.addOne(hoverHints)

  // val hoverHints: RangeSelector[HoverHint] = RangeSelector()

  // def handleHoverHints(all: Seq[HoverHint]) = {
  //   val perFile = all.groupBy(_.bound.scope.map(_.file.toUri().toString()))
  //   // Global
  //   hoverHints.globalKeyword.clear()
  //   perFile.get(None).foreach(global => {
  //     for (hint <- global) {
  //       val keyword = hint.bound.keyword.get
  //       hoverHints.globalKeyword.getOrElseUpdate(keyword, ArrayBuffer()).addOne(hint)
  //     }
  //   })
  //   // Local
  //   perFile.get(Some(file_uri)).foreach(setHoverHints)
  //   for ((Some(uri), symbs) <- perFile) {
  //     if (uri != file_uri) {
  //       coordinator.setHoverHints(uri, symbs)
  //     }
  //   }
  // }

  // def setHoverHints(newHints: Seq[HoverHint]): Unit = {
  //   hoverHints.localKeyword.clear()
  //   hoverHints.localPosition.clear()
  //   newHints.foreach {
  //     case hint@HoverHint(_, SelectionBoundTrait(Some(keyword), _)) =>
  //       hoverHints.localKeyword.getOrElseUpdate(keyword, ArrayBuffer()).addOne(hint)
  //     case hint@HoverHint(_, SelectionBoundTrait(None, _)) =>
  //       hoverHints.localPosition.addOne(hint)
  //   }
  // }
}
