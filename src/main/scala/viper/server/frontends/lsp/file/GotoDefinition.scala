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
import viper.silver.ast.utility.lsp.SelectionBoundTrait
import viper.server.frontends.lsp.file.utility.{StageRangeContainer, GotoDefinitionTranslator, LspContainer}
import viper.server.frontends.lsp.file.utility.Translates

trait GotoDefinitionManager[A <: GotoDefinitionManager[A]] extends CommonFileInfo[A] { this: A =>
  type GotoDefinitionContainer = StageRangeContainer.RangeContainer[GotoDefinition, lsp4j.LocationLink]
  val gotoDefinitions: GotoDefinitionContainer = LspContainer(GotoDefinitionTranslator)(StageRangeContainer.default)

  containers.addOne(gotoDefinitions)

  // val gotoDefinitions: RangeSelector[GotoDefinition] = RangeSelector()

  // def handleGotoDefinitions(all: Seq[GotoDefinition]) = {
  //   val perFile = all.groupBy(_.bound.scope.map(_.file.toUri().toString()))
  //   // Global
  //   gotoDefinitions.globalKeyword.clear()
  //   perFile.get(None).foreach(global => {
  //     for (hint <- global) {
  //       val keyword = hint.bound.keyword.get
  //       gotoDefinitions.globalKeyword.getOrElseUpdate(keyword, ArrayBuffer()).addOne(hint)
  //     }
  //   })
  //   // Local
  //   perFile.get(Some(file_uri)).foreach(setGotoDefinitions)
  //   for ((Some(uri), gotoDefns) <- perFile) {
  //     if (uri != file_uri) {
  //       coordinator.setGotoDefinitions(uri, gotoDefns)
  //     }
  //   }
  // }

  // def setGotoDefinitions(newHints: Seq[GotoDefinition]): Unit = {
  //   gotoDefinitions.localKeyword.clear()
  //   gotoDefinitions.localPosition.clear()
  //   newHints.foreach {
  //     case hint@GotoDefinition(_, SelectionBoundTrait(Some(keyword), _)) =>
  //       gotoDefinitions.localKeyword.getOrElseUpdate(keyword, ArrayBuffer()).addOne(hint)
  //     case hint@GotoDefinition(_, SelectionBoundTrait(None, _)) =>
  //       gotoDefinitions.localPosition.addOne(hint)
  //   }
  // }
}
