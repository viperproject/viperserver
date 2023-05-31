// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j
import scala.collection.mutable.ArrayBuffer

import scala.jdk.CollectionConverters._
import viper.silver.reporter.Import
import viper.silver.ast.utility.lsp._
import viper.server.frontends.lsp.Common
import scala.concurrent.Future
import viper.server.frontends.lsp.file.utility.{StageArrayContainer, DocumentSymbolTranslator, LspContainer}

trait DocumentSymbolManager[A <: DocumentSymbolManager[A]] extends CommonFileInfo[A] { this: A =>
  type DocumentSymbolContainer = StageArrayContainer.ArrayContainer[DocumentSymbol, lsp4j.DocumentSymbol]
  val documentSymbol: DocumentSymbolContainer =
    LspContainer(DocumentSymbolTranslator)

  containers.addOne(documentSymbol)

  // val symbolInformation: ArrayBuffer[DocumentSymbol] = ArrayBuffer()

  // def handleProgramSymbols(all: Seq[DocumentSymbol]) = {
  //   val perFile = all.groupBy(_.range.file.toUri().toString())
  //   perFile.get(file_uri).foreach(setSymbols)
  //   for ((uri, symbs) <- perFile) {
  //     if (uri != file_uri) {
  //       coordinator.setSymbols(uri, symbs)
  //     }
  //   }
  // }
  // def setSymbols(symbols: Seq[DocumentSymbol]): Unit = {
  //   symbolInformation.clear()
  //   symbolInformation.addAll(symbols)
  // }

  // def getDocumentSymbols(fixedRange: Option[lsp4j.Range]): Seq[lsp4j.DocumentSymbol] =
  //   symbolInformation.map(getSymbol(_, fixedRange)).toSeq
  
  // private def getSymbol(symbol: DocumentSymbol, fixedRange: Option[lsp4j.Range]): lsp4j.DocumentSymbol = {
  //   val range = fixedRange.getOrElse(Common.toRange(symbol.range))
  //   val selectionRange = fixedRange.getOrElse(Common.toRange(symbol.selectionRange))
  //   val kind = lsp4j.SymbolKind.forValue(symbol.kind.id)
  //   val detail = symbol.detail.getOrElse(null)
  //   val children = symbol.children.map(c => getSymbol(c, fixedRange))
  //   // Document symbols can only have a range within the current file, but imo it's nice to display
  //   // the imported symbols as well. So show these under the same range as the import statement (fixedRange).
  //   val importedChildren = symbol.imports.toSeq.flatMap(i =>
  //     coordinator.getSymbolsForFile(i.toUri().toString(), Some(selectionRange))
  //   )
  //   val allChildren = (children ++ importedChildren).asJava
  //   new lsp4j.DocumentSymbol(symbol.name, kind, range, selectionRange, detail, allChildren)
  // }
}

// trait DocumentSymbolProvider {
//   def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol
// }
// case class DocumentSymbolMember(s: lsp4j.DocumentSymbol) extends DocumentSymbolProvider {
//     override def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol = s
// }

// case class ImportMember(i: Import) extends DocumentSymbolProvider {
//     override def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol = {
//         // val start = i.from.start
//         // val end = i.from.end.getOrElse(start)
//         // val range = new lsp4j.Range(
//         //     new lsp4j.Position(start.line - 1, start.column - 1),
//         //     new lsp4j.Position(end.line - 1, end.column - 1),
//         // )
//         // val filename = i.file.getFileName().toString()
//         // val children = si.symbolInformationForFile(i.file.toUri().toString())
//         // new lsp4j.DocumentSymbol(filename, lsp4j.SymbolKind.File, range, range, null, children.asJava)
//         ???
//     }
// }
