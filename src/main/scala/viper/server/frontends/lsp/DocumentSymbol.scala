// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp

import org.eclipse.lsp4j
import scala.collection.mutable.{ArrayBuffer, HashMap}

import scala.jdk.CollectionConverters._
import viper.silver.reporter.Import

case class DocumentSymbolManager() {
  val symbolInformation: HashMap[String, ArrayBuffer[DocumentSymbolProvider]] = HashMap()
  def symbolInformationForFile(uri: String): Seq[lsp4j.DocumentSymbol] =
    symbolInformation.get(uri).toSeq.map(_.map(_.getDocumentSymbol(this))).flatten
}

trait DocumentSymbolProvider {
  def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol
}
case class DocumentSymbolMember(s: lsp4j.DocumentSymbol) extends DocumentSymbolProvider {
    override def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol = s
}

case class ImportMember(i: Import) extends DocumentSymbolProvider {
    override def getDocumentSymbol(implicit si: DocumentSymbolManager): lsp4j.DocumentSymbol = {
        val start = i.from.start
        val end = i.from.end.getOrElse(start)
        val range = new lsp4j.Range(
            new lsp4j.Position(start.line - 1, start.column - 1),
            new lsp4j.Position(end.line - 1, end.column - 1),
        )
        val filename = i.file.getFileName().toString()
        val children = si.symbolInformationForFile(i.file.toUri().toString())
        new lsp4j.DocumentSymbol(filename, lsp4j.SymbolKind.File, range, range, null, children.asJava)
    }
}
