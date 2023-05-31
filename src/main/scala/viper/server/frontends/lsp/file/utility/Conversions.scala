// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file.utility

import org.eclipse.lsp4j
import org.eclipse.lsp4j.jsonrpc.messages.Either
import viper.silver.ast.utility.lsp
import viper.server.frontends.lsp.{Common, Lsp4jSemanticHighlight}
import viper.server.frontends.lsp.file.Diagnostic
import ch.qos.logback.classic.Logger

import scala.jdk.CollectionConverters._
import viper.silver.ast.AbstractSourcePosition
import viper.silver.ast.utility.lsp.SemanticHighlight

trait Translates[-T, +U, I] {
  def translate(t: T)(i: I): U
  def translate(t: Seq[T])(i: I)(implicit @annotation.unused log: Logger): Seq[U] = t.map(translate(_)(i))
}

case object CodeLensTranslator extends Translates[lsp.CodeLens, lsp4j.CodeLens, Unit] {
  override def translate(lens: lsp.CodeLens)(i: Unit): lsp4j.CodeLens = {
    val range = Common.toRange(lens.range)
    val command = new lsp4j.Command(lens.command, "")
    new lsp4j.CodeLens(range, command, null)
  }
}

case object DiagnosticTranslator extends Translates[Diagnostic, lsp4j.Diagnostic, Unit] {
  override def translate(diag: Diagnostic)(i: Unit): lsp4j.Diagnostic = {
    val range = Common.toRange(diag.position)
    new lsp4j.Diagnostic(range, diag.message, lsp4j.DiagnosticSeverity.Error, "")
  }
}

case object DocumentSymbolTranslator extends Translates[lsp.DocumentSymbol, lsp4j.DocumentSymbol, Unit] {
  override def translate(symbol: lsp.DocumentSymbol)(i: Unit): lsp4j.DocumentSymbol = {
    val range = Common.toRange(symbol.range)
    val selectionRange = Common.toRange(symbol.selectionRange)
    val kind = lsp4j.SymbolKind.forValue(symbol.kind.id)
    val detail = symbol.detail.getOrElse(null)
    val children = symbol.children.map(translate(_)(i))
    // Document symbols can only have a range within the current file, but imo it's nice to display
    // the imported symbols as well. So show these under the same range as the import statement (fixedRange).
    // val importedChildren = symbol.imports.toSeq.flatMap(i =>
    //   coordinator.getSymbolsForFile(i.toUri().toString(), Some(selectionRange))
    // )
    // val allChildren = (children ++ importedChildren).asJava
    new lsp4j.DocumentSymbol(symbol.name, kind, range, selectionRange, detail, children.asJava)
  }
}

case object FoldingRangeTranslator extends Translates[lsp.FoldingRange, lsp4j.FoldingRange, Unit] {
  override def translate(fold: lsp.FoldingRange)(i: Unit): lsp4j.FoldingRange = {
    val range = Common.toRange(fold.range)
    val fr = new lsp4j.FoldingRange(range.getStart().getLine(), range.getEnd().getLine())
    if (!fold.ignoreStartColumn) fr.setStartCharacter(range.getStart().getCharacter())
    if (!fold.ignoreEndColumn) fr.setEndCharacter(range.getEnd().getCharacter() - 1)
    fr.setKind(fold.kind)
    fr
  }
}

case object GotoDefinitionTranslator extends Translates[lsp.GotoDefinition, lsp4j.LocationLink, (String, lsp4j.Position, lsp4j.Range)] {
  override def translate(goto: lsp.GotoDefinition)(i: (String, lsp4j.Position, lsp4j.Range)): lsp4j.LocationLink = {
    val definition = Common.toRange(goto.definition)
    // TODO: make this an actual location link
    new lsp4j.LocationLink(goto.definition.file.toUri().toString(), definition, definition, i._3)
  }
}

case object HoverHintTranslator extends Translates[lsp.HoverHint, lsp4j.Hover, (String, lsp4j.Position, lsp4j.Range)] {
  override def translate(hh: lsp.HoverHint)(i: (String, lsp4j.Position, lsp4j.Range)): lsp4j.Hover = ???
  override def translate(hhs: Seq[lsp.HoverHint])(i: (String, lsp4j.Position, lsp4j.Range))(implicit log: Logger): Seq[lsp4j.Hover] = {
    val hoverStr = hhs.map(_.hint).mkString("\n---\n")
    val hover = new lsp4j.Hover(new lsp4j.MarkupContent("markdown", hoverStr), i._3)
    // TODO: return non-seq
    Seq(hover)
  }
}

case object InlayHintTranslator extends Translates[lsp.InlayHint, lsp4j.InlayHint, Unit] {
  override def translate(inlay: lsp.InlayHint)(i: Unit): lsp4j.InlayHint = {
    val start = Common.toPosition(inlay.position.start)
    val labelParts = inlay.label.map(label => {
      val labelPart = new lsp4j.InlayHintLabelPart(label.value)
      label.tooltip.foreach(tooltip => labelPart.setTooltip(new lsp4j.MarkupContent("markdown", tooltip)))
      label.location.foreach(location => labelPart.setLocation(Common.toLocation(location)))
      labelPart
    })
    val label = Either.forRight[String, java.util.List[lsp4j.InlayHintLabelPart]](labelParts.asJava)
    val ih = new lsp4j.InlayHint(start, label)
    inlay.kind.foreach(k => ih.setKind(lsp4j.InlayHintKind.forValue(k.id)))
    ih.setPaddingLeft(inlay.paddingLeft)
    ih.setPaddingRight(inlay.paddingRight)
    ih
  }
}

case object SemanticHighlightTranslator extends Translates[lsp.SemanticHighlight, Lsp4jSemanticHighlight, Unit] {
  override def translate(token: lsp.SemanticHighlight)(i: Unit): Lsp4jSemanticHighlight = ???
  override def translate(tokens: Seq[lsp.SemanticHighlight])(i: Unit)(implicit log: Logger): Seq[Lsp4jSemanticHighlight] = {
    val sorted = tokens.sortBy(t => (t.pos.start.line, t.pos.start.column))
    val prev = new lsp4j.Position(0, 0)
    sorted.flatMap {
      case t@lsp.SemanticHighlight(pos, typ, modifiers) => {
        val start = Common.toPosition(pos.start)
        if (pos.start.line != pos._end.line) {
          log.error(s"Multiline semantic tokens are not supported: ${t.toString}.")
          None
        } else if (start.getLine <= prev.getLine && start.getCharacter < prev.getCharacter) {
          log.error(s"Overlapping semantic tokens are not supported: ${t.toString}.")
          None
        } else {
          if (start.getLine != prev.getLine) {
            prev.setCharacter(0)
          }
          val len = pos._end.column - pos.start.column
          val (line, column, length): (Integer, Integer, Integer) = (start.getLine - prev.getLine, start.getCharacter - prev.getCharacter, len)
          prev.setLine(prev.getLine + line)
          prev.setCharacter(prev.getCharacter + column)

          val mod = modifiers.foldLeft(0)((acc, m) => acc | (1 << m.id))
          Some(Lsp4jSemanticHighlight(line, column, length, typ.id, mod))
        }
      }
    }
  }
}

case object SignatureHelpTranslator extends Translates[lsp.SignatureHelp, lsp4j.SignatureHelp, Unit] {
  override def translate(sh: lsp.SignatureHelp)(i: Unit): lsp4j.SignatureHelp = {
    ???
  }
}
