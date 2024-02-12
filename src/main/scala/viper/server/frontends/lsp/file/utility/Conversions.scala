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
import viper.server.frontends.lsp.file.{Diagnostic, FindReferences}
import ch.qos.logback.classic.Logger

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import viper.silver.ast.utility.lsp.DocumentSymbol

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
    new lsp4j.Diagnostic(range, diag.message, diag.severity, "viper")
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

case object GotoDefinitionTranslator extends Translates[lsp.GotoDefinition, lsp4j.LocationLink, (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)] {
  override def translate(goto: lsp.GotoDefinition)(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): lsp4j.LocationLink = {
    val range = Common.toRange(goto.target)
    val targetRange = Common.toRange(goto.targetSelection)
    // Origin range:
    val keywordSelectRange = i._2.map(_._2)
    val selectionBoundRange = goto.bound.rangePositions.headOption.map(Common.toRange)
    val originRange = keywordSelectRange.orElse(selectionBoundRange).orNull
    new lsp4j.LocationLink(goto.target.file.toUri().toString(), range, targetRange, originRange)
  }
}

case object HoverHintTranslator extends Translates[lsp.HoverHint, lsp4j.Hover, (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)] {
  override def translate(hh: lsp.HoverHint)(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): lsp4j.Hover = ???
  override def translate(hhs: Seq[lsp.HoverHint])(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean))(implicit log: Logger): Seq[lsp4j.Hover] = {
    val hoverStr = hhs.map(h => s"```\n${h.hint}\n```${h.documentation.map("\n" + _).getOrElse("")}").mkString("\n\n---\n\n")
    // Origin range:
    val highlightRange = hhs.headOption.flatMap(_.highlight).map(Common.toRange)
    val keywordSelectRange = i._2.map(_._2)
    val selectionBoundRange = hhs.headOption.flatMap(_.bound.rangePositions.headOption.map(Common.toRange))
    val range = highlightRange.orElse(keywordSelectRange).orElse(selectionBoundRange).orNull
    val hover = new lsp4j.Hover(new lsp4j.MarkupContent("markdown", hoverStr), range)
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

case object SignatureHelpTranslator extends Translates[lsp.SignatureHelp, lsp4j.SignatureInformation, (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)] {
  override def translate(sh: lsp.SignatureHelp)(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): lsp4j.SignatureInformation = {
    val label = sh.help.map(_.text).mkString
    val si = new lsp4j.SignatureInformation(label)
    sh.documentation.foreach(d => si.setDocumentation(new lsp4j.MarkupContent("markdown", d)))
    val parameters = ArrayBuffer[lsp4j.ParameterInformation]()
    var offset = 0
    sh.help.foreach(h => {
      if (h.isArgument) {
        val pi = new lsp4j.ParameterInformation()
        val pos = new lsp4j.jsonrpc.messages.Tuple.Two[Integer, Integer](offset, offset + h.text.length)
        pi.setLabel(pos)
        h.documentation.foreach(d => pi.setDocumentation(new lsp4j.MarkupContent("markdown", d)))
        parameters.addOne(pi)
      }
      offset += h.text.length
    })
    si.setParameters(parameters.toList.asJava)
    si
  }
}

case object FindReferencesTranslator extends Translates[FindReferences, lsp4j.Location, (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)] {
  override def translate(fr: FindReferences)(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): lsp4j.Location = ???
  override def translate(frs: Seq[FindReferences])(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean))(implicit log: Logger): Seq[lsp4j.Location] = {
    frs.flatMap(_.references.map(Common.toLocation))
  }
}

case object CompletionProposalTranslator extends Translates[lsp.CompletionProposal, lsp4j.CompletionItem, (lsp.SuggestionScope, Option[lsp4j.Position], Char)] {
  override def translate(cp: lsp.CompletionProposal)(i: (lsp.SuggestionScope, Option[lsp4j.Position], Char)): lsp4j.CompletionItem = ???
  override def translate(cps: Seq[lsp.CompletionProposal])(i: (lsp.SuggestionScope, Option[lsp4j.Position], Char))(implicit log: Logger): Seq[lsp4j.CompletionItem] = {
    val (scope, _, char) = i
    def toAlphOrder(i: Int): String = {
      assert(i >= 0)
      val s = i.toString
      s"${s.length.toChar}$s"
    }
    // var group = 0
    cps.map(cp => {
      // while (group < i._1.length && i._1(group) != cp.bound.scope) group += 1
      val ci = new lsp4j.CompletionItem(cp.label)
      val detail = new lsp4j.CompletionItemLabelDetails()
      cp.detail.foreach(detail.setDetail(_))
      cp.description.foreach(detail.setDescription(_))
      ci.setLabelDetails(detail)
      cp.documentationTitle.foreach(d => ci.setDetail(d))
      cp.documentation.foreach(d => ci.setDocumentation(new lsp4j.MarkupContent("markdown", d)))
      ci.setKind(lsp4j.CompletionItemKind.forValue(cp.kind.id))
      ci.setInsertTextFormat(lsp4j.InsertTextFormat.forValue(cp.insertTextFormat.id))
      ci.setTags(cp.tags.map(t => lsp4j.CompletionItemTag.forValue(t.id)).asJava)
      val scopeOrder = cp.suggestionBound.scope.get(scope).getOrElse(Byte.MinValue)
      val charOrder = cp.suggestionBound.starting.flatMap(_.get(char)).getOrElse(Byte.MinValue)
      // A lower order means a higher priority, thus we use `-order` here.
      val sortOrder = toAlphOrder(Byte.MaxValue - scopeOrder.max(charOrder).toInt)
      ci.setSortText(sortOrder)
      ci.setInsertText(cp.newText.getOrElse(cp.label))
      ci
    })
  }
}

case object SuggestionScopeRangeTranslator extends Translates[lsp.SuggestionScopeRange, lsp.SuggestionScope, (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)] {
  override def translate(ssr: lsp.SuggestionScopeRange)(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): lsp.SuggestionScope = ???
  override def translate(ssrs: Seq[lsp.SuggestionScopeRange])(i: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean))(implicit log: Logger): Seq[lsp.SuggestionScope] = {
    def ordering(ssr: lsp.SuggestionScopeRange) = (ssr.range._end.line - ssr.range.start.line, ssr.range._end.column - ssr.range.start.column)
    val min = ssrs.minByOption(ordering).map(_.scope).getOrElse(lsp.MemberScope)
    Seq(min)
  }
}
