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
import lsp4j.jsonrpc.messages.Either
import viper.silver.ast.QuantifiedExp
import viper.silver.ast.Trigger
import viper.silver.ast.AbstractSourcePosition
import viper.silver.ast.LineColumnPosition
import viper.server.frontends.lsp.file.utility.{StageArrayContainer, InlayHintTranslator, LspContainer}

trait InlayHintManager[A <: InlayHintManager[A]] extends CommonFileInfo[A] { this: A =>
  type InlayHintContainer = StageArrayContainer.ArrayContainer[InlayHint, lsp4j.InlayHint]
  val inlayHint: InlayHintContainer =
    LspContainer(InlayHintTranslator, coordinator.client.refreshInlayHints)

  containers.addOne(inlayHint)

  // private val inlayHints: ArrayBuffer[InlayHint] = ArrayBuffer()

  // def handleInlayHints(all: Seq[InlayHint]) = {
  //   coordinator.logger.debug(s"handleInlayHints: ${all.toString()}")
  //   val perFile = all.groupBy(_.position.file.toUri().toString())
  //   perFile.get(file_uri).foreach(setInlayHints)
  //   for ((uri, inlayHints) <- perFile) {
  //     if (uri != file_uri) {
  //       coordinator.setInlayHints(uri, inlayHints)
  //     }
  //   }
  // }
  // def setInlayHints(newInlayHints: Seq[InlayHint]): Unit = {
  //   inlayHints.clear()
  //   inlayHints.addAll(newInlayHints)
  // }
  // def getInlayHints(): Seq[lsp4j.InlayHint] = (inlayHints.toSeq ++ verifInlayHints.toSeq).map(hint => {
  //   val start = Common.toPosition(hint.position.start)
  //   val labelParts = hint.label.map(label => {
  //     val labelPart = new lsp4j.InlayHintLabelPart(label.value)
  //     label.tooltip.foreach(tooltip => labelPart.setTooltip(new lsp4j.MarkupContent("markdown", tooltip)))
  //     label.location.foreach(location => labelPart.setLocation(Common.toLocation(location)))
  //     labelPart
  //   })
  //   val label = Either.forRight[String, java.util.List[lsp4j.InlayHintLabelPart]](labelParts.asJava)
  //   val ih = new lsp4j.InlayHint(start, label)
  //   hint.kind.foreach(k => ih.setKind(lsp4j.InlayHintKind.forValue(k.id)))
  //   ih.setPaddingLeft(hint.paddingLeft)
  //   ih.setPaddingRight(hint.paddingRight)
  //   ih
  // })

  // Quantifier chosen triggers

  // private val verifInlayHints: ArrayBuffer[InlayHint] = ArrayBuffer()

  def handleQuantifierChosenTriggers(quantifier: QuantifiedExp, triggers: Seq[Trigger], oldTriggers: Seq[Trigger]): Unit = {
    if (!quantifier.exp.pos.isInstanceOf[AbstractSourcePosition]) {
      coordinator.logger.error(s"Got chosen triggers, but quantifier has no position: ${quantifier.toString()}")
      return
    }
    val pos = RangePosition(quantifier.exp.pos.asInstanceOf[AbstractSourcePosition])
    val uri = pos.file.toUri().toString()
    if (uri == file_uri) {
      inlayChosenTriggersAt(pos, triggers, oldTriggers)
    } else {
      coordinator.inlayChosenTriggersAt(uri, pos, triggers, oldTriggers)
    }
  }
  // TODO: tidy this up
  def inlayChosenTriggersAt(start: RangePosition, triggers: Seq[Trigger], oldTriggers: Seq[Trigger]): Unit = {
    coordinator.logger.debug(s"handleQuantifierChosenTriggers: ${start.toString()}, ${triggers.toString()}")
    val oldTriggersBuf = ArrayBuffer.from(oldTriggers)
    var skipFirst = true
    val at = content.scanLeft(Common.toPosition(start.start), _ match {
      case _ if skipFirst => {
        skipFirst = false
        Some(false)
      }
      case ':' | '}' => Some(true)
      case ' ' => Some(false)
      case _ => None
    })
    at match {
      case Left(value) =>
        // TODO: remove the conversion back and forth
        start.start = LineColumnPosition(value.getLine() + 1, value.getCharacter() + 2)
      case Right(value) =>
        coordinator.logger.error(s"Could not find `::` of quantifier, hit unknown character at ${value.toString()}")
        return
    }
    var curr = start
    var isFirst = true
    for (trigger <- triggers) {
      if (oldTriggersBuf.length > 0 && oldTriggersBuf.head == trigger) {
        val removed = oldTriggersBuf.remove(0)
        removed.pos match {
          case pos: AbstractSourcePosition => curr = RangePosition(pos)
          case _ => coordinator.logger.error(s"User written trigger has no position: ${trigger.toString()}")
        }
      } else {
        val ih = InlayHint(curr, Seq(InlayHintLabelPart(trigger.toString())), None, isFirst, true)
        inlayHint.receiveVerify(ih, true)
        isFirst = false
      }
    }
  }
}
