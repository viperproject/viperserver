// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import viper.silver.ast.utility.lsp._
import viper.server.frontends.lsp.Common
import viper.silver.ast.{AbstractSourcePosition, QuantifiedExp, Trigger}

trait QuantifierInlayHints extends ProjectAware {
  def handleQuantifierChosenTriggers(quantifier: QuantifiedExp, triggers: Seq[Trigger], oldTriggers: Seq[Trigger]): Unit = {
    if (!quantifier.exp.pos.isInstanceOf[AbstractSourcePosition]) {
      coordinator.logger.error(s"Got chosen triggers, but quantifier has no position: ${quantifier.toString()}")
      return
    }
    val pos = RangePosition(quantifier.exp.pos.asInstanceOf[AbstractSourcePosition])
    val uri = pos.file.toUri().toString()
    inlayChosenTriggersAt(getInProject(uri), pos, triggers, oldTriggers)
  }
  private def inlayChosenTriggersAt(m: FullManager, expStart: RangePosition, triggers: Seq[Trigger], oldTriggers: Seq[Trigger]): Unit = {
    val start = Common.toPosition(expStart.start)
    m.content.iterBackward(start).drop(1).find { case (c, _) => c != ' ' && c != '\n' } match {
      case Some((_, found)) =>
        found.setCharacter(found.getCharacter + 2)
        val trueStart = m.content.normalize(found).get
        expStart.shiftStart(trueStart.getLine - start.getLine, trueStart.getCharacter - start.getCharacter)
      case None =>
    }
    expStart._end = expStart.start
    var isFirst = true
    for (trigger <- triggers) {
      if (!oldTriggers.contains(trigger)) {
        val ih = InlayHint(expStart, Seq(InlayHintLabelPart(trigger.toString())), None, isFirst, true)
        m.inlayHintContainer.receive(false, ih)
        isFirst = false
      }
    }
  }
}
