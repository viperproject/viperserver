// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j
import viper.server.frontends.lsp.Common
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

trait SignatureHelp extends ProjectAware {
    var oldSignatureHelpStart: Option[lsp4j.Position] = None
    def getSignatureHelp(uri: String, pos: lsp4j.Position, isRetrigger: Boolean): Option[lsp4j.SignatureHelp] = {
        val startOpt = if (isRetrigger) oldSignatureHelpStart else None
        val start = startOpt.getOrElse(new lsp4j.Position(pos.getLine, 0))
        if (Common.comparePosition(start, pos) >= 0) {
            oldSignatureHelpStart = None
            return None
        }
        val signatureHelps = ArrayBuffer[lsp4j.SignatureInformation]()
        val m = getInProject(uri)
        var closeBracketCount = 0
        var commaCount = 0
        var currPos = m.content.iterBackward(pos).drop(1).takeWhile(_._2 != start).find { case (c, _) => c == ',' || c == '(' || c == ')' }
        while (currPos.isDefined) {
            var (c, p) = currPos.get
            c match {
                case ',' if closeBracketCount == 0 =>
                    commaCount += 1
                case ')' =>
                    closeBracketCount += 1
                case '(' if closeBracketCount > 0 =>
                    closeBracketCount -= 1
                case '(' => {
                    val identEnd = m.content.iterBackward(p).drop(1).takeWhile(_._2 != start).find { case (c, _) => c != ' ' && c != '\n' }
                    identEnd.map(_._2).flatMap(m.content.getIdentAtPos).foreach(i => {
                        p = i._2.getStart
                        val sh = getSignatureHelpProject(uri, i._1, i._2.getStart, i._2)
                        sh.map(_.setActiveParameter(commaCount))
                        signatureHelps.addAll(sh)
                    })
                    commaCount = 0
                }
                case _ =>
            }
            currPos = m.content.iterBackward(p).drop(1).takeWhile(_._2 != start).find { case (c, _) => c == ',' || c == '(' || c == ')' }
        }
        if (signatureHelps.length > 0) {
            oldSignatureHelpStart = Some(start)
            Some(new lsp4j.SignatureHelp(signatureHelps.reverse.toList.asJava, signatureHelps.length - 1, 0))
        } else {
            None
        }
    }
}
