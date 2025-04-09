// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import viper.server.frontends.lsp.Common
import viper.silver.ast.Method
import viper.silver.parser.PKw
import org.eclipse.lsp4j
import org.eclipse.lsp4j.Position

trait Branches extends ProjectAware {
    def getBeamRange(uri: String, method: Method, failsInElseOnly: Boolean): lsp4j.Range = {
        var start = Common.toPosition(method.pos)
        val m = getInProject(uri)
        var currPos = m.content.iterForward(start)
          .drop(1).find { case (c, _) => c == '{' }
        if (currPos.isDefined) start = currPos.get._2
        currPos = m.content.iterForward(currPos.get._2)
          .drop(1).find { case (c, _) => c == '{' }
        if (currPos.isDefined) start = currPos.get._2
        var middleLn = start.getLine
        var endLn = start.getLine
        var openBraceCount = 0
        do {
            val (c,p) = currPos.get
            c match {
                case '}' =>
                    openBraceCount -= 1
                    if (openBraceCount == 0) {
                        val ln = if (!m.content.fileContent(p.getLine)
                                        .contains(PKw.Else.keyword))
                                 p.getLine+1
                                 else p.getLine
                        if (start.getLine == middleLn) {
                            middleLn = ln
                            openBraceCount += 1
                        } else {
                            endLn = ln
                        }
                    }
                case '{' =>
                    openBraceCount += 1
                case _ =>
            }
            currPos = m.content.iterForward(currPos.get._2).drop(1).find { case (c, _) => c == '{' || c == '}' }
        } while (currPos.isDefined && openBraceCount > 0)
        new lsp4j.Range(
            new Position(if (failsInElseOnly) middleLn else start.getLine,0),
            new Position(endLn,0)
        )
    }
}
