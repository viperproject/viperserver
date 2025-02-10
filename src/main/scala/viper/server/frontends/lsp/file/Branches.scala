// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j

trait Branches extends ProjectAware {
    def getBranchRange(uri: String, pos: lsp4j.Position, leftIsFatal: Boolean, rightIsFatal: Boolean): lsp4j.Range = {
        var start = new lsp4j.Position(pos.getLine, 0)
        var end = new lsp4j.Position(pos.getLine, 0)
        val m = getInProject(uri)
        var currPos = m.content.iterForward(start).drop(1).find { case (c, _) => c == '{' }
        if (!currPos.isDefined) return new lsp4j.Range(start, end)
        var openBraceCount = 0
        var movedStart = false
        do {
            val (c, p) = currPos.get
            c match {
                case '}' =>
                    openBraceCount -= 1
                    if (openBraceCount == 0) {
                        end = new lsp4j.Position(p.getLine, 0)
                        if (leftIsFatal && !movedStart) {
                            val nextBracePos = m.content.iterForward(p).drop(1).find { case (c, _) => c == '{' }
                            if (nextBracePos.isDefined) {
                                currPos = nextBracePos
                            }
                            // move start
                            if (!rightIsFatal) {
                                if (nextBracePos.isDefined) { // no else clause
                                    start = new lsp4j.Position(currPos.get._2.getLine, 0)
                                } else if (p.getLine + 1 < m.content.fileContent.length) { // else clause
                                    start = new lsp4j.Position(p.getLine + 1, 0)
                                }
                            }
                            openBraceCount = 1
                            movedStart = true
                        }
                    }
                case '{' =>
                    openBraceCount += 1
                case _ =>
            }
            currPos = m.content.iterForward(currPos.get._2).drop(1).find { case (c, _) => c == '{' || c == '}' }
        } while (currPos.isDefined && openBraceCount > 0)
        new lsp4j.Range(start, end)
    }
}
