// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}

import org.eclipse.lsp4j.{Position, Range}
import scala.collection.mutable.ArrayBuffer


object Common {

  def uriFromString(uri: String): URI = {
    URI.create(uri)
  }

  def uriToPath(uri: URI): Path = {
    Paths.get(uri)
  }

  def filenameFromUri(uri: String): String = {
    Paths.get(uri).getFileName.toString
  }

  def comparePosition(a: Position, b: Position): Int = {
    if (a == null && b == null) return 0
    if (a != null && b == null) return -1
    if (a == null && b != null) return 1
    if (a.getLine < b.getLine || (a.getLine == b.getLine && a.getCharacter < b.getCharacter)) {
      -1
    } else if (a.getLine == b.getLine && a.getCharacter == b.getCharacter) {
      0
    } else {
      1
    }
  }

  def isGlobalRange(range: Range): Boolean = range == null
  def containsPos(range: Range, pos: Position): Boolean = {
    Common.comparePosition(range.getStart, pos) <= 0 &&
    Common.comparePosition(range.getEnd, pos) >= 0
  }

  /** `-1` if `r1 < r2`, `0` if they overlap (not necessarily equal), `1` if `r1 > r2` */
  def compareRange(r1: Range, r2: Range): Int = {
    if (Common.comparePosition(r1.getEnd, r2.getStart) < 0) -1
    else if (Common.comparePosition(r2.getEnd, r1.getStart) < 0) 1
    else 0
  }

  /** returns 0 if equal, 1 if v1 is bigger than v2, -1 otherwise */
  def compareSemVer(v1: String, v2: String): Int = {
    val v1Parts = v1.split('.') // single quotes to use it as string and not as regex
      .map(part => part.toInt)
    val v2Parts = v2.split('.') // single quotes to use it as string and not as regex
      .map(part => part.toInt)
    val zippedParts = v1Parts.zipAll(v2Parts, 0, 0)
    zippedParts.collectFirst {
      case (component1, component2) if component1 > component2 => 1
      case (component1, component2) if component1 < component2 => -1
    }.getOrElse(0)
  }

  /** traverse a string and find all patterns of the form `call(`.
   * Additionally, count the number of `,` after each such pattern.
   */
  def findCallsIn(range: String): Seq[(Position, Int, String)] = {
    val len = range.length()
    if (len < 2) {
      return Seq()
    }
    var bracketCount = 0
    var bracketMinimum = 0
    var commaCount = 0
    val calls = ArrayBuffer[(Position, Int, String)]()
    var recordingCallName = false
    var potentialCallName = ""
    var callsOnCurrLine = 0
    // Traverse backwards
    for (i <- len-1 to 0 by -1) {
      val c = range(i)
      // Update positions
      if (c != '\n' && callsOnCurrLine > 0) {
        for (i <- 0 to callsOnCurrLine - 1) {
          val pos = calls(i)._1
          pos.setCharacter(pos.getCharacter + 1)
        }
      }
      // Currently in the middle of a call name
      if (recordingCallName) {
        var end = true
        if (i > 0) {
          val nextChar = range(i-1)
          if (potentialCallName.isEmpty() && nextChar == ' ') {
            // Do nothing, but keep recording until we hit the start of a string
            end = false
          } else if (nextChar.isLetterOrDigit || nextChar == '_' || nextChar == '$') {
            potentialCallName = s"$c$potentialCallName"
            end = false
          }
        }
        // The next character is an invalid character for a call name, so end here
        if (end) {
          recordingCallName = false
          if (!c.isDigit) {
            // Add the last non-digit character
            potentialCallName = s"$c$potentialCallName"
          }
          if (!potentialCallName.isEmpty()) {
            callsOnCurrLine = callsOnCurrLine + 1
            calls.addOne((new Position(0, 0), commaCount, potentialCallName))
          }
          commaCount = 0
          potentialCallName = ""
        }
      }
      // Handle current character
      c match {
        case '(' => {
          bracketCount = bracketCount - 1
          // Was just at `bracketMinimum`, start recording a new call name
          if (bracketCount < bracketMinimum) {
            bracketMinimum = bracketCount
            // If this is just a regular use of brackets (and not a call),
            // we will get `potentialCallName.isEmpty()` above and set `commaCount = 0`
            recordingCallName = true
          }
        }
        case ')' => bracketCount = bracketCount + 1
        case ',' => {
          if (bracketCount <= bracketMinimum) {
            commaCount = commaCount + 1
          }
        }
        case '\n' => {
          // Update positions
          callsOnCurrLine = 0
          calls.foreach { case (pos, _, _) => pos.setLine(pos.getLine + 1) }
        }
        case _ => {}
      }
    }
    calls.toSeq
  }
}
