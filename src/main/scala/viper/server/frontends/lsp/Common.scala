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
import viper.silver.ast
import org.eclipse.lsp4j.Location

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

  def toPosition(pos: ast.Position): Position = pos match {
    case lc: ast.HasLineColumn => new Position(lc.line - 1, lc.column - 1)
    case ast.NoPosition => new Position(0, 0)
    case _: ast.VirtualPosition => new Position(0, 0)
  }
  def toRange(sp: ast.AbstractSourcePosition): Range = {
    val start = toPosition(sp.start)
    new Range(start, sp.end.map(toPosition).getOrElse(start))
  }
  def toLocation(sp: ast.AbstractSourcePosition): Location =
    new Location(sp.file.toUri().toString(), toRange(sp))

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
  def containsPosition(range: Range, pos: Position): Int = {
    if (Common.comparePosition(pos, range.getStart) < 0) -1
    else if (Common.comparePosition(range.getEnd, pos) < 0) 1
    else 0
  }

  /** `-4` if `r1 < r2`, `4` if `r1 > r2`,
   * `-3` if `r1 <= r2`, `3` if `r1 => r2`, `0` if they are equal
   * `-2` if `r1` contains `r2` start, `2` if `r1` contains `r2` end
   * `-1` if `r1` is contained in `r2`, `1` if `r2` is contained in `r1`
  */
  def compareRange(r1: Range, r2: Range): Int = {
    val endStart = Common.comparePosition(r1.getEnd, r2.getStart)
    if (endStart < 0) return -4
    if (endStart == 0) return -3
    val startEnd = Common.comparePosition(r1.getStart, r2.getEnd)
    if (startEnd > 0) return 4
    if (startEnd == 0) return 3
    val startStart = Common.comparePosition(r1.getStart, r2.getStart)
    val endEnd = Common.comparePosition(r1.getEnd, r2.getEnd)
    if (endEnd == 0 && startStart == 0) 0
    else if (endEnd <= 0 && startStart <= 0) -2
    else if (endEnd >= 0 && startStart >= 0) 2
    else if (endEnd <= 0 && startStart >= 0) -1
    else if (endEnd >= 0 && startStart <= 0) 1
    else ??? // unreachable
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

  def isIdentChar(c: Char): Boolean = {
    c.isDigit || isIdentStartChar(c)
  }
  def isIdentStartChar(c: Char): Boolean = {
    c.isLetter || c == '_' || c == '$'
  }
}
