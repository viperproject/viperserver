// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.frontends.lsp.file

import scala.util.{Success, Try}
import scala.collection.mutable.ArrayBuffer
import org.eclipse.lsp4j.Range
import viper.silver.ast.utility.DiskLoader
import java.nio.file.Path
import org.eclipse.lsp4j.Position
import viper.server.frontends.lsp.Common
// import viper.silver.ast.utility.lsp.RangePosition

// case class CharRange(start: Int, end: Int)

case class FileContent(path: Path) extends DiskLoader {
  case class FileContentIter(fc: FileContent, delta: Int, var currPos: Option[Position]) extends Iterator[(Char, Position)] {
    override def hasNext: Boolean = currPos.isDefined
    override def next(): (Char, Position) = {
      val pos = currPos.get
      val char = fc.getCharAt(pos)
      pos.setCharacter(pos.getCharacter + delta)
      currPos = fc.normalize(pos)
      (char, pos)
    }
  }

  val fileContent = new ArrayBuffer[String]

  def set(newContent: String): Unit = {
    fileContent.clear()
    fileContent.addAll(newContent.split("\n", -1))
  }

  override def loadContent(path: Path): Try[String] =
    if (this.path == path) Success(fileContent.mkString("\n")) else super.loadContent(path)

  def handleChange(range: Range, text: String): Unit = {
    val startLine = range.getStart.getLine
    val startStr = fileContent(startLine).slice(0, range.getStart.getCharacter)
    val endLine = range.getEnd.getLine
    val endLineStr = fileContent(endLine)
    val endStr = endLineStr.slice(range.getEnd.getCharacter, endLineStr.length)
    val lines = (startStr ++ text ++ endStr).split("\n", -1)
    fileContent.patchInPlace(startLine, lines, endLine - startLine + 1)
  }

  def iterForward(pos: Position): FileContentIter = FileContentIter(this, 1, normalize(pos))
  def iterBackward(pos: Position): FileContentIter = FileContentIter(this, -1, normalize(pos))

  def normalize(pos: Position): Option[Position] = {
    if (pos.getLine < 0 || pos.getLine >= fileContent.length) return None
    if (pos.getCharacter < 0) {
      val newPos = new Position(pos.getLine - 1, pos.getCharacter)
      while (newPos.getLine >= 0) {
        newPos.setCharacter(newPos.getCharacter + fileContent(newPos.getLine).length + 1)
        if (newPos.getCharacter >= 0) return Some(newPos)
        newPos.setLine(newPos.getLine - 1)
      }
      None
    } else if (pos.getCharacter > fileContent(pos.getLine).length) {
      val newPos = new Position(pos.getLine + 1, pos.getCharacter - fileContent(pos.getLine).length - 1)
      while (newPos.getLine < fileContent.length && newPos.getCharacter <= fileContent(newPos.getLine).length) {
        newPos.setCharacter(newPos.getCharacter - fileContent(newPos.getLine).length - 1)
        newPos.setLine(newPos.getLine + 1)
      }
      if (newPos.getLine < fileContent.length) Some(newPos) else None
    } else {
      return Some(new Position(pos.getLine, pos.getCharacter))
    }
  }
  def getCharAt(pos: Position): Char = {
    val line = fileContent(pos.getLine)
    if (pos.getCharacter == line.length) '\n' else line(pos.getCharacter)
  }

  def getIdentAtPos(pos: Position): Option[(String, Range)] = {
    normalize(pos).flatMap(nPos => {
      val start = iterBackward(nPos).drop(1).takeWhile { case (c, _) => Common.isIdentChar(c) }.length
      val startPos = new Position(nPos.getLine, nPos.getCharacter - start)
      if (!Common.isIdentStartChar(getCharAt(startPos))) return None
      val end = iterForward(nPos).takeWhile { case (c, _) => Common.isIdentChar(c) }.length
      val endPos = new Position(nPos.getLine, nPos.getCharacter + end)
      if (start == 0 && end == 0 || inComment(nPos)) None else {
        val ident = fileContent(nPos.getLine).slice(startPos.getCharacter, endPos.getCharacter)
        Some((ident, new Range(startPos, endPos)))
      }
    })
  }
  def inComment(pos: Position): Boolean = {
    normalize(pos).map(nPos => {
      var isComment = false
      var noComment = false
      iterBackward(nPos).sliding(2).find {
        case Seq((prevChar, _), (currChar, p)) => {
          isComment = isComment || p.getLine() == nPos.getLine() && currChar == '/' && prevChar == '/'
          isComment = isComment || (!noComment && currChar == '/' && prevChar == '*')
          noComment = noComment || currChar == '*' && prevChar == '/'
          isComment || (noComment && p.getLine() != nPos.getLine())
        }
        case _ => false
      }
      isComment
    }).getOrElse(false)
  }
}
object FileContent {
  def apply(path: Path, content: String): FileContent = {
    val fc = new FileContent(path)
    fc.set(content)
    fc
  }
}
