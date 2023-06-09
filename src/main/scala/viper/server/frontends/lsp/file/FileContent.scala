// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import scala.util.{Success, Try}
import scala.collection.mutable.ArrayBuffer
import org.eclipse.lsp4j.Range
import viper.silver.ast.utility.DiskLoader
import java.nio.file.Path
import org.eclipse.lsp4j.Position
import viper.server.frontends.lsp.Common

case class FileContent(path: Path, fileContent: ArrayBuffer[String]) extends DiskLoader {
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

  def getIdentAtPos(pos: Position): Option[(String, Range)] = {
    val lineIdx = pos.getLine
    fileContent.lift(lineIdx).flatMap(line => {
      var start = pos.getCharacter
      if (start >= line.length) return None
      while (start > 0 && Common.isIdentChar(line(start - 1))) {
        start -= 1
      }
      if (!Common.isIdentStartChar(line(start))) {
        start += 1
      }
      val ident = line.drop(start).takeWhile(Common.isIdentChar)
      if (ident.isEmpty) None else {
        val range = new Range(new Position(lineIdx, start), new Position(lineIdx, start + ident.length))
        Some((ident, range))
      }
    })
  }

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
  def find(start: Position, pred: Char => Boolean, delta: Int, skipN: Int = 0, bound: Option[Position] = None): Option[Position] = {
    assert(delta != 0)
    var potential = normalize(start)
    var count = 0
    while (potential.isDefined && potential != bound && (skipN > count || !pred(getCharAt(potential.get)))) {
      count += 1
      val curr = potential.get
      curr.setCharacter(curr.getCharacter + delta)
      potential = normalize(curr)
    }
    if (potential == bound) None else potential
  }
  // def scanLeft(start: Position, pred: Char => Option[Boolean]): Either[Position, Position] = {
  //   if (!isValidPos(start)) return Right(start)
  //   var curr = Option(start)
  //   while (curr.isDefined && fileContent(curr.get.getLine).isEmpty()) {
  //     curr = decPos(curr.get)
  //   }
  //   while (curr.isDefined) {
  //     val pos = curr.get
  //     val line = fileContent(pos.getLine)
  //     val char = line(pos.getCharacter)
  //     pred(char) match {
  //       case Some(true) => return Left(pos)
  //       case Some(false) => curr = decPos(pos)
  //       case None => return Right(pos)
  //     }
  //   }
  //   Right(new Position(0, 0))
  // }
}
object FileContent {
  def apply(path: Path, content: String): FileContent = {
    val fileContent = content.split("\n", -1).to(ArrayBuffer)
    new FileContent(path, fileContent)
  }
}
