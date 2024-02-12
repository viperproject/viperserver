// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file.utility

import org.eclipse.lsp4j

import scala.collection.mutable.ArrayBuffer
import viper.silver.ast.utility.lsp.{BelongsToFile, HasRangePositions, RangePosition, SelectableInBound}
import viper.server.frontends.lsp.Common
import ch.qos.logback.classic.Logger
import scala.collection.mutable.HashMap
import viper.silver.ast.utility.lsp
import viper.silver.ast.utility.lsp.SelectionBoundScopeTrait
import viper.silver.ast.utility.lsp.SelectionBoundKeywordTrait

trait StageContainer[K, V, I] {
  var resetCount: Int = 0
  def reset(): Unit = {
    resetCount += 1
    innerReset()
  }
  protected def innerReset(): Unit
  def add(v: V): (Int, I) = (resetCount, innerAdd(v))
  protected def innerAdd(v: V): I
  def get(k: K): Seq[V]
  def update(i: (Int, I), f: V => V): Boolean =
    if (i._1 == resetCount) innerUpdate(i._2, f) else false
  protected def innerUpdate(i: I, f: V => V): Boolean
  def all(): Iterator[V]
  def filterInPlace(p: V => Boolean): Unit
}

case class StageArrayContainer[V]() extends StageContainer[Unit, V, Int] {
  private val array: ArrayBuffer[V] = ArrayBuffer()
  override protected def innerReset(): Unit = array.clear()
  override protected def innerAdd(v: V): Int = {
    array.addOne(v)
    array.length - 1
  }
  override def get(k: Unit): Seq[V] = array.toSeq
  override protected def innerUpdate(i: Int, f: V => V): Boolean = {
    array(i) = f(array(i))
    true
  }
  override def all(): Iterator[V] = array.iterator
  def filterInPlace(p: V => Boolean): Unit = array.filterInPlace(p)
}
object StageArrayContainer {
  type ArrayContainer[V <: HasRangePositions with BelongsToFile, U] = LspContainer[StageArrayContainer[V], Unit, V, Int, U]
  implicit def default[V]: () => StageArrayContainer[V] = () => StageArrayContainer()
}

case class StageRangeContainer[V <: SelectableInBound]() extends StageContainer[(Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean), V, (Option[String], Int)] {
  // Has a `scope` bound
  val localKeyword: HashMap[String, ArrayBuffer[V]] = HashMap()
  val local: ArrayBuffer[V] = ArrayBuffer()

  override protected def innerReset(): Unit = {
    localKeyword.clear()
    local.clear()
  }
  override protected def innerAdd(v: V): (Option[String], Int) = {
    v.bound match {
      case k: SelectionBoundKeywordTrait => {
        val array = localKeyword.getOrElseUpdate(k.keyword, ArrayBuffer())
        array += v
        (Some(k.keyword), array.length - 1)
      }
      case _ => {
        local += v
        (None, local.length - 1)
      }
    }
  }
  def filter(position: Option[lsp4j.Position], strictPosition: Boolean)(v: V): Boolean = {
    v.bound match {
      case s: SelectionBoundScopeTrait =>
        val range = Common.toRange(s.scope)
        if (strictPosition) range.getEnd().setCharacter(range.getEnd().getCharacter - 1)
        position.isDefined && Common.containsPosition(range, position.get) == 0
      case _ => true
    }
  }
  override def get(k: (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean)): Seq[V] = {
    val (position, keyword, strictPosition) = k
    val keywordMatches = keyword.flatMap(k => localKeyword.get(k._1)).iterator.flatten
    val all = local.iterator ++ keywordMatches
    all.filter(filter(position, strictPosition)).toSeq
  }
  override protected def innerUpdate(i: (Option[String], Int), f: V => V): Boolean = {
    val a = i._1.map(localKeyword).getOrElse(local)
    val oldV = a(i._2)
    val newV = f(oldV)
    assert(newV.bound == oldV.bound, "Cannot update to new element with a different bound")
    a(i._2) = newV
    true
  }
  override def all(): Iterator[V] =
    local.iterator ++ localKeyword.values.flatten
  override def filterInPlace(p: V => Boolean): Unit = {
    local.filterInPlace(p)
    localKeyword.values.foreach(_.filterInPlace(p))
    localKeyword.filterInPlace((_, g) => g.nonEmpty)
  }
}
object StageRangeContainer {
  type RangeContainer[V <: HasRangePositions with SelectableInBound, U] = LspContainer[StageRangeContainer[V], (Option[lsp4j.Position], Option[(String, lsp4j.Range)], Boolean), V, (Option[String], Int), U]
  implicit def default[V <: SelectableInBound]: () => StageRangeContainer[V] = () => StageRangeContainer()
}

case class StageSuggestableContainer[V <: lsp.SuggestableInBound]() extends StageContainer[(lsp.SuggestionScope, Option[lsp4j.Position], Char), V, Map[Option[lsp.SuggestionScope], Int]] {
  val perScope: HashMap[lsp.SuggestionScope, ArrayBuffer[V]] = HashMap()
  val onCharMatch: ArrayBuffer[V] = ArrayBuffer()

  override protected def innerReset(): Unit = {
    perScope.clear()
    onCharMatch.clear()
  }
  override protected def innerAdd(v: V): Map[Option[lsp.SuggestionScope], Int] = {
    val ocm = v.suggestionBound.starting.map(_ => {
      onCharMatch += v
      None -> (onCharMatch.length - 1)
    }).toSeq
    val others = v.suggestionBound.scope.map(s => {
      val array = perScope.getOrElseUpdate(s._1, ArrayBuffer())
      array += v
      Some(s._1) -> (array.length - 1)
    })
    (ocm ++ others).toMap
  }
  def filter(position: Option[lsp4j.Position], char: Char)(v: V): Boolean = {
    v.suggestionBound.starting.map(_.contains(char)).getOrElse(true) && (v.bound match {
      case s: SelectionBoundScopeTrait =>
        position.isDefined && Common.containsPosition(Common.toRange(s.scope), position.get) == 0
      case _ => true
    })
  }
  override def get(k: (lsp.SuggestionScope, Option[lsp4j.Position], Char)): Seq[V] = {
    val (scope, position, char) = k
    val ocms = onCharMatch.filter(ocm => {
      !ocm.suggestionBound.scope.contains(scope) &&
      ocm.suggestionBound.starting.get.contains(char)
    })
    val all = ocms.toSeq ++ perScope.get(scope).toSeq.flatMap(_.filter(filter(position, char)))
    all.filter(_.suggestionBound.range.zip(position).forall(r => {
      val range = Common.toRange(r._1)
      range.getEnd().setCharacter(range.getEnd().getCharacter - 1)
      Common.containsPosition(range, r._2) == 0
    }))
  }
  override protected def innerUpdate(i: Map[Option[lsp.SuggestionScope], Int], f: V => V): Boolean = {
    def index(i: Option[lsp.SuggestionScope]): ArrayBuffer[V] = i.map(perScope).getOrElse(onCharMatch)
    i.headOption.foreach(h => {
      val oldV = index(h._1)(h._2)
      val newV = f(oldV)
      assert(newV.suggestionBound == oldV.suggestionBound, "Cannot update to new element with a different bound")
      i.foreach(h => index(h._1)(h._2) = newV)
    })
    true
  }
  override def all(): Iterator[V] =
    perScope.values.flatten.iterator
  override def filterInPlace(p: V => Boolean): Unit = {
    perScope.values.foreach(_.filterInPlace(p))
    perScope.filterInPlace((_, g) => g.nonEmpty)
  }
}
object StageSuggestableContainer {
  type SuggestableContainer[V <: HasRangePositions with lsp.SuggestableInBound, U] = LspContainer[StageSuggestableContainer[V], (lsp.SuggestionScope, Option[lsp4j.Position], Char), V, Map[Option[lsp.SuggestionScope], Int], U]
  implicit def default[V <: lsp.SuggestableInBound]: () => StageSuggestableContainer[V] = () => StageSuggestableContainer()
}

case class LspContainer[+C <: StageContainer[K, V, I], K, V <: HasRangePositions, I, U]
  (translator: Translates[V, U, K], onUpdate: () => Unit = () => {}, expandOnBorder: Boolean = false)(implicit initial: () => C) {
  private val parseTypeck: C = initial()
  private val verify: C = initial()
  private def toPhase(first: Boolean): C = if (first) parseTypeck else verify

  def resetAll() = {
    parseTypeck.reset()
    verify.reset()
    onUpdate()
  }
  def reset(first: Boolean) = {
    toPhase(first).reset()
    onUpdate()
  }

  def receive(first: Boolean, v: V): (Int, I) = {
    toPhase(first).add(v)
  }
  def update(first: Boolean, i: (Int, I), f: V => V): Boolean = {
    toPhase(first).update(i, f)
  }

  def get(k: K)(implicit log: Logger): Seq[U] = translator.translate(Seq(parseTypeck.get(k), verify.get(k)).flatten)(k)
  def all(f: V => Boolean): Seq[V] = Seq(parseTypeck.all(), verify.all()).flatten.filter(f)

  /** If a range position is aliased, we do not want to update it twice */
  def resetModifiedFlag(): Unit = {
    parseTypeck.all().foreach(_.rangePositions.foreach(_.modified = false))
    verify.all().foreach(_.rangePositions.foreach(_.modified = false))
  }
  /**
    * Update the positions of semantic tokens after a change in the file
    *
    * @param range Replaced (deleted) range
    * @param text Newly inserted (added) text
    */
  def updatePositions(range: lsp4j.Range, text: String): Unit = {
    val lines = text.split("\n", -1)
    val newEndLine = range.getStart.getLine + lines.length - 1
    val deltaLines = newEndLine - range.getEnd.getLine
    val startCharacter = if (lines.length == 1) range.getStart.getCharacter else 0
    val newEndCharacter = startCharacter + lines.last.length
    val deltaChars = newEndCharacter - range.getEnd.getCharacter
    val endLine = range.getEnd.getLine

    // Remove overlapping semantic tokens and update positions of those after change
    def shiftEnd(pos: RangePosition, end: lsp4j.Position): Unit = {
      if (end.getLine == endLine) pos.shiftEnd(deltaLines, deltaChars)
      else pos.shiftEnd(deltaLines, 0)
    }
    def shiftStart(pos: RangePosition, start: lsp4j.Position): Unit = {
      if (start.getLine == endLine) pos.shiftStart(deltaLines, deltaChars)
      else pos.shiftStart(deltaLines, 0)
    }

    for (buffer <- Seq(parseTypeck, verify)) buffer.filterInPlace(v => {
      val positions = v.rangePositions
      var overlapped = false
      for (pos <- positions if !pos.modified && !overlapped) {
        pos.modified = true
        val pRange = Common.toRange(pos)
        val cmp = Common.compareRange(pRange, range)
        cmp match {
          case -4 =>
          case -3 | -2 =>
            if (expandOnBorder && deltaLines == 0)
              pos.shiftEnd(newEndLine - pRange.getEnd.getLine, newEndCharacter - pRange.getEnd.getCharacter)
            else
              pos.shiftEnd(range.getStart.getLine - pRange.getEnd.getLine, range.getStart.getCharacter - pRange.getEnd.getCharacter)
          case -1 => overlapped = true
          case 0 => overlapped = true
          case 1 => shiftEnd(pos, pRange.getEnd())
          case 2 | 3 =>
            if (expandOnBorder)
              pos.shiftStart(range.getStart.getLine - pRange.getStart.getLine, range.getStart.getCharacter - pRange.getStart.getCharacter)
            else
              pos.shiftStart(newEndLine - pRange.getStart.getLine, newEndCharacter - pRange.getStart.getCharacter)
            shiftEnd(pos, pRange.getEnd())
          case 4 =>
            shiftStart(pos, pRange.getStart())
            shiftEnd(pos, pRange.getEnd())
          case _ =>
        }
      }
      !overlapped
    })
  }
}
