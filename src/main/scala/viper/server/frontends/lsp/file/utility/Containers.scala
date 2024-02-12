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
import viper.silver.ast.utility.lsp.SelectionBoundScope
import viper.silver.ast.utility.lsp.SelectionBoundBoth
import viper.silver.ast.utility.lsp.SelectionBoundKeyword
import viper.silver.ast.utility.lsp.SelectionBound
import viper.silver.ast.utility.lsp.SelectionBoundKeywordTrait
import viper.server.frontends.lsp.file.VerificationPhase._

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
// trait StageProjectContainer[K, V] extends StageContainer[K, V] {
//   def get(uri: String, k: K): Seq[V]
// }

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
// case class StageArrayContainer[V]() extends StageArrayContainerTrait[V]
object StageArrayContainer {
  type ArrayContainer[V <: HasRangePositions with BelongsToFile, U] = LspContainer[StageArrayContainer[V], Unit, V, Int, U]
  implicit def default[V]: () => StageArrayContainer[V] = () => StageArrayContainer()
}

// case class StageArrayProjectContainer[V <: BelongsToFile](uriToStage: String => StageArrayContainer[V])(implicit root: String) extends StageArrayContainerTrait[V] {
//   override def add(v: V): Unit = {
//     val uri = v.file.toUri().toString()
//     if (uri == root) super.add(v) else uriToStage(uri).add(v)
//   }
//   override def get(uri: String, k: Unit): Seq[V] = if (uri == root) super.get(uri, k) else uriToStage(uri).get(uri, k)
// }
// object StageArrayProjectContainer {
//   type ArrayProjectContainer[V <: HasRangePositions with BelongsToFile, U, T <: Translates[V, U, Unit]] = LspProjectContainer[StageArrayProjectContainer[V], Unit, V, U, T]

//   def default[V <: BelongsToFile](uriToStage: String => StageArrayContainer[V])(implicit root: String): StageArrayProjectContainer[V] = StageArrayProjectContainer[V](uriToStage)
// }

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
// case class StageRangeContainer[V <: SelectableInBound[_, SelectionBoundScopeTrait]]() extends StageRangeContainerTrait[V]
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
// case class StageRangeContainer[V <: SelectableInBound[_, SelectionBoundScopeTrait]]() extends StageRangeContainerTrait[V]
object StageSuggestableContainer {
  type SuggestableContainer[V <: HasRangePositions with lsp.SuggestableInBound, U] = LspContainer[StageSuggestableContainer[V], (lsp.SuggestionScope, Option[lsp4j.Position], Char), V, Map[Option[lsp.SuggestionScope], Int], U]
  implicit def default[V <: lsp.SuggestableInBound]: () => StageSuggestableContainer[V] = () => StageSuggestableContainer()
}

// case class StageRangeProjectContainer[V <: SelectableInBound[V, SelectionBoundTrait]](uriToStage: String => StageRangeContainer[SelectableInBound[V, SelectionBoundScopeTrait]])(implicit root: String) extends StageRangeContainerTrait[SelectableInBound[V, SelectionBoundScopeTrait]] {
//   // No `scope` bound
//   val globalKeyword: HashMap[String, ArrayBuffer[V]] = HashMap()
//   val global: ArrayBuffer[V] = ArrayBuffer()

//   override def reset(): Unit = {
//     super.reset()
//     globalKeyword.clear()
//     global.clear()
//   }
//   override def add(v: V): Unit = v.selectableInScope match {
//     case Some(u) => {
//       val uri = u.bound.scope.file.toUri().toString()
//       if (uri == root) super.add(u) else uriToStage(uri).add(u)
//     }
//     case None => v.bound match {
//     case bound: SelectionBoundKeywordTrait =>
//       globalKeyword.getOrElseUpdate(bound.keyword, ArrayBuffer()).addOne(v)
//     case _ => global.addOne(v)
//     }
//   }
//   override def get(k: (Option[lsp4j.Position], Option[(String, lsp4j.Range)])): Seq[V] = {
//     val local = if (uri == root) super.get(uri, k) else uriToStage(uri).get(uri, k)
//     global.toSeq ++ globalKeyword.get(k._1).toSeq.flatten ++ local.map(_.get)
//   }
//   override def all(): Iterator[V] =
//     super.all().map(_.get) ++ global.iterator ++ globalKeyword.values.flatten
//   override def filterInPlace(p: V => Boolean): Unit = {
//     super.filterInPlace(v => p(v.get))
//     global.filterInPlace(p)
//     globalKeyword.values.foreach(_.filterInPlace(p))
//     globalKeyword.filterInPlace((_, g) => g.nonEmpty)
//   }
// }
// object StageRangeProjectContainer {
//   type RangeProjectContainer[V <: HasRangePositions with SelectableInBound[V, SelectionBoundTrait], U] = LspContainer[StageRangeProjectContainer[V], (Option[lsp4j.Position], Option[(String, lsp4j.Range)]), V, U]

//   def default[V <: SelectableInBound[V, SelectionBoundTrait]](uriToStage: String => StageRangeContainer[SelectableInBound[V, SelectionBoundScopeTrait]])(implicit root: String): StageRangeProjectContainer[V] = StageRangeProjectContainer[V](uriToStage)
// }

// case class RangeSelector[T <: SelectableInBound](
//     localKeyword: HashMap[String, ArrayBuffer[T]] = HashMap[String, ArrayBuffer[T]](),
//     localPosition: ArrayBuffer[T] = ArrayBuffer[T](),
//     globalKeyword: HashMap[String, ArrayBuffer[T]] = HashMap[String, ArrayBuffer[T]](),
// ) {
//   def getGlobal(ident: String): Seq[T] = {
//     globalKeyword.get(ident).toSeq.flatMap(_.toSeq)
//   }
//   def getLocalForPos(ident: String, pos: lsp4j.Position): Seq[T] = {
//     // println("Checking for local pos: " + pos.toString() + " in " + localPosition.toString())
//     localKeyword.get(ident).toSeq.flatMap(_.filter(inLocalPos(pos)).toSeq) ++
//     localPosition.filter(inLocalPos(pos)).toSeq
//   }
//   def inLocalPos(pos: lsp4j.Position)(h: T): Boolean = {
//       val range = Common.toRange(h.bound.scope.get)
//       Common.comparePosition(range.getStart(), pos) <= 0 &&
//       Common.comparePosition(range.getEnd(), pos) >= 0
//   }
//   // def setLocal
// }

case class LspContainer[+C <: StageContainer[K, V, I], K, V <: HasRangePositions, I, U]
  (translator: Translates[V, U, K], onUpdate: () => Unit = () => {}, expandOnBorder: Boolean = false)(implicit initial: () => C) {
  private val parse: C = initial()
  private val typeck: C = initial()
  private val verify: C = initial()
  private def toPhase(phase: VerificationPhase): C = phase match {
    case ParseStart => ???
    case ParseEnd => parse
    case TypeckEnd => typeck
    case VerifyEnd => verify
  }

  def resetAll() = {
    parse.reset()
    typeck.reset()
    verify.reset()
    onUpdate()
  }
  def reset(phase: VerificationPhase) = {
    toPhase(phase).reset()
    onUpdate()
  }

  def receive(phase: VerificationPhase, v: V): (Int, I) = {
    toPhase(phase).add(v)
  }
  def update(phase: VerificationPhase, i: (Int, I), f: V => V): Boolean = {
    toPhase(phase).update(i, f)
  }

  def get(k: K)(implicit log: Logger): Seq[U] = translator.translate(Seq(parse.get(k), typeck.get(k), verify.get(k)).flatten)(k)
  def all(f: V => Boolean): Seq[V] = Seq(parse.all(), typeck.all(), verify.all()).flatten.filter(f)

  /** If a range position is aliased, we do not want to update it twice */
  def resetModifiedFlag(): Unit = {
    parse.all().foreach(_.rangePositions.foreach(_.modified = false))
    typeck.all().foreach(_.rangePositions.foreach(_.modified = false))
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

    for (buffer <- Seq(parse, typeck, verify)) buffer.filterInPlace(v => {
      // println("Updating pos for " + v.toString)
      val positions = v.rangePositions
      var overlapped = false
      for (pos <- positions if !pos.modified && !overlapped) {
        pos.modified = true
        val pRange = Common.toRange(pos)
        val cmp = Common.compareRange(pRange, range)
        // println("PRange: " + pRange.toString() + " Range: " + range.toString())
        // println("Cmp: " + cmp)
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
      // if (overlapped) println("overlapped: " + v.toString())
      !overlapped
    })
  }
}

// case class LspProjectContainer[C <: StageProjectContainer[K, V], K, V <: HasRangePositions, U, T <: Translates[V, U, K]]
//   (containerInFile: String => LspContainer[C, K, V], onUpdate: () => Unit = () => {})
//   (implicit initial: (String => StageContainer[K, V]) => C, implicit val translator: T) extends LspContainerTrait[C, K, V] {
//   override val parse: C = initial(containerInFile(_).parse)
//   override val typeck: C = initial(containerInFile(_).typeck)
//   override val verify: C = initial(containerInFile(_).verify)

//   def resetAll() = {
//     resetParse(false)
//     resetTypeck(false)
//     resetVerify(false)
//     onUpdate()
//   }
//   def resetParse(doUpdate: Boolean = true) = {
//     parse.reset()
//     if (doUpdate) onUpdate()
//   }
//   def resetTypeck(doUpdate: Boolean = true) = {
//     typeck.reset()
//     if (doUpdate) onUpdate()
//   }
//   def resetVerify(doUpdate: Boolean = true) = {
//     verify.reset()
//     if (doUpdate) onUpdate()
//   }

//   // private def addToFile[N <: V with BelongsToFile](v: N, uriToStage: String => StageContainer[K, V]) = {
//   //   val uri = v.file.toUri().toString()
//   //   uriToStage(uri).add(v)
//   // }

//   def receiveParse(ps: Seq[V], doUpdate: Boolean = true): Unit = {
//     ps.foreach(receiveParse(_, false))
//     if (doUpdate) onUpdate()
//   }
//   def receiveTypeck(ps: Seq[V], doUpdate: Boolean = true): Unit = {
//     ps.foreach(receiveTypeck(_, false))
//     if (doUpdate) onUpdate()
//   }
//   def receiveVerify(ps: Seq[V], doUpdate: Boolean = true): Unit = {
//     ps.foreach(receiveVerify(_, false))
//     if (doUpdate) onUpdate()
//   }

//   def receiveParse(p: V, doUpdate: Boolean): Unit = {
//     this.parse.add(p)
//     if (doUpdate) onUpdate()
//   }
//   def receiveTypeck(t: V, doUpdate: Boolean): Unit = {
//     this.typeck.add(t)
//     if (doUpdate) onUpdate()
//   }
//   def receiveVerify(v: V, doUpdate: Boolean): Unit = {
//     this.verify.add(v)
//     if (doUpdate) onUpdate()
//   }

//   def get(uri: String, k: K)(implicit log: Logger): Seq[U] = translator.translate(Seq(parse.get(uri, k), typeck.get(uri, k), verify.get(uri, k)).flatten)(k)

//   /** If a range position is aliased, we do not want to update it twice */
//   def resetModifiedFlag(): Unit = {
//     parse.all().foreach(_.rangePositions.foreach(_.modified = false))
//     typeck.all().foreach(_.rangePositions.foreach(_.modified = false))
//     verify.all().foreach(_.rangePositions.foreach(_.modified = false))
//   }
//   /**
//     * Update the positions of semantic tokens after a change in the file
//     *
//     * @param range Replaced (deleted) range
//     * @param text Newly inserted (added) text
//     */
//   def updatePositions(range: lsp4j.Range, text: String): Unit = {
//     val lines = text.split("\n", -1)
//     val deltaLines = lines.length - 1 + range.getStart.getLine - range.getEnd.getLine
//     val startCharacter = if (lines.length == 1) range.getStart.getCharacter else 0
//     val deltaChars = startCharacter + lines.last.length - range.getEnd.getCharacter
//     val endLine = range.getEnd.getLine

//     // TODO:
//     // If the change cannot ruin the meaning of a semantic token at the start,
//     // adjust the range start to avoid overlaps with adjacent tokens
//     // if (text.isEmpty || !text.head.isLetterOrDigit) {
//       // range.getStart.setCharacter(range.getStart.getCharacter + 1)
//     // }
//     // If the change cannot ruin the meaning of a semantic token at the end,
//     // adjust the range end to avoid overlaps with adjacent tokens
//     // if (text.isEmpty || !text.last.isLetterOrDigit) {
//       // range.getEnd.setCharacter(range.getEnd.getCharacter - 1)
//     // }
//     // Remove overlapping semantic tokens and update positions of those after change
//     def shiftEnd(pos: RangePosition, end: lsp4j.Position): Unit = {
//       if (end.getLine == endLine) pos.shiftEnd(deltaLines, deltaChars)
//       else pos.shiftEnd(deltaLines, 0)
//     }
//     def shiftStart(pos: RangePosition, start: lsp4j.Position): Unit = {
//       if (start.getLine == endLine) pos.shiftStart(deltaLines, deltaChars)
//       else pos.shiftStart(deltaLines, 0)
//     }

//     for (buffer <- Seq(parse, typeck, verify)) buffer.filterInPlace(v => {
//       val positions = v.rangePositions
//       var overlapped = false
//       for (pos <- positions if !pos.modified && !overlapped) {
//         pos.modified = true
//         val start = Common.toPosition(pos.start)
//         val end = Common.toPosition(pos._end)
//         Common.containsPos(range, start) match {
//           // `start` < `range.start`
//           case neg if neg < 0 => Common.containsPos(range, end) match {
//             // `end` < `range.start`
//             case neg if neg < 0 => {}
//             // `end` within `range`
//             case 0 => overlapped = true
//             // `end` > `range.end`
//             case _ => shiftEnd(pos, end)
//           }
//           // `start` within `range`
//           case 0 => overlapped = true
//           // `start` > `range.end`
//           case _ => {
//             shiftStart(pos, start)
//             shiftEnd(pos, end)
//           }
//         }
//       }
//       if (overlapped) println("overlapped: " + v.toString())
//       !overlapped
//     })
//   }
// }
