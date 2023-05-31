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
import viper.silver.ast.utility.lsp.SelectionBoundTrait
import viper.silver.ast.utility.lsp.SelectionBoundScopeTrait
import viper.silver.ast.utility.lsp.SelectionBoundScope
import viper.silver.ast.utility.lsp.SelectionBoundBoth
import viper.silver.ast.utility.lsp.SelectionBoundKeyword
import viper.silver.ast.utility.lsp.SelectionBound
import viper.silver.ast.utility.lsp.SelectionBoundKeywordTrait

trait StageContainer[K, V] {
  def reset(): Unit
  def add(v: V): Unit
  def get(k: K): Seq[V]
  def all(): Iterator[V]
  def filterInPlace(p: V => Boolean): Unit
}
// trait StageProjectContainer[K, V] extends StageContainer[K, V] {
//   def get(uri: String, k: K): Seq[V]
// }

case class StageArrayContainer[V]() extends StageContainer[Unit, V] {
  private val array: ArrayBuffer[V] = ArrayBuffer()
  override def reset(): Unit = array.clear()
  override def add(v: V): Unit = array.addOne(v)
  override def get(k: Unit): Seq[V] = array.toSeq
  override def all(): Iterator[V] = array.iterator
  def filterInPlace(p: V => Boolean): Unit = array.filterInPlace(p)
}
// case class StageArrayContainer[V]() extends StageArrayContainerTrait[V]
object StageArrayContainer {
  type ArrayContainer[V <: HasRangePositions with BelongsToFile, U] = LspContainer[StageArrayContainer[V], Unit, V, U]
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

case class StageRangeContainer[V <: SelectableInBound]() extends StageContainer[(String, lsp4j.Position, lsp4j.Range), V] {
  // Has a `scope` bound
  val localKeyword: HashMap[String, ArrayBuffer[(Option[lsp4j.Range], V)]] = HashMap()
  val local: ArrayBuffer[(Option[lsp4j.Range], V)] = ArrayBuffer()

  override def reset(): Unit = {
    localKeyword.clear()
    local.clear()
  }
  override def add(v: V): Unit = {
    val range = v.bound match {
      case s: SelectionBoundScopeTrait => Some(Common.toRange(s.scope))
      case _ => None
    }
    v.bound match {
      case k: SelectionBoundKeywordTrait =>
        localKeyword.getOrElseUpdate(k.keyword, ArrayBuffer()).addOne((range, v))
      case _ => local.addOne((range, v))
    }
  }
  override def get(k: (String, lsp4j.Position, lsp4j.Range)): Seq[V] = {
    val (keyword, position, _) = k
    (local.iterator ++ localKeyword.get(keyword).iterator.flatten)
      .filter(h => h._1.isEmpty || Common.containsPos(h._1.get, position) == 0)
      .map(_._2).toSeq
  }
  override def all(): Iterator[V] =
    local.iterator.map(_._2) ++ localKeyword.values.flatten.map(_._2)
  override def filterInPlace(p: V => Boolean): Unit = {
    local.filterInPlace(l => p(l._2))
    localKeyword.values.foreach(_.filterInPlace(l => p(l._2)))
    localKeyword.filterInPlace((_, g) => g.nonEmpty)
  }
}
// case class StageRangeContainer[V <: SelectableInBound[_, SelectionBoundScopeTrait]]() extends StageRangeContainerTrait[V]
object StageRangeContainer {
  type RangeContainer[V <: HasRangePositions with SelectableInBound, U] = LspContainer[StageRangeContainer[V], (String, lsp4j.Position, lsp4j.Range), V, U]
  implicit def default[V <: SelectableInBound]: () => StageRangeContainer[V] = () => StageRangeContainer()
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
//   override def get(k: (String, lsp4j.Position, lsp4j.Range)): Seq[V] = {
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
//   type RangeProjectContainer[V <: HasRangePositions with SelectableInBound[V, SelectionBoundTrait], U] = LspContainer[StageRangeProjectContainer[V], (String, lsp4j.Position, lsp4j.Range), V, U]

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

case class LspContainer[+C <: StageContainer[K, V], K, V <: HasRangePositions, U]
  (translator: Translates[V, U, K], onUpdate: () => Unit = () => {})(implicit initial: () => C) {
  private val parse: C = initial()
  private val typeck: C = initial()
  private val verify: C = initial()

  def resetAll() = {
    resetParse(false)
    resetTypeck(false)
    resetVerify(false)
    onUpdate()
  }
  def resetParse(doUpdate: Boolean = true) = {
    parse.reset()
    if (doUpdate) onUpdate()
  }
  def resetTypeck(doUpdate: Boolean = true) = {
    typeck.reset()
    if (doUpdate) onUpdate()
  }
  def resetVerify(doUpdate: Boolean = true) = {
    verify.reset()
    if (doUpdate) onUpdate()
  }

  // private def addToFile[N <: V with BelongsToFile](v: N, uriToStage: String => StageContainer[K, V]) = {
  //   val uri = v.file.toUri().toString()
  //   uriToStage(uri).add(v)
  // }

  def receiveParse(ps: Seq[V], doUpdate: Boolean = true): Unit = {
    ps.foreach(receiveParse(_, false))
    if (doUpdate) onUpdate()
  }
  def receiveTypeck(ps: Seq[V], doUpdate: Boolean = true): Unit = {
    ps.foreach(receiveTypeck(_, false))
    if (doUpdate) onUpdate()
  }
  def receiveVerify(ps: Seq[V], doUpdate: Boolean = true): Unit = {
    ps.foreach(receiveVerify(_, false))
    if (doUpdate) onUpdate()
  }

  def receiveParse(p: V, doUpdate: Boolean): Unit = {
    this.parse.add(p)
    if (doUpdate) onUpdate()
  }
  def receiveTypeck(t: V, doUpdate: Boolean): Unit = {
    this.typeck.add(t)
    if (doUpdate) onUpdate()
  }
  def receiveVerify(v: V, doUpdate: Boolean): Unit = {
    this.verify.add(v)
    if (doUpdate) onUpdate()
  }

  def get(k: K)(implicit log: Logger): Seq[U] = translator.translate(Seq(parse.get(k), typeck.get(k), verify.get(k)).flatten)(k)

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
    val deltaLines = lines.length - 1 + range.getStart.getLine - range.getEnd.getLine
    val startCharacter = if (lines.length == 1) range.getStart.getCharacter else 0
    val deltaChars = startCharacter + lines.last.length - range.getEnd.getCharacter
    val endLine = range.getEnd.getLine

    // TODO:
    // If the change cannot ruin the meaning of a semantic token at the start,
    // adjust the range start to avoid overlaps with adjacent tokens
    // if (text.isEmpty || !text.head.isLetterOrDigit) {
      // range.getStart.setCharacter(range.getStart.getCharacter + 1)
    // }
    // If the change cannot ruin the meaning of a semantic token at the end,
    // adjust the range end to avoid overlaps with adjacent tokens
    // if (text.isEmpty || !text.last.isLetterOrDigit) {
      // range.getEnd.setCharacter(range.getEnd.getCharacter - 1)
    // }
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
      val positions = v.rangePositions
      var overlapped = false
      for (pos <- positions if !pos.modified && !overlapped) {
        pos.modified = true
        val start = Common.toPosition(pos.start)
        val end = Common.toPosition(pos._end)
        Common.containsPos(range, start) match {
          // `start` < `range.start`
          case neg if neg < 0 => Common.containsPos(range, end) match {
            // `end` < `range.start`
            case neg if neg < 0 => {}
            // `end` within `range`
            case 0 => overlapped = true
            // `end` > `range.end`
            case _ => shiftEnd(pos, end)
          }
          // `start` within `range`
          case 0 => overlapped = true
          // `start` > `range.end`
          case _ => {
            shiftStart(pos, start)
            shiftEnd(pos, end)
          }
        }
      }
      if (overlapped) println("overlapped: " + v.toString())
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
