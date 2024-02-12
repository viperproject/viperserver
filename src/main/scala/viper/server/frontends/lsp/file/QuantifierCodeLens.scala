// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import scala.collection.mutable.HashMap

import viper.silver.ast.utility.lsp._
import viper.silver.ast.LineColumnPosition
import java.nio.file.Paths
import akka.actor.ActorSystem
import scala.collection.mutable.HashSet
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext

trait QuantifierCodeLens extends ProjectAware {
  implicit val ec: ExecutionContext

  // Quantifier chosen triggers
  private val quantifierMap: HashMap[RangePosition, ((Int, Int), Int, Int, Int)] = HashMap()

  def quantToPos(quantifier: String): Option[RangePosition] = {
    val filePos = quantifier.stripPrefix("prog.").split('@')
    if (filePos.length != 5) return None
    (filePos(1).toIntOption, filePos(2).toIntOption, filePos(3).toIntOption, filePos(4).toIntOption) match {
      case (Some(startLine), Some(startCol), Some(endLine), Some(endCol)) => {
        val path = Paths.get(filePos(0))
        val pos = RangePosition(path, LineColumnPosition(startLine, startCol), LineColumnPosition(endLine, endCol))
        Some(pos)
      }
      case _ => None
    }
  }

  def handleQuantifierInstantiations(quantifier: String, instantiations: Int, maxGen: Int, maxCost: Int): Unit = {
    quantToPos(quantifier) match {
      case None => coordinator.logger.error(s"Got QIs, but unknown quantifier: ${quantifier}")
      case Some(pos) => {
        val uri = pos.file.toUri().toString()
        setQIsInFile(getInProject(uri), pos, instantiations, maxGen, maxCost)
      }
    }
  }
  private object Locker
  def setQIsInFile(m: FullManager, pos: RangePosition, instantiations: Int, maxGen: Int, maxCost: Int) = {
    def newCl() = CodeLens(pos, s"Instantiations $instantiations, Max generation $maxGen, Max cost $maxCost")
    Locker.synchronized {
      val idx = quantifierMap.get(pos) match {
        case None => Some(m.codeLensContainer.receive(false, newCl()))
        case Some((_, oldInsts, oldMaxGen, oldMaxCost)) if instantiations == oldInsts && maxGen == oldMaxGen && maxCost == oldMaxCost =>
          None
        case Some((idx, _, _, _)) => {
          val updated = m.codeLensContainer.update(false, idx, _ => newCl())
          if (updated) Some(idx)
          else {
            Some(m.codeLensContainer.receive(false, newCl()))
          }
        }
      }
      idx.foreach(idx => {
        quantifierMap.update(pos, (idx, instantiations, maxGen, maxCost))
        scheduleRefresh(pos.file.toUri().toString())
      })
    }
  }

  private val system = ActorSystem("codeLensRefresher")
  private def doRefresh(): Unit = {
    Locker.synchronized {
      changed.foreach(uri => getInProject(uri).codeLensContainer.onUpdate())
      changed.clear()
      lastRefresh = System.currentTimeMillis()
    }
  }
  private val changed: HashSet[String] = HashSet()
  private var lastRefresh: Long = 0
  private val refreshRate: Long = 1000
  private def scheduleRefresh(uri: String) = {
    val wasEmpty = changed.isEmpty
    changed.addOne(uri)
    if (wasEmpty) {
      val delta = System.currentTimeMillis() - lastRefresh
      if (delta > refreshRate) {
        doRefresh()
      } else {
        system.scheduler.scheduleOnce(Duration(refreshRate - delta, TimeUnit.MILLISECONDS))(doRefresh())
      }
    }
  }
}
