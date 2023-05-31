// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import org.eclipse.lsp4j
import scala.collection.mutable.{ArrayBuffer, HashMap}

import scala.jdk.CollectionConverters._
import viper.silver.reporter.Import
import viper.silver.ast.utility.lsp._
import viper.server.frontends.lsp.Common
// import viper.server.frontends.lsp.file.RangeSelector
import viper.silver.ast.utility.lsp.{HoverHint, SelectionBoundTrait}
import lsp4j.jsonrpc.messages.Either
import viper.silver.ast.QuantifiedExp
import viper.silver.ast.Trigger
import viper.silver.ast.AbstractSourcePosition
import viper.silver.ast.LineColumnPosition
import viper.silver.ast.SourcePosition
import java.net.URI
import java.nio.file.Paths
import akka.actor.ActorSystem
import akka.actor.setup.ActorSystemSetup
import akka.actor.Cancellable
import viper.server.frontends.lsp.file.utility.{CodeLensTranslator, StageArrayContainer, LspContainer}

trait CodeLensManager[A <: CodeLensManager[A]] extends CommonFileInfo[A] { this: A =>
  type CodeLensContainer = StageArrayContainer.ArrayContainer[CodeLens, lsp4j.CodeLens]
  val codeLens: CodeLensContainer = LspContainer(CodeLensTranslator, coordinator.client.refreshCodeLenses)

  containers.addOne(codeLens)

  // def handleCodeLens(all: Seq[CodeLens]) = {
  //   coordinator.logger.debug(s"handleCodeLens: ${all.toString()}")
  //   val perFile = all.groupBy(_.range.file.toUri().toString())
  //   perFile.get(file_uri).foreach(setCodeLens)
  //   for ((uri, inlayHints) <- perFile) {
  //     if (uri != file_uri) {
  //       // coordinator.setCodeLens(uri, inlayHints)
  //     }
  //   }
  // }
  // def setCodeLens(newCodeLens: Seq[CodeLens]): Unit = {
  //   codeLens.clear()
  //   codeLens.addAll(newCodeLens)
  // }
  // def getCodeLens(): Seq[lsp4j.CodeLens] = (codeLens.toSeq ++ verifCodeLens.toSeq).map(lens => {
  //   val range = Common.toRange(lens.range)
  //   // TODO: Is this necessary?
  //   // if (range.getEnd().getLine() != range.getStart().getLine()) {
  //   //   range.getEnd().setLine(range.getStart().getLine())
  //   //   range.getEnd().setCharacter(range.getStart().getCharacter() + 1)
  //   // }
  //   val command = new lsp4j.Command(lens.command, "")
  //   new lsp4j.CodeLens(range, command, null)
  // })

  // Quantifier chosen triggers

  // private val verifCodeLens: ArrayBuffer[CodeLens] = ArrayBuffer()
  private val quantifierMap: HashMap[RangePosition, (CodeLens, Int, Int, Int)] = HashMap()

  private val verifStart: () => Unit = () => {
    quantifierMap.clear()
  }
  val registerCLOnStartVerify: Unit = {
    onVerifyStart.addOne(verifStart)
  }

  def handleQuantifierInstantiations(quantifier: String, instantiations: Int, maxGen: Int, maxCost: Int): Unit = {
    val filePos = quantifier.stripPrefix("prog.").split('@')
    if (filePos.length != 5) {
      coordinator.logger.error(s"Got QIs, but unknown quantifier: ${quantifier}")
      return
    }
    (filePos(1).toIntOption, filePos(2).toIntOption, filePos(3).toIntOption, filePos(4).toIntOption) match {
      case (Some(startLine), Some(startCol), Some(endLine), Some(endCol)) => {
        val path = Paths.get(filePos(0))
        val uri = path.toUri().toString()
        val pos = RangePosition(path, LineColumnPosition(startLine, startCol), LineColumnPosition(endLine, endCol))
        if (uri == file_uri) {
          setQIsInFile(pos, instantiations, maxGen, maxCost)
        } else {
          coordinator.setQIsInFile(uri, pos, instantiations, maxGen, maxCost)
        }
      }
      case _ => coordinator.logger.error(s"Got QIs, but unknown quantifier: ${quantifier}")
    }
  }
  def setQIsInFile(pos: RangePosition, instantiations: Int, maxGen: Int, maxCost: Int) = {
    if (!quantifierMap.contains(pos)) {
      val cl = CodeLens(pos, s"Instantiations $instantiations, Max generation $maxGen, Max cost $maxCost")
      codeLens.receiveVerify(cl, true)
      quantifierMap.addOne(pos, (cl, instantiations, maxGen, maxCost))
    } else {
      val (cl, oldInsts, oldMaxGen, oldMaxCost) = quantifierMap(pos)
      if (instantiations != oldInsts || maxGen != oldMaxGen || maxCost != oldMaxCost) {
        cl.command = s"Instantiations $instantiations, Max generation $maxGen, Max cost $maxCost"
        quantifierMap.update(pos, (cl, instantiations, maxGen, maxCost))
        scheduleRefresh()
      }
    }
  }

  private var codeLensRefresh: Option[Cancellable] = {
    onVerifyEnd.addOne((_) => {
      if (codeLensRefresh.isDefined) {
        codeLensRefresh.get.cancel()
        codeLensRefresh = None
      }
    })
    None
  }
  private val system = ActorSystem("codeLensRefresher")
  private var didChange = false
  private def doRefresh(): Unit = {
    if (didChange) {
      didChange = false
      coordinator.client.refreshCodeLenses
    }
  }
  private def scheduleRefresh() = {
    didChange = true
    if (codeLensRefresh.isEmpty) {
      import scala.concurrent.duration._
      val cancel = system.scheduler.scheduleAtFixedRate(0.milli, 500.milli)(() => doRefresh())
      codeLensRefresh = Some(cancel)
    }
  }
}
