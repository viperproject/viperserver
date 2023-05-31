// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

import viper.server.frontends.lsp
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{PublishDiagnosticsParams}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import viper.silver.verifier.AbstractError
import viper.silver.ast.HasLineColumn
import org.eclipse.lsp4j.Range
import viper.silver.ast.AbstractSourcePosition
import org.eclipse.lsp4j.DiagnosticSeverity
import viper.server.frontends.lsp.file.utility.{StageArrayContainer, DiagnosticTranslator, LspContainer}
import viper.silver.ast.utility.lsp.HasRangePositions
import viper.silver.ast.utility.lsp.BelongsToFile
import viper.silver.ast.utility.lsp.RangePosition
import java.nio.file.Path
import viper.silver.ast.LineColumnPosition
import scala.collection.mutable.HashSet

case class Diagnostic(backendClassName: Option[String], position: RangePosition, message: String, cached: Boolean, errorMsgPrefix: Option[String]) extends HasRangePositions with BelongsToFile {
  override def rangePositions: Seq[RangePosition] = Seq(position)
  override def file: Path = position.file
}

trait DiagnosticManager[+A <: DiagnosticManager[A]] extends CommonFileInfo[A] { this: A =>
  type DiagnosticContainer = StageArrayContainer.ArrayContainer[Diagnostic, lsp4j.Diagnostic]
  val diagnostic: DiagnosticContainer = LspContainer(DiagnosticTranslator)

  containers.addOne(diagnostic)

  // private val verDiags: ArrayBuffer[lsp4j.Diagnostic] = ArrayBuffer()
  // private val astDiags: ArrayBuffer[lsp4j.Diagnostic] = ArrayBuffer()

  def publishDiags(): Unit = {
    val diags = diagnostic.get(file_uri, ())(coordinator.logger)
    coordinator.logger.debug(s"Publishing ${diags.size} diagnostics")
    val diagnosticParams = new PublishDiagnosticsParams(file_uri, diags.asJava)
    coordinator.client.publishDiagnostics(diagnosticParams)
  }

  // def diags: Seq[lsp4j.Diagnostic] = verDiags.toSeq ++ astDiags.toSeq

  // def sendDiags(ver: Boolean, newDiags: Seq[lsp4j.Diagnostic]) = {
  //   if (ver) {
  //     if (canClearVerErrors) {
  //         canClearVerErrors = false
  //         verDiags.clear()
  //     }
  //     verDiags.addAll(newDiags)
  //   } else {
  //     if (canClearAstErrors) {
  //         canClearAstErrors = false
  //         astDiags.clear()
  //     }
  //     astDiags.addAll(newDiags)
  //   }
  //   val diagnosticParams = new PublishDiagnosticsParams(file_uri, diags.asJava)
  //   coordinator.client.publishDiagnostics(diagnosticParams)
  // }

  def handleFinish() = {
    // if (canClearVerErrors && isVerifying) {
    //   canClearVerErrors = false
    //   diagnostic.resetVerify()
    // }
    // if (canClearAstErrors) {
    //   canClearAstErrors = false
    //   diagnostic.resetParse()
    // }
    // val diagnosticParams = new PublishDiagnosticsParams(file_uri, diagnostic.get(coordinator.logger).asJava)
    // coordinator.client.publishDiagnostics(diagnosticParams)
  }
}
