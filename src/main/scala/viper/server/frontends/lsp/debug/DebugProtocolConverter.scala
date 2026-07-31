// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug

import viper.server.frontends.lsp
import viper.silicon.{debugger => sil}

/**
  * Converts Silicon's debug model into the wire format of the LSP debugger protocol: `Seq` becomes `Array`,
  * `Option` becomes `null` or a sentinel value, and Viper's 1-based positions become LSP's 0-based ones.
  */
object DebugProtocolConverter {

  def toPosition(p: sil.DebugSourcePos): lsp.DebugPosition =
    lsp.DebugPosition(p.file, math.max(p.startLine - 1, 0), math.max(p.startColumn - 1, 0),
      math.max(p.endLine - 1, 0), math.max(p.endColumn - 1, 0))

  def toPosition(p: Option[sil.DebugSourcePos]): lsp.DebugPosition = p.map(toPosition).orNull

  def toNode(n: sil.DebugNode): lsp.DebugNode =
    lsp.DebugNode(n.id, n.kind, n.label, n.description.orNull, n.expString.orNull, n.termString.orNull,
      n.isInternal, n.childCount, toPosition(n.pos), n.children.map(toNode).toArray)

  def toNodes(ns: Seq[sil.DebugNode]): Array[lsp.DebugNode] = ns.map(toNode).toArray

  def toHeap(h: sil.DebugHeapModel): lsp.DebugHeap =
    lsp.DebugHeap(h.title, h.key, toNodes(h.chunks))

  def toPrintConfig(c: sil.PrintConfigModel): lsp.DebugPrintConfig =
    lsp.DebugPrintConfig(c.printInternal, c.nChildrenToShow, c.hierarchyLevel, c.printAxioms,
      c.printInternalTermRepresentation, c.printOldHeaps)

  def fromPrintConfig(c: lsp.DebugPrintConfig): sil.PrintConfigModel =
    sil.PrintConfigModel(c.printInternal, c.nChildrenToShow, c.hierarchyLevel, c.printAxioms,
      c.printInternalTermRepresentation, c.printOldHeaps)

  def toFailure(f: sil.DebugFailureInfo): lsp.DebugFailure =
    lsp.DebugFailure(f.index, f.message, toPosition(f.pos), f.memberName.orNull, f.debuggable)

  def toMessage(m: sil.DebugMessage): lsp.DebugMessage = lsp.DebugMessage(m.severity, m.text)

  def toMessages(ms: Seq[sil.DebugMessage]): Array[lsp.DebugMessage] = ms.map(toMessage).toArray

  def toSection(s: sil.DebugSectionModel): lsp.DebugSection =
    lsp.DebugSection(s.title, s.key, toNodes(s.nodes))

  def toCounterexample(c: sil.CounterexampleModel): lsp.DebugCounterexample =
    lsp.DebugCounterexample(c.sections.map(toSection).toArray, c.renderedText, c.stale)

  def toObligation(sessionId: String, o: sil.ObligationModel): lsp.DebugObligation =
    lsp.DebugObligation(
      sessionId = sessionId,
      failureIndex = o.failureIndex,
      errorMessage = o.originalError.message,
      errorPos = toPosition(o.originalError.pos),
      memberName = o.originalError.memberName.orNull,
      branchConditions = toNodes(o.branchConditions),
      store = toNodes(o.store),
      heaps = o.heaps.map(toHeap).toArray,
      axioms = toNodes(o.axioms),
      declarations = toNodes(o.declarations),
      assumptions = toNodes(o.assumptions),
      assertion = toNode(o.assertion),
      proverName = o.proverName,
      proverArgs = o.proverArgs.orNull,
      timeoutMs = o.timeoutMs.getOrElse(-1),
      printConfig = toPrintConfig(o.printConfig),
      renderedText = sil.DebugRenderer.renderObligation(o),
      counterexample = o.counterexample.map(toCounterexample).orNull)

  def toResponse(sessionId: String, r: sil.DebugCommandResult): lsp.DebugCommandResponse =
    lsp.DebugCommandResponse(sessionId, r.ok, r.obligation.map(toObligation(sessionId, _)).orNull,
      toMessages(r.messages), r.proved.map(java.lang.Boolean.valueOf(_)).orNull)

  def errorResponse(sessionId: String, error: String): lsp.DebugCommandResponse =
    lsp.DebugCommandResponse(sessionId, ok = false, null, Array.empty, null, error)
}
