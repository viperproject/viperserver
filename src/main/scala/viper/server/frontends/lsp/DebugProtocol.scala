// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp

import org.eclipse.lsp4j.Position

/**
  * The payloads of the debugger's LSP messages.
  *
  * These mirror the model in `viper.silicon.debugger` but are shaped for LSP4J's Gson serializer, which knows
  * nothing about Scala: use `Array` rather than `Seq`, `null` rather than `Option`, and sentinel values rather
  * than optional numbers. There is a corresponding set of interfaces in the Viper IDE client, in
  * 'ViperProtocol.ts', which has to be kept in sync with this file.
  */

/** A source range in LSP coordinates, i.e. 0-based lines and characters. */
case class DebugPosition(file: String, startLine: Int, startCharacter: Int, endLine: Int, endCharacter: Int)

/**
  * A node of the proof obligation tree.
  *
  * @param id         the id of the assumption, which the user can refer to when removing or expanding it.
  *                   Negative for nodes that are not assumptions (store entries, heap chunks, ...).
  * @param kind       one of "assumption", "implication", "quantified", "store", "chunk", "branchCondition",
  *                   "axiom", "term", "declaration", "assertion", "literal".
  * @param childCount the number of children of this node; if it exceeds `children.length`, the remaining
  *                   children can be requested with the DebugExpandNode request.
  */
case class DebugNode(id: Int,
                     kind: String,
                     label: String,
                     description: String = null,
                     expString: String = null,
                     termString: String = null,
                     isInternal: Boolean = false,
                     childCount: Int = 0,
                     pos: DebugPosition = null,
                     children: Array[DebugNode] = Array.empty)

case class DebugHeap(title: String, key: String, chunks: Array[DebugNode])

/** A named group of nodes of a counterexample, e.g. the values on return or those at a labelled state. */
case class DebugSection(title: String, key: String, nodes: Array[DebugNode])

/**
  * A model of the assumptions in which the assertion does not hold, i.e. a concrete situation in which the
  * verification fails. It is recomputed whenever the obligation is proved, so it reflects assumptions the user
  * added or removed.
  *
  * @param stale true if the assumptions changed after this counterexample was computed.
  */
case class DebugCounterexample(sections: Array[DebugSection],
                               renderedText: String,
                               stale: Boolean)

case class DebugPrintConfig(printInternal: Boolean,
                            nChildrenToShow: Int,
                            hierarchyLevel: Int,
                            printAxioms: Boolean,
                            printInternalTermRepresentation: Boolean,
                            printOldHeaps: Boolean)

/** A verification failure that a debug session can be opened for. */
case class DebugFailure(index: Int,
                        message: String,
                        pos: DebugPosition,
                        memberName: String = null,
                        debuggable: Boolean = true)

/** The proof obligation of the failure that is currently being debugged. */
case class DebugObligation(sessionId: String,
                           failureIndex: Int,
                           errorMessage: String,
                           errorPos: DebugPosition,
                           memberName: String,
                           branchConditions: Array[DebugNode],
                           store: Array[DebugNode],
                           heaps: Array[DebugHeap],
                           axioms: Array[DebugNode],
                           declarations: Array[DebugNode],
                           assumptions: Array[DebugNode],
                           assertion: DebugNode,
                           proverName: String,
                           proverArgs: String,
                           timeoutMs: Int, /* -1 means no timeout */
                           printConfig: DebugPrintConfig,
                           renderedText: String,
                           /** null if no counterexample has been computed for this obligation. */
                           counterexample: DebugCounterexample = null)

/** A message produced by a debugger command; `severity` is "info", "warning" or "error". */
case class DebugMessage(severity: String, text: String)

// --- Requests -------------------------------------------------------------------------------------------

/**
  * @param position           the start of the diagnostic the user asked to debug, in LSP coordinates.
  * @param message            the message of that diagnostic, used to disambiguate failures at the same position.
  * @param restrictToMember   verify only the member containing `position`, which is much faster.
  */
case class DebugStartParams(uri: String,
                            backend: String,
                            customArgs: String,
                            position: Position,
                            message: String,
                            restrictToMember: Boolean,
                            /** Also compute counterexamples, which needs `--counterexample` and `--exhaleMode=1`. */
                            withCounterexample: Boolean = true)

case class DebugStartResponse(sessionId: String,
                              failures: Array[DebugFailure],
                              selected: Int,
                              obligation: DebugObligation,
                              messages: Array[DebugMessage],
                              error: String = null)

case class DebugSessionRef(sessionId: String)

case class DebugStopResponse(success: Boolean)

/** The response to every command that operates on an open session. */
case class DebugCommandResponse(sessionId: String,
                                ok: Boolean,
                                obligation: DebugObligation,
                                messages: Array[DebugMessage],
                                proved: java.lang.Boolean = null,
                                error: String = null)

case class DebugSelectFailureParams(sessionId: String, index: Int)
case class DebugExpandParams(sessionId: String, nodeId: Int)
case class DebugExpandResponse(nodes: Array[DebugNode], error: String = null)
case class DebugRemoveAssumptionsParams(sessionId: String, nodeIds: Array[Int])
case class DebugAddAssumptionParams(sessionId: String, expression: String, free: Boolean)
case class DebugPrintConfigParams(sessionId: String, config: DebugPrintConfig)
case class DebugSetProverParams(sessionId: String, prover: String, args: String)
case class DebugSetTimeoutParams(sessionId: String, timeoutMs: Int)

// --- Notifications --------------------------------------------------------------------------------------

/** Progress while a debug session is being started; `state` is "starting", "running" or "ready". */
case class DebugSessionStateParams(sessionId: String, state: String, message: String)

case class DebugSessionClosedParams(sessionId: String, reason: String)
