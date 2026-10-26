// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug.dap

import org.eclipse.lsp4j.debug.{Scope, Source, Variable, VariablePresentationHint}
import viper.silicon.{debugger => sil}

import java.nio.file.Paths
import scala.collection.mutable

/**
  * A DAP variable with VS Code's extension `__vscodeVariableMenuContext`, which a `debug/variables/context` menu can
  * refer to as `debugProtocolVariableMenuContext`, to offer commands only for some variables.
  */
class MenuContextVariable(menuContext: String) extends Variable {
  // Serialized under this name by Gson; read by VS Code only.
  val __vscodeVariableMenuContext: String = menuContext
}

object MenuContextVariable {
  /** An assumption, which can be removed. */
  val Assumption = "assumption"
  /** A predicate chunk, which can be unfolded. */
  val Predicate = "predicate"
}

/** What a `variablesReference` handed out to the client stands for. */
sealed trait VariableContainer

object VariableContainer {
  /** A list of nodes, e.g. the contents of a scope. */
  case class Nodes(nodes: Seq[sil.DebugNode]) extends VariableContainer
  /** The children of a node; they may have to be requested from the session, see [[sil.DebugNode.hasHiddenChildren]]. */
  case class Children(node: sil.DebugNode) extends VariableContainer
  /** The sections of a counterexample. */
  case class Sections(sections: Seq[sil.DebugSectionModel]) extends VariableContainer
  /** Plain name/value pairs. */
  case class Entries(entries: Seq[(String, String)]) extends VariableContainer
}

/**
  * The `variablesReference`s of the current proof obligation. DAP clients only use references while the
  * debuggee is stopped and re-request everything after an `invalidated` or `stopped` event, so all references
  * are dropped whenever the obligation changes.
  */
class VariableHandles {
  private val containers = mutable.Map.empty[Int, VariableContainer]
  private var next = 1

  def create(c: VariableContainer): Int = synchronized {
    val ref = next
    next += 1
    containers(ref) = c
    ref
  }

  def get(ref: Int): Option[VariableContainer] = synchronized(containers.get(ref))

  def reset(): Unit = synchronized(containers.clear())
}

/**
  * Converts Silicon's debug model ([[sil.ObligationModel]], [[sil.DebugNode]], ...) into DAP scopes and variables.
  *
  * @param linesStartAt1   whether the client counts lines from 1, as Viper does.
  * @param columnsStartAt1 whether the client counts columns from 1, as Viper does.
  */
class DapModel(handles: VariableHandles, linesStartAt1: Boolean, columnsStartAt1: Boolean) {

  def line(viperLine: Int): Int = if (linesStartAt1) viperLine else viperLine - 1
  def column(viperColumn: Int): Int = if (columnsStartAt1) viperColumn else viperColumn - 1

  def source(pos: sil.DebugSourcePos): Option[Source] =
    Option(pos.file).filter(_.nonEmpty).map { file =>
      val src = new Source()
      src.setPath(file)
      src.setName(Option(Paths.get(file).getFileName).map(_.toString).getOrElse(file))
      src
    }

  /**
    * The scopes of a proof obligation. The obligation of a paused verification (`paused`) has neither an assertion
    * nor a counterexample.
    */
  def scopes(model: sil.ObligationModel, paused: Boolean = false): Seq[Scope] = {
    import VariableContainer._
    val heapScopes = model.heaps.map(h => scope(h.title, Nodes(h.chunks), h.chunks.size))
    val counterexample = model.counterexample.toSeq.map { ce =>
      scope(if (ce.stale) "Counterexample (outdated)" else "Counterexample", Sections(ce.sections), ce.sections.size)
    }
    val axioms =
      if (model.axioms.isEmpty) Seq.empty
      else Seq(scope("Axioms", Nodes(model.axioms), model.axioms.size, expensive = true))
    val declarations =
      if (model.declarations.isEmpty) Seq.empty
      else Seq(scope("Declarations", Nodes(model.declarations), model.declarations.size, expensive = true))

    // After a fold or unfold, the assertion still is the one evaluated before; :recheck checks the failed one again.
    val assertionTitle = if (model.assertionOutdated) "Assertion (before fold/unfold)" else "Assertion"
    val assertion = if (paused) Seq.empty else Seq(scope(assertionTitle, Nodes(Seq(model.assertion)), 1))
    assertion ++ Seq(
      scope("Assumptions", Nodes(model.assumptions), model.assumptions.size),
      scope("Store", Nodes(model.store), model.store.size, hint = Some("locals")),
    ) ++ heapScopes ++ Seq(
      scope("Branch conditions", Nodes(model.branchConditions), model.branchConditions.size),
    ) ++ counterexample ++ axioms ++ declarations ++ Seq(
      scope("Prover", Entries(proverEntries(model)), 3),
    )
  }

  private def proverEntries(model: sil.ObligationModel): Seq[(String, String)] = Seq(
    "solver" -> model.proverName,
    "arguments" -> model.proverArgs.getOrElse(""),
    "timeout" -> model.timeoutMs.map(ms => s"$ms ms").getOrElse("none"),
  )

  private def scope(name: String, container: VariableContainer, count: Int,
                    expensive: Boolean = false, hint: Option[String] = None): Scope = {
    val s = new Scope()
    s.setName(name)
    s.setVariablesReference(handles.create(container))
    s.setNamedVariables(count)
    s.setExpensive(expensive)
    hint.foreach(s.setPresentationHint)
    s
  }

  def variables(nodes: Seq[sil.DebugNode]): Seq[Variable] =
    nodes.zipWithIndex.map { case (n, idx) => variable(n, idx) }

  def sectionVariables(sections: Seq[sil.DebugSectionModel]): Seq[Variable] =
    sections.map { section =>
      val v = new Variable()
      v.setName(section.title)
      v.setValue(s"${section.nodes.size} entries")
      v.setVariablesReference(if (section.nodes.isEmpty) 0 else handles.create(VariableContainer.Nodes(section.nodes)))
      v
    }

  def entryVariables(entries: Seq[(String, String)]): Seq[Variable] =
    entries.map { case (name, value) =>
      val v = new Variable()
      v.setName(name)
      v.setValue(value)
      v
    }

  /** Kinds of nodes whose label has the form `name -> value`. */
  private val keyValueKinds = Set(sil.DebugNodeKind.StoreEntry, sil.DebugNodeKind.Value, sil.DebugNodeKind.Reference,
    sil.DebugNodeKind.Field, sil.DebugNodeKind.Predicate, sil.DebugNodeKind.Collection, sil.DebugNodeKind.FunctionCase)

  /** Kinds of nodes that correspond to a `DebugExp` and can therefore be referred to (and removed) by their id. */
  val assumptionKinds: Set[String] =
    Set(sil.DebugNodeKind.Assumption, sil.DebugNodeKind.Implication, sil.DebugNodeKind.Quantified)

  private def variable(n: sil.DebugNode, idx: Int): Variable = {
    val (name, value) =
      if (assumptionKinds.contains(n.kind) && n.id >= 0) {
        // The id is what the user types to remove the assumption (":remove 12"), so it is the name.
        (s"[${n.id}]", n.label)
      } else if (keyValueKinds.contains(n.kind) && n.label.contains(" -> ")) {
        val i = n.label.indexOf(" -> ")
        (n.label.substring(0, i), n.label.substring(i + 4))
      } else {
        (s"$idx", n.label)
      }

    val menuContext =
      if (assumptionKinds.contains(n.kind) && n.id >= 0) Some(MenuContextVariable.Assumption)
      else if (n.kind == sil.DebugNodeKind.Chunk && n.expString.isDefined) Some(MenuContextVariable.Predicate)
      else None
    val v = menuContext.map(new MenuContextVariable(_)).getOrElse(new Variable())
    v.setName(name)
    v.setValue(value)
    n.expString.foreach(v.setEvaluateName)
    val hasChildren = n.children.nonEmpty || n.childCount > 0
    v.setVariablesReference(if (hasChildren) handles.create(VariableContainer.Children(n)) else 0)
    if (hasChildren) v.setNamedVariables(n.childCount)

    val hint = new VariablePresentationHint()
    hint.setKind("data")
    hint.setAttributes(Array("readOnly"))
    if (n.isInternal) hint.setVisibility("internal")
    v.setPresentationHint(hint)
    v
  }
}
