package viper.server

import viper.silicon.state.terms.{BuiltinEquals, Combine, Unit}
import viper.silicon.verifier.Verifier
import viper.silicon._

import scala.collection.immutable.VectorBuilder
import spray.json._
import viper.silicon.interfaces.state.Chunk
import viper.silicon.resources.{FieldID, PredicateID}
import viper.silicon.rules.InverseFunctions
import viper.silicon.state.{utils => _, _}
import viper.silver.ast.AbstractLocalVar


// TODO: Clean this up
/** Wrapper for the SymbExLogReport conversion to JSON. */
object SymbExLogReportWriter {

  private def inverseFunctionsToJSON(invs: InverseFunctions): JsValue = {
    JsArray(
      TermWriter.toJSON(invs.axiomInversesOfInvertibles),
      TermWriter.toJSON(invs.axiomInvertiblesOfInverses)
    )
  }

  private def heapChunkToJSON(chunk: Chunk) = chunk match {
    case BasicChunk(PredicateID(), id, args, snap, perm) =>
      JsObject(
        "type" -> JsString("basic_predicate_chunk"),
        "predicate" -> JsString(id.toString),
        "args" -> JsArray(args.map(TermWriter.toJSON).toVector),
        "snap" -> TermWriter.toJSON(snap),
        "perm" -> TermWriter.toJSON(perm)
      )

    case BasicChunk(FieldID(), id, Seq(receiver), snap, perm) =>
      JsObject(
        "type" -> JsString("basic_field_chunk"),
        "field" -> JsString(id.toString),
        "receiver" -> TermWriter.toJSON(receiver),
        "snap" -> TermWriter.toJSON(snap),
        "perm" -> TermWriter.toJSON(perm)
      )

    // TODO: Are ID and bindings needed?
    case MagicWandChunk(id, bindings, args, snap, perm) =>
      JsObject(
        "type" -> JsString("basic_magic_wand_chunk"),
        "args" -> JsArray(args.map(TermWriter.toJSON).toVector),
        "snap" -> TermWriter.toJSON(snap),
        "perm" -> TermWriter.toJSON(perm)
      )

    case QuantifiedFieldChunk(id, fvf, perm, invs, cond, receiver, hints) =>
      JsObject(
        "type" -> JsString("quantified_field_chunk"),
        "field" -> JsString(id.toString),
        "field_value_function" -> TermWriter.toJSON(fvf),
        "perm" -> TermWriter.toJSON(perm),
        "invs" -> invs.map(inverseFunctionsToJSON).getOrElse(JsNull),
        "cond" -> cond.map(TermWriter.toJSON).getOrElse(JsNull),
        "receiver" -> receiver.map(TermWriter.toJSON).getOrElse(JsNull),
        "hints" -> (if (hints != Nil) JsArray(hints.map(TermWriter.toJSON).toVector) else JsNull)
      )

    case QuantifiedPredicateChunk(id, vars, psf, perm, invs, cond, singletonArgs, hints) =>
      JsObject(
        "type" -> JsString("quantified_predicate_chunk"),
        "vars" -> JsArray(vars.map(TermWriter.toJSON).toVector),
        "predicate" -> JsString(id.toString),
        "predicate_snap_function" -> TermWriter.toJSON(psf),
        "perm" -> TermWriter.toJSON(perm),
        "invs" -> invs.map(inverseFunctionsToJSON).getOrElse(JsNull),
        "cond" -> cond.map(TermWriter.toJSON).getOrElse(JsNull),
        "singleton_args" -> singletonArgs.map(as => JsArray(as.map(TermWriter.toJSON).toVector)).getOrElse(JsNull),
        "hints" -> (if (hints != Nil) JsArray(hints.map(TermWriter.toJSON).toVector) else JsNull)
      )

    case QuantifiedMagicWandChunk(id, vars, wsf, perm, invs, cond, singletonArgs, hints) =>
      JsObject(
        "type" -> JsString("quantified_magic_wand_chunk"),
        "vars" -> JsArray(vars.map(TermWriter.toJSON).toVector),
        "predicate" -> JsString(id.toString),
        "wand_snap_function" -> TermWriter.toJSON(wsf),
        "perm" -> TermWriter.toJSON(perm),
        "invs" -> invs.map(inverseFunctionsToJSON).getOrElse(JsNull),
        "cond" -> cond.map(TermWriter.toJSON).getOrElse(JsNull),
        "singleton_args" -> singletonArgs.map(as => JsArray(as.map(TermWriter.toJSON).toVector)).getOrElse(JsNull),
        "hints" -> (if (hints != Nil) JsArray(hints.map(TermWriter.toJSON).toVector) else JsNull)
      )

    case other => JsObject(
      "type" -> JsString("unstructrured_chunk"),
      "value" -> JsString(other.toString)
    )
  }

  // TODO: Implement structured translation of the state (PCs, heap?, store?)
  private def stateToJSON(record: SymbolicRecord) = {
    val state = record.state
    val pcs = record.pcs

    val store = JsArray(state.g.values.map({
      case (v @ AbstractLocalVar(name), value) =>
        JsObject(
          "name" -> JsString(name),
          "value" -> TermWriter.toJSON(value),
          "sort" -> TermWriter.toJSON(value.sort)
        )
      case other =>
        JsString(s"Unexpected variable in store '$other'")
    }).toVector)
    val heap = JsArray(state.h.values.map(heapChunkToJSON).toVector)
    val oldHeap = state.oldHeaps.get(Verifier.PRE_STATE_LABEL) match {
      case Some(h) => JsArray(h.values.map(heapChunkToJSON).toVector)
      case _ => JsArray()
    }

    // Ignore empty combines
    val filteredPcs = pcs.filterNot {
        case BuiltinEquals(_, Combine(Unit, Unit)) => true
        case _ => false
      }
    val pathConditions = JsArray(filteredPcs.map(TermWriter.toJSON).toVector)

    JsObject(
      "store" -> store,
      "heap" -> heap,
      "oldHeap" -> oldHeap,
      "pcs" -> pathConditions
    )
  }

  /** Translates a SymbolicRecord to a JsValue.
    *
    * @param record The symbolic to translate.
    * @return The record translated as a JsValue.
    */
  def toJSON(record: SymbolicRecord): JsValue = record match {
    case gb: GlobalBranchRecord =>
      JsObject(
        "kind" -> JsString("GlobalBranch"),
        "value" -> JsString(gb.value.toString()),
        "prestate" -> stateToJSON(gb),
        "children" -> JsArray(
          JsObject(
            "kind" -> JsString("Branch 1"),
            "children" -> JsArray(gb.thnSubs.map(toJSON).toVector)
          ),
          JsObject(
            "kind" -> JsString("Branch 2"),
            "children" -> JsArray(gb.elsSubs.map(toJSON).toVector)
          )
        )
      )

    case mc: MethodCallRecord =>
      JsObject(
        "kind" -> JsString("MethodCall"),
        "value" -> JsString(mc.value.toString()),
        "pos" -> JsString(utils.ast.sourceLineColumn(mc.value)),
        "children" -> JsArray(
          JsObject(
            "kind" -> JsString("parameters"),
            "children" -> JsArray(mc.parameters.map(toJSON).toVector)
          ),
          JsObject(
            "kind" -> JsString("precondition"),
            "prestate" -> stateToJSON(mc.precondition),
            "children" -> JsArray(toJSON(mc.precondition))
          ),
          JsObject(
            "kind" -> JsString("postcondition"),
            "prestate" -> stateToJSON(mc.postcondition),
            "children" -> JsArray(toJSON(mc.postcondition))
          )
        )
      )

    case cr: CommentRecord =>
      JsObject(
        "kind" -> JsString("comment"),
        "value" -> JsString(if(cr.comment != null) cr.comment else "")
      )

    // Records that don't have a special structure.
    case r =>
      // We first gather all the fields since many might not be present
      val members: VectorBuilder[JsField] = new VectorBuilder[(String, JsValue)]()

      // Get the kind/type and value of the record
      members ++= (r match {
        case m: MethodRecord => Vector( "kind" -> JsString("Method"), "value" -> JsString(m.value.name) )
        case p: PredicateRecord => Vector( "kind" -> JsString("Predicate"), "value" -> JsString(p.value.name) )
        case f: FunctionRecord => Vector( "kind" -> JsString("Function"), "value" -> JsString(f.value.name) )
        case e: ExecuteRecord =>
          Vector(
            "type" -> JsString("execute"),
            "pos" -> JsString(utils.ast.sourceLineColumn(e.value)),
            "value" -> JsString(e.value.toString())
          )
        case e: EvaluateRecord =>
          Vector(
            "type" -> JsString("evaluate"),
            "pos" -> JsString(utils.ast.sourceLineColumn(e.value)),
            "value" -> JsString(e.value.toString())
          )
        case p: ProduceRecord =>
          Vector(
            "type" -> JsString("produce"),
            "pos" -> JsString(utils.ast.sourceLineColumn(p.value)),
            "value" -> JsString(p.value.toString())
          )
        case c: ConsumeRecord =>
          Vector(
            "type" -> JsString("produce"),
            "pos" -> JsString(utils.ast.sourceLineColumn(c.value)),
            "value" -> JsString(c.value.toString())
          )
        case _: WellformednessCheckRecord =>
          Vector( "kind" -> JsString("WellformednessCheck") )
        case _ =>
          Vector( "kind" -> JsString(s"UnexpectedRecord ${r.toSimpleString()}") )
      })

      if (r.state != null) {
        members += ("prestate" -> stateToJSON(r))
      }

      if (r.lastFailedProverQuery.isDefined) {
        members += ("lastSMTQuery" -> TermWriter.toJSON(r.lastFailedProverQuery.get))
      }
      members += ("children" -> JsArray(r.subs.map(toJSON).toVector))

      JsObject(members.result():_*)
  }
}
