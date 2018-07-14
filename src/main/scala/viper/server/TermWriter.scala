package viper.server

import spray.json._
import viper.silicon.state.Identifier
import viper.silicon.state.terms._

/**
  *
  */
object TermWriter {

  private def binary(op: String, lhs: JsValue, rhs: JsValue): JsValue =
    JsObject(
      "type" -> JsString("binary"),
      "op" -> JsString(op),
      "lhs" -> lhs,
      "rhs" -> rhs
    )

  private def unary(op: String, p: JsValue) =
    JsObject(
      "type" -> JsString("unary"),
      "op" -> JsString(op),
      "p" -> p
    )

  private def variable(id: Identifier, sort: Sort) =
    JsObject(
      "type" -> JsString("variable"),
      "id" -> JsString(id.name),
      "sort" -> JsString(sort.toString)
    )

  // TODO: Might have to also structure Sorts, because of parametric types such as Set[...]
  def toJSON(term: Term): JsValue = term match {

    case Combine(p0, Unit) => toJSON(p0)
    case Combine(Unit, p1) => toJSON(p1)

    case b: BinaryOp[Term] => binary(b.op, toJSON(b.p0), toJSON(b.p1))
    case u: UnaryOp[Term] => unary(u.op, toJSON(u.p))

    // TODO: do we need triggers and isGlobal?
    case Quantification(quantifier, vars, body, _, name, _) =>
      JsObject(
        "type" -> JsString("quantification"),
        "quantifier" -> JsString(quantifier.toString),
        "vars" -> JsArray((vars map toJSON).toVector),
        "body" -> toJSON(body),
        "name" -> (if (name != null) JsString(name) else JsNull)
      )

    case App(applicable, args) =>
      JsObject(
        "type" -> JsString("application"),
        "applicable" -> JsString(applicable.id.name),
        "args" -> JsArray((args map toJSON).toVector)
      )

    case Lookup(field, fieldValueFunction, receiver) =>
      JsObject(
        "type" -> JsString("lookup"),
        "field" -> JsString(field),
        "fieldValueFunction" -> toJSON(fieldValueFunction),
        "receiver" -> toJSON(receiver)
      )
    case PredicateLookup(predicate, predicateSnapFunction, args) =>
      JsObject(
        "type" -> JsString("predicateLookup"),
        "predicate" -> JsString(predicate),
        "predicateSnapFunction" -> toJSON(predicateSnapFunction),
        "args" -> JsArray((args map toJSON).toVector)
      )

    case And(terms) => JsObject("type" -> JsString("and"), "terms" -> JsArray((terms map toJSON).toVector))
    case Or(terms) => JsObject("type" -> JsString("or"), "terms" -> JsArray((terms map toJSON).toVector))

    case Distinct(symbols) =>
      JsObject(
        "type" -> JsString("distinct"),
        "terms" -> JsArray((symbols map (s => JsString(s.id.name))).toVector)
      )

    case Ite(cond, thenBranch, elseBranch) =>
      JsObject(
        "type" -> JsString("ite"),
        "cond" -> toJSON(cond),
        "thenBranch" -> toJSON(thenBranch),
        "elseBranch" -> toJSON(elseBranch)
      )

    case Var(id, sort) => variable(id, sort)
    case SortWrapper(t, sorts.Snap) => toJSON(t)
    case Let(bindings, body) =>
      val bs = bindings map { case (v, t) => JsObject("var" -> toJSON(v), "value" -> toJSON(t)) }
      JsObject(
        "type" -> JsString("let"),
        "bindings" -> JsArray(bs.toVector),
        "body" -> toJSON(body)
      )

    case l: Literal =>
      JsObject(
        "type" -> JsString("literal"),
        "sort" -> JsString(l.sort.toString),
        "value" -> JsString(l.toString)
      )

    // PermLiteral is not actually a Literal. This case can actually be reached
    // and we map PermLiterals to normal literals.
    case p: PermLiteral =>
      JsObject(
        "type" -> JsString("literal"),
        "sort" -> JsString(sorts.Perm.toString),
        "value" -> JsString(p.toString)
      )

    case SeqRanged(p0, p1) => JsObject("type" -> JsString("seqRanged"), "lhs" -> toJSON(p0), "rhs" -> toJSON(p1))
    case SeqSingleton(p) => JsObject("type" -> JsString("seqSingleton"), "value" -> toJSON(p))
    case SeqUpdate(seq, index, value) => JsObject(
      "type" -> JsString("seqUpdate"),
      "seq" -> toJSON(seq),
      "index" -> toJSON(index),
      "value" -> toJSON(value)
    )

    case SingletonSet(p) => JsObject("type" -> JsString("singletonSet"), "value" -> toJSON(p))
    case SingletonMultiset(p) => JsObject("type" -> JsString("singletonMultiset"), "value" -> toJSON(p))

      // TODO: What about domains?

    case other => JsObject(
      "type" -> JsString("unstructrured"),
      "value" -> JsString(other.toString)
    )
  }
}
