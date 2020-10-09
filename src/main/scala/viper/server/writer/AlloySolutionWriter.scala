package viper.server.writer

import edu.mit.csail.sdg.ast.Sig.Field
import edu.mit.csail.sdg.ast.{ExprVar, Sig}
import edu.mit.csail.sdg.translator.{A4Solution, A4Tuple}
import spray.json.{JsArray, JsObject, JsString, JsValue}

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object AlloySolutionWriter {

  private def toJSON(t: A4Tuple): JsArray = {
      val atoms = ListBuffer[JsString]()
      for(i <- 0 until t.arity()) {
        atoms += JsString(t.atom(i))
      }

    JsArray(atoms.toVector)
  }

  private def toJSON(f: Field, sol: A4Solution): JsValue = {
    JsObject(
      "name" -> JsString(f.label),
      "atoms" -> JsArray(sol.eval(f).map(toJSON).toVector)
    )
  }

  private def toJSON(sig: Sig, sol: A4Solution): JsValue = {
    JsObject(
      "label" -> JsString(sig.label),
      "atoms" -> JsArray(sol.eval(sig).flatMap(s => toJSON(s).elements).toVector),
      "fields" -> JsArray(sig.getFields.map(f => toJSON(f, sol)).toVector)
    )
  }

  private def toJSON(atom: ExprVar): JsValue =
    JsObject(
      "name" -> JsString(atom.label),
      "type" -> JsString(atom.`type`().toString)
    )

  def toJSON(solution: A4Solution): JsValue = {
    val signatures = solution.getAllReachableSigs
                             .iterator()
                             .filter(s => s.label.startsWith("this/"))
                             .map(s => toJSON(s, solution))
    JsObject(
      "signatures" -> JsArray(signatures.toVector),
      "atoms" -> JsArray(solution.getAllAtoms.map(toJSON).toVector)
    )
  }
}
