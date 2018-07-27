package viper.server

import edu.mit.csail.sdg.ast.Sig
import edu.mit.csail.sdg.ast.Sig.Field

import collection.JavaConversions._
import edu.mit.csail.sdg.translator.{A4Solution, A4Tuple}
import spray.json.{JsArray, JsObject, JsString, JsValue}

import scala.collection.mutable.ListBuffer

/**
  *
  */
object AlloySolutionWriter {

  private def toJSON(t: A4Tuple): JsArray = {
      val atoms = ListBuffer[JsString]()
      for(i <- 0 until t.arity()) {
        atoms += JsString(t.atom(i))
      }

    JsArray(atoms.toVector)
  }

  private def toJSON(f: Field, sol: A4Solution): JsValue = {
    val atoms = sol.eval(f)
    if (atoms.arity() > 0) {
      JsObject(
        "name" -> JsString(f.label),
        "atoms" -> JsArray(sol.eval(f).map(toJSON).toVector)
      )
    } else {
      JsObject( "name" -> JsString(f.label) )
    }
  }

  private def toJSON(sig: Sig, sol: A4Solution): JsValue = {
    if (sig.getFields.size() > 0) {
      JsObject(
        "label" -> JsString(sig.label),
        "fields" -> JsArray(sig.getFields.map(f => toJSON(f, sol)).toVector)
      )
    } else {
      JsObject( "label" -> JsString(sig.label) )
    }
  }


  def toJSON(solution: A4Solution): JsValue = {
    val signatures = solution.getAllReachableSigs
                             .iterator()
                             .filter(s => s.label.startsWith("this/"))
                             .map(s => toJSON(s, solution))
    JsObject(
      "signatures" -> JsArray(signatures.toVector)
    )
  }
}
