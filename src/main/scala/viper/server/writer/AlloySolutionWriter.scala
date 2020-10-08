// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.writer

import edu.mit.csail.sdg.ast.Sig.Field
import edu.mit.csail.sdg.ast.{ExprVar, Sig}
import edu.mit.csail.sdg.translator.{A4Solution, A4Tuple}
import spray.json._

import scala.collection.JavaConverters._
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
      "atoms" -> JsArray(sol.eval(f).asScala.map(toJSON).toVector)
    )
  }

  private def toJSON(sig: Sig, sol: A4Solution): JsValue = {
    JsObject(
      "label" -> JsString(sig.label),
      "atoms" -> JsArray(sol.eval(sig).asScala.flatMap(s => toJSON(s).elements).toVector),
      "fields" -> JsArray(sig.getFields.asScala.map(f => toJSON(f, sol)).toVector)
    )
  }

  private def toJSON(atom: ExprVar): JsValue =
    JsObject(
      "name" -> JsString(atom.label),
      "type" -> JsString(atom.`type`().toString)
    )

  def toJSON(solution: A4Solution): JsValue = {
    val signatures = solution.getAllReachableSigs.asScala
//                             .iterator
                             .filter(s => s.label.startsWith("this/"))
                             .map(s => toJSON(s, solution))
    JsObject(
      "signatures" -> JsArray(signatures.toVector),
      "atoms" -> JsArray(solution.getAllAtoms.asScala.map(toJSON).toVector)
    )
  }
}
