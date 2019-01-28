/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import org.scalatest.{ Matchers, WordSpec }
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server._
import akka.http.scaladsl.model._
import Directives._

class ViperServerSpec extends WordSpec with Matchers with ScalatestRouteTest {

  val emptyRoute =
    get {
      path("/") {
        complete("OK")
      }
    }

  val exitRoute =
    get {
      path("exit") {
        complete("")
      }
    }

  "ViperServer" should {
    "stop all running executions and terminate self" in {
      // tests:
      Get("/exit") ~> exitRoute ~> check {
        status should ===(StatusCodes.OK)

        contentType should ===(ContentTypes.`text/plain(UTF-8)`)
      }
    }
  }

}
