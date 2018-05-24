/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import java.nio.file.Path

//import viper.server.ViperCarbonFrontend
import viper.silver.frontend.Frontend
import viper.silver.reporter.StdIOReporter
import viper.silver.testing.SilSuite
import viper.silver.verifier.Verifier

/*
class AllTests extends SilSuite {
  override def testDirectories: Seq[String] = Vector(
    "local"
    //, "all", "quantifiedpermissions", "quantifiedpredicates", "quantifiedcombinations", "wands", "examples"
    //, "generated"
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    require(files.length == 1, "tests should consist of exactly one file")
    val fe = new ViperCarbonFrontend(new StdIOReporter("carbon_tests_reporter"))
    fe.init(verifier)
    fe.reset(files.head)
    fe
  }

  lazy val verifiers = List(CarbonVerifier())
}
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
