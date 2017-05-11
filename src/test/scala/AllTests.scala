/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import java.nio.file.Path

import viper.carbon.CarbonVerifier
import viper.server.ViperCarbonFrontend
import viper.silver.frontend.Frontend
import viper.silver.testing.SilSuite
import viper.silver.verifier.Verifier

/** All tests for the viperServer.
  * TODO: implement tests, as this is only a method stub
  */
class AllTests extends SilSuite {
  override def testDirectories: Seq[String] = Vector(
    "local"
    //, "all", "quantifiedpermissions", "quantifiedpredicates", "quantifiedcombinations", "wands", "examples"
    //, "generated"
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    require(files.length == 1, "tests should consist of exactly one file")
    val fe = new ViperCarbonFrontend()
    fe.init(verifier)
    fe.reset(files.head)
    fe
  }

  lazy val verifiers = List(CarbonVerifier())
}
