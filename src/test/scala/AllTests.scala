/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import java.io.File
import java.nio.file.Paths

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.scalatest.{Matchers, WordSpec}
import viper.server.{ViperRequests, ViperServerRunner}

import scala.concurrent.duration._

class ViperServerSpec extends WordSpec with Matchers with ScalatestRouteTest {

  import ViperRequests._
  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()
  implicit val requestTimeput: RouteTestTimeout = RouteTestTimeout(10.second dilated)

  ViperServerRunner.main(Array())

  private val _routsUnderTest = ViperServerRunner.routes(ViperServerRunner.logger)

  def printRequestResponsePair(req: String, res: String): Unit = {
    println(s">>> ViperServer test request `$req` response in the following response: $res")
  }

  def getResourcePath(sil_file: String): String = {
    val resource = getClass.getResource(sil_file)
    val fname = if (resource != null) {
      val file = Paths.get(resource.toURI)
      file.toString
    } else {
      // simulate absent file
      val temp_file = File.createTempFile("io_testing", ".sil")
      val absent_fname = temp_file.getPath
      temp_file.delete()
      absent_fname
    }
    fname
  }

  val verifiableFile = "all/basic/let.sil"
  //val nonExistingFile = "bla/bla/bla.sil"
  val emptyFile = "graphs/empty.vpr"

  val tool = "silicon"
  val testSimpleViperCodeWithSilicon_cmd = s"$tool ${getResourcePath(verifiableFile)}"
  val testEmptyFileWithSilicon_cmd = s"$tool ${getResourcePath(emptyFile)}"

  "ViperServer" should {

    "start a verification session using Carbon over a small Viper program" in {
      Post("/verify", VerificationRequest(testSimpleViperCodeWithSilicon_cmd)) ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"POST, /verify, $testSimpleViperCodeWithSilicon_cmd", responseAs[String])
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "respond with the result for session #0" in {
      Get("/verify/0") ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"GET, /verify/0", responseAs[String])
        responseAs[String] should include (""""kind":"overall","status":"success","verifier":"silicon"""")
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "start another verification session using Carbon " in {
      Post("/verify", VerificationRequest(testEmptyFileWithSilicon_cmd)) ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"POST, /verify, $testEmptyFileWithSilicon_cmd", responseAs[String])
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "respond with the result for session #1" in {
      Get("/verify/1") ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"GET, /verify/1", responseAs[String])
        responseAs[String] should include (""""kind":"overall","status":"success","verifier":"silicon"""")
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "stop all running executions and terminate self" in {
      Get("/exit") ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"GET, /exit", responseAs[String])
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }
  }

}
