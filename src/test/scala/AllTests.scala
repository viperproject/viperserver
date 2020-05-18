/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
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

  import scala.language.postfixOps
  import ViperRequests._
  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()
  implicit val requestTimeput: RouteTestTimeout = RouteTestTimeout(10.second dilated)

  //val viperHttpServer: ViperHttpServer = new ViperHttpServer(Array())

  ViperServerRunner.main(Array())

  private val _routsUnderTest = ViperServerRunner.httpServer.routes(ViperServerRunner.httpServer.logger)

  def printRequestResponsePair(req: String, res: String): Unit = {
    println(s">>> ViperServer test request `$req` response in the following response: $res")
  }

  def getResourcePath(vpr_file: String): String = {
    val cross_platform_path = new File(vpr_file) getPath
    val resource = getClass.getResource(cross_platform_path)
    val fname = if (resource != null) {
      val file = Paths.get(resource.toURI)
      file.toString
    } else {
      // simulate absent file
      val temp_file = File.createTempFile("ViperServer_testing", ".vpr")
      val absent_fname = temp_file.getPath
      temp_file.delete()
      absent_fname
    }
    "\"" + fname + "\""
  }

  private val verifiableFile = "viper/let.vpr"
  private val nonExistingFile = "viper/bla.vpr"
  private val emptyFile ="viper/empty.vpr"

  private val tool = "silicon"
  private val testSimpleViperCode_cmd = s"$tool --disableCaching ${getResourcePath(verifiableFile)}"
  private val testEmptyFile_cmd = s"$tool --disableCaching ${getResourcePath(emptyFile)}"
  private val testNonExistingFile_cmd = s"$tool --disableCaching ${getResourcePath(nonExistingFile)}"

  "ViperServer" should {
    s"start a verification session using `$tool` over a small Viper program" in {
      Post("/verify", VerificationRequest(testSimpleViperCode_cmd)) ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"POST, /verify, $testSimpleViperCode_cmd", responseAs[String])
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "respond with the result for session #0" in {
      Get("/verify/0") ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"GET, /verify/0", responseAs[String])
        responseAs[String] should include (s""""kind":"overall","status":"success","verifier":"$tool"""")
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    s"start another verification session using `$tool` " in {
      Post("/verify", VerificationRequest(testEmptyFile_cmd)) ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"POST, /verify, $testEmptyFile_cmd", responseAs[String])
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
      }
    }

    "respond with the result for session #1" in {
      Get("/verify/1") ~> _routsUnderTest ~> check {
        //printRequestResponsePair(s"GET, /verify/1", responseAs[String])
        responseAs[String] should include (s""""kind":"overall","status":"success","verifier":"$tool"""")
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
