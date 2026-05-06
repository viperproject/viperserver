package viper.server.core

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}
import viper.server.frontends.http.ViperHttpServer
import viper.server.ViperConfig

import scala.concurrent.Await
import scala.concurrent.duration._

class ViperServerHttpSpec extends AnyWordSpec with Matchers with TimeLimits {

  private val verificationContext: VerificationExecutionContext = new DefaultVerificationExecutionContext()
  private val viperServerHttp = {
    val config = new ViperConfig(IndexedSeq())
    new ViperHttpServer(config)(verificationContext)
  }

  // Started lazily on first test to set the bound port.
  private def baseUrl: String = s"http://localhost:${viperServerHttp.port}"

  private val verifiableFile = "src/test/resources/viper/let.vpr"
  private val nonExistingFile = "2165e0fbd4b980436557b5a6f1a41f68.vpr"
  private val emptyFile = "src/test/resources/viper/empty.vpr"

  private val tool = "silicon"
  private val testSimpleViperCode_cmd = s"$tool --disableCaching $verifiableFile"
  private val testEmptyFile_cmd = s"$tool --disableCaching $emptyFile"
  private val testEmptyFileWithCaching_cmd = s"$tool $emptyFile"
  private val testNonExistingFile_cmd = s"$tool --disableCaching $nonExistingFile"

  private def postVerify(cmd: String): String = {
    val body = s"""{"arg":${jsonString(cmd)}}"""
    requests.post(s"$baseUrl/verify", data = body, headers = Map("Content-Type" -> "application/json")).text()
  }

  private def jsonString(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"' => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c => sb.append(c)
    }
    sb.append("\"").toString
  }

  "ViperServer" should {
    "eventually start" in {
      failAfter(Span(10, Seconds)) {
        val started = viperServerHttp.start()
        Await.result(started, Duration.Inf)
      }
    }

    s"start a verification process using `$tool` over a small Viper program" in {
      val resp = postVerify(testSimpleViperCode_cmd)
      resp should not include "File not found"
    }

    "respond with the result for process #0" in {
      val resp = requests.get(s"$baseUrl/verify/0").text()
      resp should include(s""""kind":"overall","status":"success","verifier":"$tool"""")
    }

    s"start another verification process using `$tool` on an empty file" in {
      val resp = postVerify(testEmptyFile_cmd)
      resp should not include "File not found"
    }

    "respond with the result for process #1" in {
      val resp = requests.get(s"$baseUrl/verify/1").text()
      resp should include(s""""kind":"overall","status":"success","verifier":"$tool"""")
    }

    s"start another verification process with caching enabled using `$tool` on an empty file (Issue #111)" in {
      val resp = postVerify(testEmptyFileWithCaching_cmd)
      resp should not include "File not found"
    }

    "respond with the result for process #2 that should not contain an exception report (Issue #111)" in {
      val resp = requests.get(s"$baseUrl/verify/2").text()
      resp should include(s""""kind":"overall","status":"success","verifier":"$tool"""")
      resp should not include """"msg_type":"exception_report""""
    }

    s"start another verification process using `$tool` on an non-existent file" in {
      val resp = postVerify(testNonExistingFile_cmd)
      resp should include("not found")
    }

    "stop all running executions and terminate self" in {
      val resp = requests.get(s"$baseUrl/exit").text()
      resp should include("shutting down")
    }

    "should eventually stop" in {
      failAfter(Span(10, Seconds)) {
        Await.ready(viperServerHttp.stopped(), Duration.Inf)
        verificationContext.terminate()
      }
    }
  }
}
