package viper.server.core

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distrcibuted with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

import org.scalatest.{Assertion, Succeeded}
import org.scalatest.funsuite.AnyFunSuite
import viper.server.ViperConfig
import viper.server.frontends.lsp.{ClientCoordinator, ViperServerService}
import viper.server.frontends.lsp.file.FileManager
import viper.server.utility.AstGenerator
import viper.server.vsi.JobNotFoundException
import viper.silver.ast.utility.DiskLoader
import viper.silver.logger.SilentLogger

import java.nio.file.Paths
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class BranchTreeTests extends AnyFunSuite {

  val testDir = Paths.get("src", "test", "resources", "viper","branch-tree").toString
  private val ast_gen = new AstGenerator(SilentLogger().get)

  def executeTestDefault(fileName: String) : Unit = executeTest(fileName, "default")

  test("FirstPathFailsPath") {
    executeTestDefault("firstPathFails")
  }
  test("LastPathFailsPath") {
    executeTestDefault("lastPathFails")
  }
  test("WhilePath") {
    executeTestDefault("while")
  }
  test("OnlyIfPath") {
    executeTestDefault("onlyIf")
  }
  test("AllPathsPath") {
    executeTestDefault("allPathsCorrect")
  }
  test("NoBranchesPath") {
    executeTestDefault("noBranches")
  }
  test("MultipleMethodsPath") {
    executeTestDefault("multipleMethods")
  }

  def executeTestReportAllErrors(fileName: String) : Unit = executeTest(fileName, "reportAllErrors", List("--numberOfErrorsToReport", "0"))

  test("FirstPathFailsTreeAll") {
    executeTestReportAllErrors("firstPathFails")
  }
  test("LastPathFailsTreeAll") {
    executeTestReportAllErrors("lastPathFails")
  }
  test("WhileTreeAll") {
    executeTestReportAllErrors("while")
  }
  test("OnlyIfTreeAll") {
    executeTestReportAllErrors("onlyIf")
  }
  test("AllPathsCorrectTreeAll") {
    executeTestReportAllErrors("allPathsCorrect")
  }
  test("NoBranchesTreeAll") {
    executeTestReportAllErrors("noBranches")
  }

  def executeTestReportTwoErrors(fileName: String) : Unit = executeTest(fileName, "reportTwoErrors", List("--numberOfErrorsToReport", "2"))

  test("FirstPathFailsTreeTwo") {
    executeTestReportTwoErrors("firstPathFails")
  }
  test("LastPathFailsTreeTwo") {
    executeTestReportTwoErrors("lastPathFails")
  }

  def getExpectedString(fileName: String, expectedFolder : String): String = {
    val expectedFilePath = Paths.get(testDir,expectedFolder,fileName+"_expected")
    DiskLoader.loadContent(expectedFilePath).get
  }

  def executeTest(fileName: String, expectedFolder : String, args: List[String] = List.empty)
  : Unit = {
    val filePath = Paths.get(testDir,s"$fileName.vpr")
    val expected = getExpectedString(fileName, expectedFolder)

    val viperConfig = new ViperConfig(Seq.empty)
    val siliconConfig: SiliconConfig = SiliconConfig(args)
    val ast = ast_gen.generateViperAst(filePath.toString).get

    val verificationContext = new DefaultVerificationExecutionContext()
    val core = new ViperServerService(viperConfig)(verificationContext)
    val started =  core.start()

    // execute testCode
    val testFuture = started.flatMap(_ => {
      val coordinator = new ClientCoordinator(core)(verificationContext)
      coordinator.setClient(new MockClient())
      val fileManager = FileManager(filePath.toAbsolutePath.toUri.toString, coordinator, None)(verificationContext)
      val jid = core.verify("some-program-id", siliconConfig, ast)
      val relayActorRef = verificationContext.actorSystem.actorOf(fileManager.props(Some("silicon")))
      core.streamMessages(jid, relayActorRef, true).getOrElse(Future.failed(JobNotFoundException))
        .map(_=>{
          val diags = fileManager.getDiagnostic()
          val actual = if (diags.nonEmpty) diags.map(_.getMessage)
                        .filter(m => m.startsWith("Branch"))
            .flatMap(m => m.split("\n").filterNot(l => l.startsWith(" (")
              ||l.startsWith("  [")
              ||l.startsWith("Branch"))
            ).map(str => str+"\n").reduce((str,s)=>str+s) else "Verification successful."
          assert(actual.contains(expected))
        })(verificationContext)
    })(verificationContext)
    val testWithShutdownFuture = testFuture.transformWith(testRes => {
      val coreStopFuture = core.stop()
      testRes match {
        case Success(_) => coreStopFuture
        case f: Failure[Assertion] => coreStopFuture.transform(_ => f)(verificationContext)
      }
    })(verificationContext)
    Await.result(testWithShutdownFuture, Duration.Inf)
  }
}


