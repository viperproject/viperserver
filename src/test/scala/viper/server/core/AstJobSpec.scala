// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.core

import org.scalatest.wordspec.AnyWordSpec
import viper.server.ViperConfig
import viper.server.frontends.lsp.{ClientCoordinator, ViperServerService}

import java.nio.file.{Files, Paths}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, TimeoutException}

/**
  * Tests how the LSP frontend manages AST construction jobs, of which only a few may exist at a time
  * (`JobPool.MAX_ACTIVE_JOBS`): files that were only parsed must not keep their job once they are closed, and a
  * verification that cannot get a job must say so rather than never finish.
  */
class AstJobSpec extends AnyWordSpec {

  private val files = Seq("let.vpr", "sum_method.vpr", "empty.vpr", "verification_error.vpr")
    .map(f => Paths.get("src/test/resources/viper", f).toAbsolutePath)

  private def withCoordinator(test: (ClientCoordinator, VerificationExecutionContext) => Unit): Unit = {
    val context = new DefaultVerificationExecutionContext()
    val logFile = Paths.get("logs", s"viperserver_ast_${System.currentTimeMillis()}.log").toFile
    logFile.getParentFile.mkdirs
    logFile.createNewFile()
    val server = new ViperServerService(new ViperConfig(Seq("--logLevel", "INFO", "--logFile", logFile.getAbsolutePath)))(context)
    Await.result(server.start(), 1.minute)
    val coordinator = new ClientCoordinator(server)(context)
    coordinator.setClient(new SilentClient())
    try test(coordinator, context)
    finally {
      Await.result(coordinator.stopAllRunningVerifications(), 1.minute)
      // Stopping the server interrupts the verification the test started, while its AST is being constructed, and
      // may then not finish; like the LSP shutdown handler, wait only briefly.
      scala.util.Try(Await.result(server.stop(), 10.seconds))
      context.terminate(5000)
    }
  }

  /** Opens a file like the IDE does: the file manager is created, and the file is parsed and typechecked. */
  private def openAndParse(coordinator: ClientCoordinator, file: java.nio.file.Path): String = {
    val uri = file.toUri.toString
    assert(coordinator.canVerificationBeStarted(uri, Files.readString(file), manuallyTriggered = false))
    assert(coordinator.startParseTypecheck(uri), s"parsing $file should have started")
    uri
  }

  "The LSP frontend" should {

    "release the AST jobs of files that were only parsed once they are closed" in withCoordinator { (coordinator, _) =>
      // As many files as there may be AST jobs: each is parsed, but not verified, and then closed.
      for (file <- files.init) {
        val uri = openAndParse(coordinator, file)
        coordinator.closeFile(uri)
      }
      // A verification needs an AST job of its own.
      val uri = files.last.toUri.toString
      assert(coordinator.canVerificationBeStarted(uri, Files.readString(files.last), manuallyTriggered = true))
      val started: Future[Boolean] = coordinator.startVerification("silicon", "", uri, manuallyTriggered = true)
      val result = try Await.result(started, 30.seconds) catch {
        case _: TimeoutException => fail("starting the verification never finished")
      }
      assert(result, "the verification should have been started, since the closed files no longer need their AST jobs")
    }

    "make room for a verification by releasing the finished AST jobs of open files" in withCoordinator { (coordinator, _) =>
      // As many files as there may be AST jobs stay open, but are only parsed.
      files.init.foreach(openAndParse(coordinator, _))
      // Give the parsing time to finish: only finished jobs are released.
      Thread.sleep(5000)
      val uri = files.last.toUri.toString
      assert(coordinator.canVerificationBeStarted(uri, Files.readString(files.last), manuallyTriggered = true))
      val result = try Await.result(coordinator.startVerification("silicon", "", uri, manuallyTriggered = true), 30.seconds)
      catch {
        case _: TimeoutException => fail("starting the verification never finished")
      }
      assert(result, "the verification should have been started in place of an idle AST job")
    }
  }
}
