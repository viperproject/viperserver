// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.frontends.lsp

import java.net.{InetAddress, ServerSocket}
import java.util.concurrent.TimeoutException

import org.eclipse.lsp4j.jsonrpc.Launcher
import viper.server.ViperConfig
import viper.server.blackbox.{BlackBoxSuite, LspClient}
import viper.server.core.{DefaultVerificationExecutionContext, VerificationExecutionContext}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._

/** How many threads the LSP mode keeps for the files of an IDE. Threads can only be looked at from
  * inside their JVM, so this suite, unlike the black-box suites whose helpers it shares, runs the
  * server in the JVM of the tests: put together the way `ViperServerRunner` does it for one IDE.
  * The IDE is the black-box suites' [[viper.server.blackbox.LspClient]], connected over a socket.
  */
class LspThreadTests extends BlackBoxSuite {

  private var executor: VerificationExecutionContext = _
  private var service: ViperServerService = _
  private var serverSocket: ServerSocket = _
  private var ide: LspClient = _

  private val text = "method m(x: Int) returns (y: Int)\n  ensures y == x\n{\n  y := x\n}\n"

  override def beforeAll(): Unit = {
    super.beforeAll()
    val config = new ViperConfig(Seq("--serverMode", "LSP", "--singleClient", "--logFile", dir.resolve("server.log").toString))
    executor = new DefaultVerificationExecutionContext(threadPoolSize = Some(config.nThreads()))
    implicit val context: VerificationExecutionContext = executor
    service = new ViperServerService(config)
    Await.result(service.start(), 60.seconds)

    serverSocket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    ide = new LspClient(serverSocket.getLocalPort)
    val socket = serverSocket.accept()
    val receiver = new CustomReceiver(config, service, s"127.0.0.1:${serverSocket.getLocalPort}")
    val launcher = new Launcher.Builder[IdeLanguageClient]()
      .setLocalService(receiver)
      .setRemoteInterface(classOf[IdeLanguageClient])
      .setInput(socket.getInputStream)
      .setOutput(socket.getOutputStream)
      .setExecutorService(executor.executorService)
      .create()
    receiver.connect(launcher.getRemoteProxy)
    Future {
      launcher.startListening().get()
      receiver.disconnected()
      socket.close()
    }
    ide.initialize()
  }

  override def afterAll(): Unit = {
    Option(ide).foreach(_.close())
    Option(serverSocket).foreach(_.close())
    if (service != null) {
      try {
        Await.ready(service.stop(), 20.seconds)
        executor.terminate()
      } catch {
        case _: TimeoutException => // the threads that are left end with the JVM
      }
    }
    super.afterAll()
  }

  /** opens a file of a project of its own, as an IDE does, and closes it again */
  private def openAndClose(name: String): Unit = {
    val file = write(s"$name/$name.vpr", text)
    ide.open(file, text)
    ide.awaitQuiet()
    ide.close(file)
    ide.awaitQuiet(quietMs = 100)
  }

  private def liveThreads: Set[Thread] = Thread.getAllStackTraces.keySet.asScala.toSet

  /** thread names with their numbers taken out, and how many threads have each */
  private def byName(threads: Iterable[Thread]): String =
    threads.groupBy(_.getName.replaceAll("\\d+", "#")).toSeq.map { case (name, group) => s"${group.size} x $name" }.sorted.mkString(", ")

  "A ViperServer in LSP mode" when {

    "files are opened and closed" should {

      "not keep threads for the files that were closed" in wantedButNotYet {
        // today every file leaves the threads of two actor systems behind, which it used as timers
        val files = 20
        // the server's own pools have their threads after these
        (1 to 5).foreach(i => openAndClose(s"warm-up-$i"))
        val before = liveThreads
        (1 to files).foreach(i => openAndClose(s"file-$i"))
        val left = liveThreads -- before
        // threads that a pool starts late are allowed; a thread per file is not
        withClue(s"threads that were started for $files files and are still there: ${byName(left)}. ") {
          left.size should be < files
        }
      }
    }
  }
}
