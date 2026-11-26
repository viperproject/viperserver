// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters._

/** What the suites share that test a ViperServer from the outside: a directory for their files,
  * the server processes they start, and the two markers for tests of behaviour that the server
  * does not have yet.
  *
  * With the environment variable `VIPERSERVER_TEST_VERBOSE` set, a suite prints why each pending
  * test is still pending and what its servers wrote, and it keeps its files.
  */
trait BlackBoxSuite extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  /** the suite's own directory. It is the real path, because a server may name a file by it. */
  protected var dir: Path = _
  private var servers = Vector[ServerProcess]()
  private val counter = new AtomicInteger()

  protected val verbose: Boolean = sys.env.contains("VIPERSERVER_TEST_VERBOSE")

  override def beforeAll(): Unit = {
    dir = Files.createTempDirectory("viperserver-blackbox-tests").toRealPath()
  }

  override def afterAll(): Unit = {
    val output = servers.map(_.output)
    servers.foreach(_.destroy())
    if (verbose) {
      servers.zip(output).foreach { case (server, lines) =>
        println(s"---- output of the server started with `${server.options.mkString(" ")}`\n${lines.mkString("\n")}")
      }
      println(s"---- the suite's files are in $dir")
    } else if (dir != null) {
      // as far as possible: on Windows a file that a process is just letting go of cannot be deleted
      Files.walk(dir).iterator().asScala.toSeq.reverse.foreach(p => try Files.deleteIfExists(p) catch { case _: java.io.IOException => })
    }
  }

  /** Starts a server that is ended when the suite ends. Its log file goes to the suite's
    * directory. */
  protected def startServer(name: String, options: String*): ServerProcess = {
    val server = ServerProcess.start(options ++ Seq("--logFile", dir.resolve(s"$name.log").toString))
    servers :+= server
    server
  }

  protected def write(name: String, content: String): Path = {
    val file = dir.resolve(name)
    Files.createDirectories(file.getParent)
    Files.write(file, content.getBytes(UTF_8))
  }

  /** a path in the suite's directory that nothing else uses; the file is not created */
  protected def newFile(kind: String): Path = dir.resolve(s"$kind-${counter.incrementAndGet()}")

  protected def quoted(path: Path): String = "\"" + path + "\""

  protected def journalOf(journal: Path): Seq[String] = ScriptedBackend.readJournal(journal)

  protected def eventually(what: String, timeoutMs: Long = 20000)(condition: => Boolean): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (!condition) {
      if (System.currentTimeMillis() > deadline) fail(s"not within $timeoutMs ms: $what")
      Thread.sleep(20)
    }
  }

  /** For behaviour that is wanted and that the server does not have yet. The body describes the
    * wanted behaviour and is run: while it fails, the test is reported as pending, and once it
    * passes, the test fails with the request to remove this marker.
    */
  protected def wantedButNotYet(body: => Unit): Assertion = pendingUntilFixed {
    try body catch {
      case e: Throwable if verbose =>
        println(s"    still pending because: ${e.toString.linesIterator.take(3).mkString(" | ").take(600)}")
        throw e
    }
  }

  /** Like [[wantedButNotYet]], for behaviour that the server has today in some runs and not in
    * others, so that running the body would tell nothing. The body is run only if the environment
    * variable `VIPERSERVER_TEST_RUN_ALL` is set.
    */
  protected def wantedButNotRun(body: => Unit): Assertion =
    if (sys.env.contains("VIPERSERVER_TEST_RUN_ALL")) { body; succeed } else pending
}
