// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** A ViperServer that runs in a process of its own, started the way a user starts it: the main
  * class with command-line options. A test knows this process only by its options, its standard
  * output, its exit code and the port it listens on.
  *
  * The process is started from the class path of the running test JVM, so that it runs the code
  * under test and finds the classes of the test sources, e.g. [[ScriptedBackend]].
  */
final class ServerProcess private(val process: Process, val options: Seq[String]) {

  private val lines = ArrayBuffer[String]()
  private var outputEnded = false

  private val reader = new Thread(() => {
    val in = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
    try {
      var line = in.readLine()
      while (line != null) {
        lines.synchronized {
          lines += line
          lines.notifyAll()
        }
        line = in.readLine()
      }
    } catch {
      case _: java.io.IOException => // the process is gone
    } finally {
      lines.synchronized {
        outputEnded = true
        lines.notifyAll()
      }
    }
  }, "server-process-output")
  reader.setDaemon(true)
  reader.start()

  /** everything the process has written to its standard output and standard error so far */
  def output: Seq[String] = lines.synchronized(lines.toList)

  /** the first group of the first output line that matches `pattern`; waits for such a line */
  def awaitOutput(pattern: Regex, timeoutMs: Long): String = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var checked = 0
    lines.synchronized {
      while (true) {
        while (checked < lines.size) {
          pattern.findFirstMatchIn(lines(checked)) match {
            case Some(m) => return m.group(1)
            case None => checked += 1
          }
        }
        val remaining = deadline - System.currentTimeMillis()
        if (outputEnded || remaining <= 0) {
          throw new AssertionError(
            s"the server did not print a line matching `$pattern` " +
              (if (outputEnded) "before its output ended" else s"within $timeoutMs ms") +
              s". Its output:\n${lines.mkString("\n")}")
        }
        lines.wait(remaining)
      }
      throw new IllegalStateException("unreachable")
    }
  }

  /** the port named in the server's start-up line */
  lazy val port: Int = awaitOutput(ServerProcess.startUpLine, ServerProcess.startUpTimeoutMs).toInt

  /** the exit code, if the process ends within the given time */
  def awaitExit(timeoutMs: Long): Option[Int] =
    if (process.waitFor(timeoutMs, TimeUnit.MILLISECONDS)) Some(process.exitValue()) else None

  def isAlive: Boolean = process.isAlive

  /** ends the process, whatever state it is in */
  def destroy(): Unit = {
    process.destroyForcibly()
    process.waitFor(10, TimeUnit.SECONDS)
    ServerProcess.forget(this)
  }
}

object ServerProcess {

  /** printed by a server in HTTP mode once it is listening */
  val httpStartUpLine: Regex = """^ViperServer online at http://localhost:(\d+)$""".r
  /** printed by a server in LSP mode before it accepts its first client */
  val lspStartUpLine: Regex = """^<ViperServerPort:(\d+)>$""".r
  private val startUpLine: Regex = """^(?:ViperServer online at http://localhost:|<ViperServerPort:)(\d+)>?$""".r

  /** a cold JVM on a busy machine takes its time */
  val startUpTimeoutMs: Long = 120000

  private val running = scala.collection.mutable.Set[ServerProcess]()

  // no server of a test outlives the test JVM:
  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    running.synchronized(running.toList).foreach(_.process.destroyForcibly())
  }))

  private def forget(server: ServerProcess): Unit = running.synchronized { running -= server }

  /** Starts `java viper.server.ViperServerRunner <options>` and returns without waiting for the
    * server to be ready; `port` waits for the start-up line.
    */
  def start(options: Seq[String], jvmOptions: Seq[String] = Seq.empty): ServerProcess = {
    val java = new File(new File(System.getProperty("java.home"), "bin"), "java").getPath
    // a server is started with large thread stacks, as `README.md` says:
    val inherited = Seq("logback.configurationFile").flatMap(name => sys.props.get(name).map(value => s"-D$name=$value"))
    val command = Seq(java, "-Xss128m") ++ inherited ++ jvmOptions ++ Seq("viper.server.ViperServerRunner") ++ options
    val builder = new ProcessBuilder(command: _*)
    // The class path goes through the environment and not through the command line: under sbt it
    // has several hundred entries, more than a command line may hold on Windows.
    builder.environment().put("CLASSPATH", System.getProperty("java.class.path"))
    builder.redirectErrorStream(true)
    val server = new ServerProcess(builder.start(), options)
    running.synchronized { running += server }
    server
  }
}
