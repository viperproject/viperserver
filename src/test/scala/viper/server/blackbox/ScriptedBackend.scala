// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import ch.qos.logback.classic.Logger
import org.rogach.scallop.ScallopOption
import viper.silver.ast.Program
import viper.silver.frontend.{SilFrontend, SilFrontendConfig}
import viper.silver.reporter.{ConfigurationConfirmation, Reporter}
import viper.silver.verifier.{Dependency, Success, VerificationResult, Verifier}

import scala.jdk.CollectionConverters._

/** A verification backend that follows a script instead of verifying anything, for tests that
  * must know when a verification runs, or that need a verification to last exactly as long as the
  * test wants. A server loads it like any custom backend: by its class name in the place of
  * `silicon` or `carbon`.
  *
  * Its run consists of these steps, each of which is left out unless its option is given:
  *  1. it writes `<name> started` to the journal
  *  1. it reports the text of `--scriptText`
  *  1. it reports `--scriptMessages` messages of `--scriptMessageSize` characters each
  *  1. it sleeps for `--scriptRunMs` milliseconds
  *  1. it waits until the file `--scriptWaitFor` exists, reporting a message every
  *     `--scriptReportEveryMs` milliseconds, if that option is given
  *  1. it writes `<name> finished` to the journal and returns "verified"
  *
  * If the run ends in any other way, e.g. because it is interrupted or because a report fails, it
  * writes `<name> aborted <exception class>` to the journal instead. With `--scriptStopMs` it
  * first takes that many milliseconds to do so, like a backend that needs a moment to stop.
  *
  * The journal is a file that a test reads; all runs of a server may share one. Every message
  * this backend reports is a `configuration_confirmation` whose text starts with `scripted: `.
  */
class ScriptedBackend(override val reporter: Reporter, override implicit val logger: Logger) extends SilFrontend {

  private var scripted: ScriptedBackend.ScriptedVerifier = _

  override def createVerifier(fullCmd: String): Verifier = {
    scripted = new ScriptedBackend.ScriptedVerifier(reporter)
    scripted
  }

  override def configureVerifier(args: Seq[String]): SilFrontendConfig = {
    scripted.parseCommandLine(args)
    scripted.config
  }
}

object ScriptedBackend {

  /** the name by which a request selects this backend */
  val className: String = classOf[ScriptedBackend].getName

  class Config(args: Seq[String]) extends SilFrontendConfig(args, "ScriptedBackend") {
    val scriptName: ScallopOption[String] = opt[String]("scriptName", default = Some("run"), noshort = true)
    val scriptJournal: ScallopOption[String] = opt[String]("scriptJournal", noshort = true)
    val scriptText: ScallopOption[String] = opt[String]("scriptText", noshort = true)
    val scriptMessages: ScallopOption[Int] = opt[Int]("scriptMessages", default = Some(0), noshort = true)
    val scriptMessageSize: ScallopOption[Int] = opt[Int]("scriptMessageSize", default = Some(0), noshort = true)
    val scriptRunMs: ScallopOption[Int] = opt[Int]("scriptRunMs", default = Some(0), noshort = true)
    val scriptWaitFor: ScallopOption[String] = opt[String]("scriptWaitFor", noshort = true)
    val scriptReportEveryMs: ScallopOption[Int] = opt[Int]("scriptReportEveryMs", default = Some(0), noshort = true)
    val scriptStopMs: ScallopOption[Int] = opt[Int]("scriptStopMs", default = Some(0), noshort = true)
    verify()
  }

  private val journalLock = new Object

  /** appends a line to a journal; lines of runs in one server never interleave */
  def writeJournal(journal: Path, line: String): Unit = journalLock.synchronized {
    Files.write(journal, (line + "\n").getBytes(UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  }

  /** the lines of a journal so far; empty if nothing has been written yet */
  def readJournal(journal: Path): Seq[String] =
    if (Files.exists(journal)) Files.readAllLines(journal, UTF_8).asScala.toSeq else Seq.empty

  /** the longest a run waits for its `--scriptWaitFor` file, so that a failed test leaves no
    * verification behind that runs forever */
  val maxWaitMs: Long = 120000

  class ScriptedVerifier(override val reporter: Reporter) extends Verifier {
    private var _config: Config = _
    def config: Config = _config

    override def name: String = "scripted"
    override def version: String = "1.0"
    override def buildVersion: String = ""
    override def copyright: String = ""
    override def debugInfo(info: Seq[(String, Any)]): Unit = {}
    override def dependencies: Seq[Dependency] = Nil
    override def parseCommandLine(args: Seq[String]): Unit = { _config = new Config(args) }
    override def start(): Unit = {}
    override def stop(): Unit = {}

    private def journal(event: String): Unit =
      config.scriptJournal.toOption.foreach(file => writeJournal(Paths.get(file), s"${config.scriptName()} $event"))

    private def say(text: String): Unit = reporter report ConfigurationConfirmation(s"scripted: $text")

    /** lets `--scriptStopMs` pass, whatever interrupts arrive meanwhile */
    private def takeTimeToStop(): Unit = {
      val end = System.currentTimeMillis() + config.scriptStopMs()
      var interrupted = false
      while (System.currentTimeMillis() < end) {
        try Thread.sleep(math.max(1, end - System.currentTimeMillis())) catch { case _: InterruptedException => interrupted = true }
      }
      if (interrupted) Thread.currentThread().interrupt()
    }

    override def verify(program: Program): VerificationResult = {
      journal("started")
      try {
        config.scriptText.toOption.foreach(say)
        val padding = "x" * config.scriptMessageSize()
        (0 until config.scriptMessages()).foreach(i => say(s"message $i $padding".trim))
        if (config.scriptRunMs() > 0) Thread.sleep(config.scriptRunMs().toLong)
        config.scriptWaitFor.toOption.foreach { file =>
          say("waiting")
          val deadline = System.currentTimeMillis() + maxWaitMs
          var lastReport = System.currentTimeMillis()
          var reports = 0
          while (!Files.exists(Paths.get(file))) {
            if (System.currentTimeMillis() > deadline) throw new IllegalStateException(s"$file did not appear")
            Thread.sleep(10)
            if (config.scriptReportEveryMs() > 0 && System.currentTimeMillis() - lastReport >= config.scriptReportEveryMs()) {
              say(s"still waiting $reports")
              reports += 1
              lastReport = System.currentTimeMillis()
            }
          }
          say("released")
        }
      } catch {
        case e: Throwable =>
          takeTimeToStop()
          journal(s"aborted ${e.getClass.getName}")
          throw e
      }
      journal("finished")
      Success
    }
  }
}
