// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.nio.file.{Files, Path}

/** Tests of the LSP mode that know nothing about the server's code. Each server under test is a
  * process of its own, started the way the Viper IDE starts it, and [[LspClient]] plays the IDE:
  * it opens, edits and closes files, asks for verifications, and keeps what the server sends.
  *
  * Most tests pin behaviour the server has today. A test marked `wantedButNotYet` describes
  * behaviour that is wanted and that the server does not have yet.
  *
  * The tests of today's behaviour do what an IDE does, and they keep away from a few sequences
  * whose outcome today depends on timing: they let a parse end before they edit the file again
  * or ask for its verification, they stop or close a verification only once the backend runs,
  * and they leave a moment between a stop and the next `Verify` of the same file. What is wanted
  * for a `Verify` that follows a stop or another `Verify` at once has tests of its own.
  */
class LspBlackBoxTests extends BlackBoxSuite {

  import LspClient._

  /** The options of the servers here: as the IDE starts a server, plus room. A server with
    * default options can parse three files in its life, which has a test of its own below. */
  private val roomy = Seq("--maximumActiveJobs", "1000")

  private var ide: LspClient = _
  /** a second server and its IDE, for the tests of what is wanted: a `Verify` that is never
    * answered there does not count against the first one */
  private var ide2: LspClient = _

  private val verifying = "method m(x: Int) returns (y: Int)\n  ensures y == x\n{\n  y := x\n}\n"
  private val failing = verifying.replace("y == x", "y == x + 1")
  private val slowParse = s"--disableCaching --plugin ${SleepingPlugin.className}"

  override def beforeAll(): Unit = {
    super.beforeAll()
    val first = startServer("main", Seq("--serverMode", "LSP", "--singleClient") ++ roomy: _*)
    val second = startServer("second", Seq("--serverMode", "LSP", "--singleClient") ++ roomy: _*)
    ide = connect(first)
    ide2 = connect(second)
    // the first verification of a JVM takes seconds; no test should wait for it
    Seq(ide, ide2).foreach { client =>
      val file = write(s"warm-up-${client.hashCode}/warm_up.vpr", verifying)
      client.open(file, verifying)
      client.awaitQuiet()
      val from = client.mark
      client.verify(file)
      verdictOf(client, file, from, timeoutMs = 180000) shouldBe "Success"
      client.awaitQuiet()
    }
  }

  override def afterAll(): Unit = {
    Seq(ide, ide2).foreach(client => Option(client).foreach(_.close()))
    super.afterAll()
  }

  // ---------------------------------------------------------------------------------------------
  // helpers

  private def connect(server: ServerProcess): LspClient = {
    val client = new LspClient(server.port)
    client.initialize()
    client
  }

  /** a file of its own project: every test gets a directory of its own */
  private def project(name: String, text: String): Path = write(s"${name.replace(' ', '-')}/$name.vpr", text)

  /** opens a file and waits until the server has nothing more to say about that */
  private def open(client: LspClient, file: Path): Unit = {
    client.open(file, Files.readString(file))
    client.awaitQuiet()
  }

  /** edits a file and waits for the end of the parse that the edit starts */
  private def edit(client: LspClient, file: Path, text: String): Unit = {
    val from = client.mark
    client.edit(file, text)
    client.await("the start of the edit's parse", from) { case s: StateChange if s.state == "ConstructingAst" => s }
    client.awaitQuiet()
  }

  /** the verdict that answers the `Verify` of `file`; fails if the server says instead that the
    * verification was not started */
  private def verdictOf(client: LspClient, file: Path, from: Int, timeoutMs: Long = 60000): String =
    client.await(s"the answer to the Verify of ${file.getFileName}", from, timeoutMs) {
      case s: StateChange if s.isResult && s.uri.contains(uriOf(file)) => s.success
      case NotStarted(_, uri) if uri == uriOf(file) => fail(s"the verification of ${file.getFileName} was not started")
    }

  /** a diagnostic of `file` that contains `text`; waits for it */
  private def diagnosticOf(client: LspClient, file: Path, from: Int, text: String): String =
    client.await(s"a diagnostic for ${file.getFileName} with `$text`", from) {
      case d: Diagnostics if d.uri == uriOf(file) && d.messages.exists(_.contains(text)) => d.messages.find(_.contains(text)).get
    }

  /** the verdicts of all answers that `file` has got since `from` */
  private def answersOf(client: LspClient, file: Path, from: Int): Seq[String] =
    client.events(from).collect { case s: StateChange if s.isResult && s.uri.contains(uriOf(file)) => s.success }

  /** the diagnostics the IDE shows for `file`: the last ones the server has published */
  private def shownDiagnostics(client: LspClient, file: Path): Seq[String] =
    client.events().collect { case d: Diagnostics if d.uri == uriOf(file) => d.messages }.lastOption.getOrElse(Seq.empty)

  /** `Verify` with the scripted backend, which does not end before the file `release` exists */
  private def verifyScripted(client: LspClient, file: Path, name: String, journal: Path, release: Path, more: String = ""): Unit =
    client.verify(file, backend = ScriptedBackend.className,
      customArgs = s"--disableCaching --scriptName $name --scriptJournal ${quoted(journal)} --scriptWaitFor ${quoted(release)} $more".trim)

  private def parseStarted(client: LspClient, from: Int): Unit =
    client.await("the start of the parse", from) { case s: StateChange if s.state == "ConstructingAst" => s }

  /** the `Ready` that the server sends for a file whose verification was stopped or closed */
  private def stoppedNotice(client: LspClient, file: Path, from: Int): StateChange =
    client.await(s"the notice that ${file.getFileName} is no longer verified", from) {
      case s: StateChange if s.state == "Ready" && s.verificationCompleted == 0.0 && s.uri.contains(uriOf(file)) => s
    }

  // ---------------------------------------------------------------------------------------------

  "A ViperServer in LSP mode" when {

    "the IDE sends `Verify`" should {

      "answer `Success` for a file that verifies, and leave it without diagnostics" in {
        val file = project("verifies", verifying)
        open(ide, file)
        val from = ide.mark
        ide.verify(file)
        verdictOf(ide, file, from) shouldBe "Success"
        ide.awaitQuiet()
        shownDiagnostics(ide, file) shouldBe empty
      }

      "answer `VerificationFailed` for a file that does not verify, and publish the error" in {
        val file = project("fails", failing)
        open(ide, file)
        val from = ide.mark
        ide.verify(file)
        verdictOf(ide, file, from) shouldBe "VerificationFailed"
        diagnosticOf(ide, file, from, "Postcondition of m might not hold")
        ide.awaitQuiet()
        shownDiagnostics(ide, file).size shouldBe 1
      }

      "answer `ParsingFailed` for a file with a syntax error, and publish the error" in {
        val file = project("syntax error", "method m( {\n")
        open(ide, file)
        val from = ide.mark
        ide.verify(file)
        verdictOf(ide, file, from) shouldBe "ParsingFailed"
        diagnosticOf(ide, file, from, "Parse error")
      }

      "answer `TypecheckingFailed` for a file with a type error, and publish the error" in {
        val file = project("type error", "method m() {\n  var x: Int := true\n}\n")
        open(ide, file)
        val from = ide.mark
        ide.verify(file)
        // the verdict and the diagnostic, in either order
        verdictOf(ide, file, from) shouldBe "TypecheckingFailed"
        diagnosticOf(ide, file, from, "incompatible type")
      }

      "answer a file with a consistency error with a failure, and publish the error" in {
        val file = project("consistency error",
          "method two() returns (a: Int, b: Int)\nmethod m() {\n  var x: Int\n  x, x := two()\n}\n")
        open(ide, file)
        val from = ide.mark
        ide.verify(file)
        // today's verdict for a file that has not been parsed before; it is not a type error's
        verdictOf(ide, file, from) shouldBe "VerificationFailed"
        diagnosticOf(ide, file, from, "Consistency error")
      }

      "verify a file with an import, report an error typed into the imported file for that file, and verify again once it is taken out" in {
        val lib = write("import/lib.vpr", "function f(): Int { 1 }\n")
        val root = write("import/root.vpr", "import \"lib.vpr\"\nmethod m() {\n  assert f() == 1\n}\n")
        open(ide, root)
        var from = ide.mark
        ide.verify(root)
        verdictOf(ide, root, from) shouldBe "Success"
        ide.awaitQuiet()

        open(ide, lib)
        from = ide.mark
        edit(ide, lib, "function f(): Int { true }\n")
        diagnosticOf(ide, lib, from, "incompatible type")
        shownDiagnostics(ide, root) shouldBe empty

        from = ide.mark
        edit(ide, lib, "function f(): Int { 1 }\n")
        shownDiagnostics(ide, lib) shouldBe empty
        from = ide.mark
        ide.verify(root)
        verdictOf(ide, root, from) shouldBe "Success"
      }

      "answer two `Verify` that come 10 ms apart once: the second replaces the first" in wantedButNotRun {
        // Today both are answered, in each of 16 runs. In about half of them the first answer was
        // `Success` for this file, which does not verify, and in about half a diagnostic of the
        // first verification ("Verification aborted exceptionally") stayed next to the right one.
        // Not run: a `Verify` that arrives before the one before it has started its jobs finds
        // nothing to replace, and whether it does is a matter of milliseconds.
        val file = project("twice", failing)
        open(ide2, file)
        val from = ide2.mark
        ide2.verify(file)
        Thread.sleep(10)
        ide2.verify(file)
        verdictOf(ide2, file, from)
        ide2.awaitQuiet(quietMs = 2000)
        answersOf(ide2, file, from) shouldBe Seq("VerificationFailed")
        val shown = shownDiagnostics(ide2, file)
        shown.size shouldBe 1
        shown.head should include("Postcondition of m might not hold")
      }
    }

    "a verification is running" should {

      "stop it on `StopVerification`, and say that the file is no longer verified" in {
        val file = project("stopped", verifying)
        val journal = newFile("journal")
        open(ide, file)
        val from = ide.mark
        verifyScripted(ide, file, "S", journal, newFile("never"))
        eventually("the backend runs")(journalOf(journal) == Seq("S started"))
        ide.stop(file) shouldBe Some(true)
        ide.stoppedVerify(file)
        stoppedNotice(ide, file, from)
        eventually("the backend is interrupted")(journalOf(journal) == Seq("S started", "S aborted java.lang.InterruptedException"))
        ide.awaitQuiet()
        ide.events(from).collect { case s: StateChange if s.isResult => s } shouldBe empty
      }

      "stop it when its file is closed" in {
        val file = project("closed", verifying)
        val journal = newFile("journal")
        open(ide, file)
        val from = ide.mark
        verifyScripted(ide, file, "C", journal, newFile("never"))
        eventually("the backend runs")(journalOf(journal) == Seq("C started"))
        ide.close(file)
        ide.stoppedVerify(file)
        eventually("the backend is interrupted")(journalOf(journal) == Seq("C started", "C aborted java.lang.InterruptedException"))
        ide.awaitQuiet()
        ide.events(from).collect { case s: StateChange if s.isResult => s } shouldBe empty
      }
    }

    "a verification has been stopped or replaced" should {

      "answer the `Verify` that replaces a running verification of the same file, and only that one" in wantedButNotYet {
        // today the end of the replaced verification is taken for an end of the new one: the IDE
        // gets a second answer, and it can be `Success` for a file that does not verify
        val file = project("replaced", failing)
        val journal = newFile("journal")
        open(ide2, file)
        val from = ide2.mark
        verifyScripted(ide2, file, "R", journal, newFile("never"), "--scriptStopMs 400")
        eventually("the backend runs")(journalOf(journal) == Seq("R started"))
        ide2.verify(file)
        eventually("the replaced backend has ended")(journalOf(journal).size == 2)
        verdictOf(ide2, file, from)
        ide2.awaitQuiet(quietMs = 1500)
        answersOf(ide2, file, from) shouldBe Seq("VerificationFailed")
        val shown = shownDiagnostics(ide2, file)
        shown.size shouldBe 1
        shown.head should include("Postcondition of m might not hold")
      }

      "answer a `Verify` that follows a `StopVerification` at once with its own verdict only" in wantedButNotYet {
        // today what the stopped verification still sends is taken for the new one's
        val file = project("verified again", verifying)
        val journal = newFile("journal")
        open(ide2, file)
        verifyScripted(ide2, file, "V", journal, newFile("never"), "--scriptStopMs 400")
        eventually("the backend runs")(journalOf(journal) == Seq("V started"))
        ide2.stop(file) shouldBe Some(true)
        val from = ide2.mark
        ide2.verify(file)
        eventually("the stopped backend has ended")(journalOf(journal).size == 2)
        verdictOf(ide2, file, from)
        ide2.awaitQuiet(quietMs = 1500)
        answersOf(ide2, file, from) shouldBe Seq("Success")
        shownDiagnostics(ide2, file) shouldBe empty
      }
    }

    "a file is still being parsed for its verification" should {

      "give the verdict of a corrected text that is verified after a stop" in {
        // the first text has a type error, and nothing of its parse may reach the second run
        val wrong = "// sleep 3000\nmethod m() {\n  var x: Int := true\n}\n"
        val file = project("corrected", wrong)
        open(ide, file)
        var from = ide.mark
        ide.verify(file, customArgs = slowParse)
        parseStarted(ide, from)
        Thread.sleep(500)
        ide.stop(file) shouldBe Some(true)
        ide.stoppedVerify(file)
        Thread.sleep(500)

        from = ide.mark
        ide.verify(file, customArgs = slowParse, text = Some(verifying))
        verdictOf(ide, file, from) shouldBe "Success"
        ide.awaitQuiet()
        shownDiagnostics(ide, file) shouldBe empty
      }

      "not verify it when it is closed" in {
        val file = project("closed while parsed", "// sleep 3000\n" + verifying)
        val journal = newFile("journal")
        val release = Files.createFile(newFile("release"))
        open(ide, file)
        val from = ide.mark
        verifyScripted(ide, file, "P", journal, release, s"--plugin ${SleepingPlugin.className}")
        parseStarted(ide, from)
        Thread.sleep(500)
        ide.close(file)
        ide.stoppedVerify(file)
        Thread.sleep(4500) // the parse has ended by now
        ide.awaitQuiet()
        journalOf(journal) shouldBe empty
        ide.events(from).collect { case s: StateChange if s.isResult => s } shouldBe empty
      }

      "give another file its own verdict when that one is verified instead" in {
        val first = project("given up", "// sleep 3000\n" + failing)
        val other = project("verified instead", verifying)
        val journal = newFile("journal")
        val release = Files.createFile(newFile("release"))
        open(ide, first)
        open(ide, other)
        val from = ide.mark
        verifyScripted(ide, first, "G", journal, release, s"--plugin ${SleepingPlugin.className}")
        parseStarted(ide, from)
        Thread.sleep(500)
        ide.verify(other)
        ide.supersededVerify(first)
        verdictOf(ide, other, from) shouldBe "Success"
        ide.awaitQuiet(quietMs = 1500)
        shownDiagnostics(ide, other) shouldBe empty
        // the first file's verification is given up for the other one
        journalOf(journal) shouldBe empty
        ide.events(from).collect { case s: StateChange if s.isResult && s.uri.contains(uriOf(first)) => s } shouldBe empty
      }

      "answer a `StopVerification` within a second" in wantedButNotYet {
        // today the stop is answered when the parser has returned
        val file = project("stopped while parsed", "// sleep 4000\n" + verifying)
        open(ide2, file)
        val from = ide2.mark
        ide2.verify(file, customArgs = slowParse)
        parseStarted(ide2, from)
        Thread.sleep(500)
        try {
          ide2.stop(file, timeoutMs = 1000) shouldBe Some(true)
        } finally {
          Thread.sleep(4000)
          ide2.awaitQuiet()
        }
      }
    }

    "a `Verify` cannot be carried out" should {

      "answer it when the parser throws" in wantedButNotYet {
        // today: two diagnostics and no answer; the IDE waits for its timeout
        val file = project("parser throws", verifying)
        open(ide2, file)
        val from = ide2.mark
        ide2.verify(file, customArgs = "--disableCaching --plugin no.such.Plugin")
        val answer = ide2.await("the answer", from, timeoutMs = 10000) {
          case s: StateChange if s.isResult && s.uri.contains(uriOf(file)) => s
        }
        answer.success shouldBe "Error"
        answer.error should not be empty // the reason, which the IDE shows
        // the error itself has been published by then
        ide2.events(from).takeWhile(_ != answer).collect { case d: Diagnostics if d.uri == uriOf(file) && d.items.nonEmpty => d } should not be empty
        ide2.awaitQuiet()
        ide2.verifiesAndAnswers(uriOf(file)) shouldBe ((1, 1))
      }

      "say that the verification was not started when the file is no longer on disk, and leave its diagnostics" in wantedButNotYet {
        // today: the notice that the parse starts, and then nothing
        val file = project("gone from disk", "method m() {\n  var x: Int := true\n}\n")
        open(ide2, file)
        var from = ide2.mark
        ide2.verify(file)
        verdictOf(ide2, file, from) shouldBe "TypecheckingFailed"
        diagnosticOf(ide2, file, from, "incompatible type")
        ide2.awaitQuiet()

        Files.delete(file)
        from = ide2.mark
        ide2.verify(file)
        ide2.await("the refusal", from, timeoutMs = 10000) { case n: NotStarted if n.uri == uriOf(file) => n }
        ide2.awaitQuiet()
        answersOf(ide2, file, from) shouldBe empty
        shownDiagnostics(ide2, file).exists(_.contains("incompatible type")) shouldBe true
      }

      "carry out a later `Verify` of a file whose verification was refused because the server was busy" in wantedButNotYet {
        // today every later `Verify` of that file is refused as well, until the file is edited
        val server = startServer("one-job", "--serverMode", "LSP", "--maximumActiveJobs", "1")
        val busy = connect(server)
        val other = connect(server)
        try {
          val long = project("keeps the server busy", verifying)
          val file = project("refused", verifying)
          val journal = newFile("journal")
          val release = newFile("release")
          open(busy, long)
          verifyScripted(busy, long, "B", journal, release)
          eventually("the backend runs")(journalOf(journal) == Seq("B started"))

          open(other, file)
          var from = other.mark
          other.verify(file)
          other.await("the refusal", from, timeoutMs = 20000) { case n: NotStarted if n.uri == uriOf(file) => n }

          Files.createFile(release)
          eventually("the server is free again")(journalOf(journal) == Seq("B started", "B finished"))
          busy.awaitQuiet()
          from = other.mark
          other.verify(file)
          verdictOf(other, file, from, timeoutMs = 20000) shouldBe "Success"
        } finally {
          busy.close()
          other.close()
        }
      }
    }

    "it has default options" should {

      "parse a fourth file after three others were opened, edited and closed" in wantedButNotYet {
        // today a file that was parsed and never verified keeps one of three places for good
        val server = startServer("default", "--serverMode", "LSP", "--singleClient")
        val client = connect(server)
        try {
          for (i <- 1 to 4) {
            val file = project(s"file $i", verifying)
            open(client, file)
            val from = client.mark
            client.edit(file, verifying + "\n")
            client.await(s"the result of the parse of file $i", from, timeoutMs = if (i == 1) 120000 else 10000) {
              case u: Unhandled if u.msgType == "ast_construction_result" => u
            }
            client.awaitQuiet()
            client.close(file)
            client.awaitQuiet()
          }
        } finally {
          client.close()
        }
      }
    }

    "it has three threads" should {

      "report that a verification runs, and answer a stop, while it runs" in wantedButNotYet {
        // today neither arrives before the verification has ended by itself. Three threads are
        // the default on a machine with up to three cores.
        val server = startServer("three-threads", "--serverMode", "LSP", "--singleClient", "--maximumActiveJobs", "1000", "--nThreads", "3")
        val client = connect(server)
        val release = newFile("release")
        try {
          val file = project("three threads", verifying)
          val journal = newFile("journal")
          open(client, file)
          val from = client.mark
          verifyScripted(client, file, "T", journal, release)
          eventually("the backend runs", 120000)(journalOf(journal) == Seq("T started"))
          client.await("the notice that the verification runs", from, timeoutMs = 5000) {
            case s: StateChange if s.state == "VerificationRunning" => s
          }
          client.stop(file, timeoutMs = 5000) shouldBe Some(true)
          journalOf(journal) should not contain "T finished"
        } finally {
          Files.createFile(release)
          Thread.sleep(500)
          client.close()
        }
      }
    }

    "the IDE goes away" should {

      "end with exit code 0, also while it verifies" in {
        val server = startServer("left-alone", Seq("--serverMode", "LSP", "--singleClient") ++ roomy: _*)
        val client = connect(server)
        val file = project("left alone", verifying)
        val journal = newFile("journal")
        open(client, file)
        verifyScripted(client, file, "A", journal, newFile("never"))
        eventually("the backend runs", 120000)(journalOf(journal) == Seq("A started"))
        client.close()
        server.awaitExit(20000) shouldBe Some(0)
      }
    }

    "the IDE goes away while a large file's parse result is being stored" should {

      "end with exit code 0" in wantedButNotYet {
        // today the server's shutdown gives up after a second when the result is still being
        // stored then: a TimeoutException on standard output, and the process stays
        val server = startServer("left-while-storing", Seq("--serverMode", "LSP", "--singleClient") ++ roomy: _*)
        val client = connect(server)
        val text = (1 to 6000).map(i => s"method m$i(x: Int) returns (y: Int)\n  ensures y == x\n{\n  y := x\n}\n").mkString
        val file = project("large", text)
        open(client, file)
        val from = client.mark
        client.edit(file, text + "\n")
        // storing a parse result begins with this request to the IDE
        client.await("the request to set the project up", from, timeoutMs = 180000) {
          case r: ServerRequest if r.method == "SetupProject" => r
        }
        client.close()
        try server.awaitExit(20000) shouldBe Some(0) finally server.destroy()
      }
    }

    "the suite has run" should {

      "have answered every `Verify` exactly once, and those that the tests stopped or replaced not at all" in {
        // an answer is a `Ready` that says the verification is completed, or `VerificationNotStarted`
        ide.awaitQuiet(quietMs = 1500)
        ide.verifiesAndAnswers.foreach { case (uri, (sent, answers)) =>
          val givenUp = ide.givenUp(uri)
          withClue(s"$uri: $sent sent, $givenUp of them stopped or replaced, $answers answered. ") {
            answers shouldBe sent - givenUp
          }
        }
      }
    }
  }
}
