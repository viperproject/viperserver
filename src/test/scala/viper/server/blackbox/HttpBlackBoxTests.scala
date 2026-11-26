// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.io.{ByteArrayOutputStream, IOException}
import java.net.{Inet4Address, InetSocketAddress, NetworkInterface, Socket}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import com.google.gson.JsonParser

import scala.jdk.CollectionConverters._

/** Tests of the HTTP mode that know nothing about the server's code. Each server under test is a
  * process of its own, started with command-line options like any ViperServer, and the tests talk
  * to the port it prints. They send what `client.py` of `viperproject/viper_client` sends and look
  * at the bytes that come back.
  *
  * The suite describes what a client of the HTTP mode can rely on, so that the code behind the
  * port can be replaced. Most tests pin behaviour the server has today. A test marked
  * `wantedButNotYet` or `wantedButNotRun` describes behaviour that is wanted and that the server
  * does not have yet; each of them says what the server does today.
  *
  * Verifications that have to last as long as a test wants, or whose running time a test needs
  * to know, use [[ScriptedBackend]] in the place of a real verifier.
  */
class HttpBlackBoxTests extends BlackBoxSuite {

  import RawHttp._

  /** the server most tests use. It is given more room than the default: a server with default
    * options accepts three requests in its life (issue #375). */
  private var mainServer: ServerProcess = _
  /** a server that accepts one request at a time */
  private var smallServer: ServerProcess = _
  /** two servers with default options */
  private var defaultServerA: ServerProcess = _
  private var defaultServerB: ServerProcess = _

  private var verified: Path = _
  private var failing: Path = _
  private var parseError: Path = _
  private var typeError: Path = _

  /** opened when the suite starts and not touched before the suite's last test */
  private var idleConnection: Connection = _
  private var silentStream: OpenStream = _
  private var silentRelease: Path = _
  private var idleSince: Long = _

  private def port: Int = mainServer.port

  override def beforeAll(): Unit = {
    super.beforeAll()
    verified = write("verified.vpr", "method m(x: Int) returns (y: Int)\n  ensures y == x\n{\n  y := x\n}\n")
    failing = write("failing.vpr", "method m(x: Int) returns (y: Int)\n  ensures y == x + 1\n{\n  y := x\n}\n")
    parseError = write("parse_error.vpr", "method m( {\n")
    typeError = write("type_error.vpr", "method m() {\n  var x: Int := true\n}\n")

    mainServer = startServer("main", "--maximumActiveJobs", "1000")
    smallServer = startServer("small", "--maximumActiveJobs", "1")
    defaultServerA = startServer("default-a")
    defaultServerB = startServer("default-b")
    Seq(mainServer, smallServer, defaultServerA, defaultServerB).foreach(_.port) // waits for each start-up line

    warmUp(mainServer, silicon(verified))
    warmUp(mainServer, scripted(typeError))

    idleConnection = new Connection(port)
    idleConnection.send("GET", "/cache/flush")
    idleConnection.readResponse().status shouldBe 200
    silentRelease = newFile("release")
    silentStream = openStream(port, submit(port, scripted(verified, "--scriptWaitFor", quoted(silentRelease))))
    silentStream.readUntil("scripted: waiting")
    idleSince = System.currentTimeMillis()
  }

  override def afterAll(): Unit = {
    Option(idleConnection).foreach(_.close())
    Option(silentStream).foreach(_.close())
    super.afterAll()
  }

  // ---------------------------------------------------------------------------------------------
  // helpers

  /** the `arg` of a request for Silicon as `client.py` builds it: backend, options, quoted path */
  private def silicon(file: Path, options: String*): String =
    (Seq("silicon", "--disableCaching") ++ options :+ quoted(file)).mkString(" ")

  private def scripted(file: Path, options: String*): String =
    (Seq(ScriptedBackend.className, "--disableCaching") ++ options :+ quoted(file)).mkString(" ")

  /** `POST /verify` with the body and the headers of `client.py` */
  private def postVerify(port: Int, arg: String): Response = postBody(port, s"""{"arg": ${jsonString(arg)}}""")

  private def postBody(port: Int, body: String, contentType: Option[String] = Some("application/json")): Response =
    request(port, "POST", "/verify", clientHeaders ++ contentType.map("Content-Type" -> _), Some(body.getBytes(UTF_8)))

  private val Accepted = """\{"ast_id":(\d+),"id":(\d+)\}""".r

  /** posts a request that the server has to accept, and returns its id */
  private def submit(port: Int, arg: String): Int = {
    val reply = postVerify(port, arg)
    reply.status shouldBe 200
    reply.text match {
      case Accepted(astId, id) =>
        astId shouldBe id
        id.toInt
      case other => fail(s"the request was not accepted: $other")
    }
  }

  /** `GET /verify/<id>`, read to its end */
  private def fetch(port: Int, id: Int, timeoutMs: Int = 60000): Response = get(port, s"/verify/$id", timeoutMs)

  /** a complete result stream's lines */
  private def linesOf(stream: Response): Seq[String] = {
    stream.status shouldBe 200
    stream.header("Transfer-Encoding").map(_.toLowerCase) shouldBe Some("chunked")
    stream.header("Content-Type") shouldBe Some("application/json")
    stream.ending shouldBe Complete
    stream.text.split("\n", -1).toSeq
  }

  private def doesNotExist(id: Int): String = s"""{"msg":"The verification job #$id does not exist."}"""

  private def messageOfType(msgType: String): String = s""","msg_type":"$msgType"}"""

  private val firstLine = """{"msg_body":{"text":"Viper AST Constructor"},"msg_type":"copyright_report"}"""

  private def overallResult(status: String, verifier: String): String =
    s""""kind":"overall","status":"$status","verifier":"$verifier"},"msg_type":"verification_result"}"""

  /** the running times a stream reports differ from run to run */
  private def withoutTimes(text: String): String = text.replaceAll(""""time":\d+""", """"time":0""")

  /** a string as the server's JSON printer writes it into a stream */
  private def inJson(s: String): String = s.flatMap {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case c => c.toString
  }

  /** A result stream that is read piece by piece. */
  private final class OpenStream(connection: Connection, val head: Head) {
    private val received = new ByteArrayOutputStream()

    def soFar: String = new String(received.toByteArray, UTF_8)

    /** reads until the stream contains `text`; fails if the stream ends first */
    def readUntil(text: String, timeoutMs: Int = 30000): Unit = {
      while (!soFar.contains(text)) {
        connection.readChunk(timeoutMs) match {
          case Some(chunk) => received.write(chunk)
          case None => fail(s"the stream ended without `$text`:\n$soFar")
        }
      }
    }

    /** reads to the end and returns the whole body and how it ended */
    def finish(timeoutMs: Int = 30000): (String, Ending) = {
      val rest = connection.readBody(head, timeoutMs = timeoutMs)
      received.write(rest.body)
      (soFar, rest.ending)
    }

    def close(): Unit = connection.close()
  }

  /** sends `GET /verify/<id>` and reads the status line and the headers */
  private def openStream(port: Int, id: Int): OpenStream = {
    val connection = new Connection(port)
    connection.send("GET", s"/verify/$id")
    new OpenStream(connection, connection.readHead(60000))
  }

  /** The first verification of a server. Today a `GET /verify/<id>` is answered with 503 when the
    * file's parse takes longer than 5 s, which only the first parse of a cold JVM does. This is
    * the one place that tries again; every test after it takes a 503 as a failure.
    */
  private def warmUp(server: ServerProcess, arg: String): Response = {
    val id = submit(server.port, arg)
    val deadline = System.currentTimeMillis() + 120000
    var reply = fetch(server.port, id)
    while (reply.status == 503 && System.currentTimeMillis() < deadline) reply = fetch(server.port, id)
    reply.status shouldBe 200
    reply
  }

  // ---------------------------------------------------------------------------------------------

  "A ViperServer in HTTP mode" when {

    "it starts" should {

      "print one line that names the port it chose, and answer on that port" in {
        mainServer.output.count(line => ServerProcess.httpStartUpLine.matches(line)) shouldBe 1
        get(port, "/cache/flush").status shouldBe 200
        request(port, "GET", "/cache/flush", host = "localhost").status shouldBe 200
      }

      "use the port it is given" in {
        val free = { val s = new java.net.ServerSocket(0); try s.getLocalPort finally s.close() }
        val server = startServer("port", "-p", free.toString)
        server.port shouldBe free
        get(free, "/cache/flush").status shouldBe 200
        server.destroy()
      }

      "listen on the loopback interface only" in {
        val others = NetworkInterface.getNetworkInterfaces.asScala.toSeq
          .filter(i => i.isUp && !i.isLoopback)
          .flatMap(_.getInetAddresses.asScala)
          .collect { case a: Inet4Address => a }
        if (others.isEmpty) cancel("this machine has no other interface")
        others.foreach { address =>
          val socket = new Socket()
          try {
            intercept[IOException](socket.connect(new InetSocketAddress(address, port), 2000))
          } finally {
            socket.close()
          }
        }
      }
    }

    "it gets a `POST /verify`" should {

      "answer with the ids of the new request" in {
        val reply = postVerify(port, silicon(verified))
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.header("Content-Length") shouldBe Some(reply.body.length.toString)
        reply.text should fullyMatch regex Accepted
      }

      "count the ids up by one, and use up no id for a request it refuses" in {
        val first = submit(port, scripted(verified))
        postVerify(port, silicon(dir.resolve("no_such_file.vpr"))).text shouldBe """{"msg":"File not found"}"""
        postBody(port, "{}").status shouldBe 400
        postBody(port, s"""{"arg": ${jsonString(silicon(verified))}}""", contentType = Some("text/plain")).status shouldBe 415
        submit(port, scripted(verified)) shouldBe first + 1
      }

      "refuse a file that does not exist, a directory, and a request without a file" in {
        for (arg <- Seq(silicon(dir.resolve("no_such_file.vpr")), silicon(dir), "silicon")) {
          val reply = postVerify(port, arg)
          reply.status shouldBe 200
          reply.header("Content-Type") shouldBe Some("application/json")
          reply.text shouldBe """{"msg":"File not found"}"""
        }
      }

      "refuse a request without a backend" in {
        val reply = postVerify(port, quoted(verified))
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.text shouldBe """{"msg":"Invalid arguments for backend."}"""
      }

      "accept a body only as `application/json`" in {
        // This check keeps a web page from starting a verification: a browser sends a `text/plain`
        // or form-encoded POST to localhost from any site without asking first.
        val body = s"""{"arg": ${jsonString(silicon(dir.resolve("no_such_file.vpr")))}}"""
        def refusal(contentType: String) = s"The request's Content-Type $contentType is not supported. Expected:\napplication/json"
        for ((sent, named) <- Seq(
          None -> "[application/octet-stream]",
          Some("text/plain") -> "[text/plain]",
          Some("application/x-www-form-urlencoded") -> "[application/x-www-form-urlencoded]",
          Some("text/json") -> "[text/json]")) {
          val reply = postBody(port, body, sent)
          withClue(s"Content-Type $sent: ") {
            reply.status shouldBe 415
            reply.header("Content-Type") shouldBe Some("text/plain; charset=UTF-8")
            reply.text shouldBe refusal(named)
          }
        }
        for (sent <- Seq("application/json; charset=utf-8", "Application/JSON")) {
          withClue(s"Content-Type $sent: ") {
            postBody(port, body, Some(sent)).text shouldBe """{"msg":"File not found"}"""
          }
        }
      }

      "answer 400 and say what is wrong with a body that is not the expected JSON" in {
        for ((body, complaint) <- Seq(
          """{"arg": "silicon""" -> "Unexpected end-of-input at input index 16 (line 1, position 17), expected '\"':\n{\"arg\": \"silicon\n                ^\n",
          "" -> "Unexpected end-of-input at input index 0 (line 1, position 1), expected JSON Value:\n\n^\n",
          "{}" -> "Object is missing required member 'arg'",
          """{"arg": 5}""" -> "Expected String as JsString, but got 5")) {
          val reply = postBody(port, body)
          withClue(s"body `$body`: ") {
            reply.status shouldBe 400
            reply.header("Content-Type") shouldBe Some("text/plain; charset=UTF-8")
            reply.text shouldBe s"The request content was malformed:\n$complaint"
          }
        }
      }

      "answer 500 to an empty `arg`" in {
        for (arg <- Seq("", "   ")) {
          val reply = postVerify(port, arg)
          reply.status shouldBe 500
          reply.text shouldBe "There was an internal server error."
        }
      }

      "accept a body of 8 MB and refuse a larger one with 413" in {
        val limit = 8 * 1024 * 1024
        val headers = clientHeaders :+ jsonContentType
        def bodyOf(size: Int): Seq[Array[Byte]] = {
          val start = s"""{"arg": ${jsonString(silicon(dir.resolve("no_such_file.vpr")))}"""
          val bytes = (start + " " * (size - start.length - 1) + "}").getBytes(UTF_8)
          bytes.length shouldBe size
          bytes.grouped(64 * 1024).toSeq
        }
        def post(size: Int, chunked: Boolean): Response = {
          val connection = new Connection(port)
          try {
            connection.sendLarge("POST", "/verify", headers, bodyOf(size), chunked)
            connection.readResponse()
          } finally {
            connection.close()
          }
        }
        val tooLarge = "The request content was malformed:\nEntityStreamSizeException: incoming entity size (%s) " +
          s"exceeded size limit ($limit bytes)! This may have been a parser limit (set via " +
          "`akka.http.[server|client].parsing.max-content-length`), a decoder limit (set via " +
          "`akka.http.routing.decode-max-size`), or a custom limit set with `withSizeLimit`."

        post(limit, chunked = false).text shouldBe """{"msg":"File not found"}"""
        post(limit, chunked = true).text shouldBe """{"msg":"File not found"}"""

        val announced = post(limit + 1, chunked = false)
        announced.status shouldBe 413
        announced.text shouldBe tooLarge.format((limit + 1).toString)
        // a chunked body does not say how long it is
        val streamed = post(limit + 1, chunked = true)
        streamed.status shouldBe 413
        streamed.text shouldBe tooLarge.format("while streaming")
      }
    }

    "it gets a request for something else" should {

      "answer 404 to a path it does not know and to an id that is not a non-negative integer" in {
        for (path <- Seq("/", "/nonexistent", "/verify/", "/verify/0/", "/verify/0/extra", "/verifyfoo", "/VERIFY/0",
          "/exit/", "/cache", "/cache/flush/", "/ast", "/discard",
          "/verify/-1", "/verify/abc", "/verify/1.5", "/verify/99999999999", "/discard/-1", "/ast/abc")) {
          val reply = get(port, path)
          withClue(s"GET $path: ") {
            reply.status shouldBe 404
            reply.header("Content-Type") shouldBe Some("text/plain; charset=UTF-8")
            reply.text shouldBe "The requested resource could not be found."
          }
        }
      }

      "read an id as a number" in {
        get(port, "/verify/007").text shouldBe doesNotExist(7)
        get(port, "/verify/%37").text shouldBe doesNotExist(7)
        get(port, "/verify/2147483647").text shouldBe doesNotExist(2147483647)
      }

      "say that an id it does not know does not exist" in {
        for (path <- Seq("/verify/12345", "/discard/12345", "/ast/12345")) {
          val reply = get(port, path)
          withClue(s"GET $path: ") {
            reply.status shouldBe 200
            reply.header("Content-Type") shouldBe Some("application/json")
            reply.text shouldBe doesNotExist(12345)
          }
        }
      }

      "answer 405 to any other method than the one a path is for, and do nothing" in {
        val id = submit(port, scripted(verified))
        for ((method, path, allowed) <- Seq(
          ("POST", s"/verify/$id", "GET"), ("DELETE", s"/verify/$id", "GET"), ("PUT", s"/verify/$id", "GET"),
          ("POST", s"/discard/$id", "GET"), ("DELETE", s"/discard/$id", "GET"),
          ("POST", "/exit", "GET"), ("DELETE", "/exit", "GET"), ("POST", "/cache/flush", "GET"),
          ("GET", "/verify", "POST"), ("PUT", "/verify", "POST"), ("DELETE", "/verify", "POST"))) {
          val reply = request(port, method, path)
          withClue(s"$method $path: ") {
            reply.status shouldBe 405
            reply.header("Allow") shouldBe Some(allowed)
            reply.header("Content-Type") shouldBe Some("text/plain; charset=UTF-8")
            reply.text shouldBe s"HTTP method not allowed, supported methods: $allowed"
          }
        }
        val head = request(port, "HEAD", s"/verify/$id")
        head.status shouldBe 405
        head.header("Allow") shouldBe Some("GET")
        // none of them has fetched or discarded the request, and the server is still there:
        linesOf(fetch(port, id)).last should endWith(overallResult("success", "scripted"))
      }

      "flush the cache on `GET /cache/flush`" in {
        val reply = get(port, "/cache/flush")
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.text shouldBe """{"msg":"The cache has been flushed successfully."}"""
      }
    }

    "it gets a `GET /verify/<id>`" should {

      "send the result as a chunked stream of JSON lines: the parse's, then the backend's, then the overall result" in {
        // A scripted backend, because a real one decides itself how many lines it writes and in
        // which order. Its text has what a JSON printer can get wrong.
        val text = """a "quoted" word, a \ and ü€"""
        val id = submit(port, scripted(verified, "--scriptText", s"'$text'", "--scriptMessages", "2"))
        val stream = fetch(port, id)
        stream.status shouldBe 200
        stream.header("Transfer-Encoding").map(_.toLowerCase) shouldBe Some("chunked")
        stream.header("Content-Type") shouldBe Some("application/json")
        stream.header("Content-Length") shouldBe None
        stream.ending shouldBe Complete

        // UTF-8, one JSON object per line; the newline separates two messages, so there is none
        // at the end
        new String(stream.body, UTF_8).getBytes(UTF_8) shouldBe stream.body
        stream.text should not endWith "\n"
        val lines = withoutTimes(stream.text).split("\n", -1).toSeq
        lines.foreach { line =>
          val message = JsonParser.parseString(line)
          withClue(s"$line: ") {
            message.isJsonObject shouldBe true
            message.getAsJsonObject.keySet.asScala shouldBe Set("msg_body", "msg_type")
          }
        }

        // The lines whose text this test decides are compared to the byte. What the parse reports
        // about the program is not: only that it comes first, and that it ends well.
        val ofBackend = Seq(
          s"""{"msg_body":{"text":"scripted: ${inJson(text)}"},"msg_type":"configuration_confirmation"}""",
          """{"msg_body":{"text":"scripted: message 0"},"msg_type":"configuration_confirmation"}""",
          """{"msg_body":{"text":"scripted: message 1"},"msg_type":"configuration_confirmation"}""",
          """{"msg_body":{"details":{"time":0},"kind":"overall","status":"success","verifier":"scripted"},"msg_type":"verification_result"}"""
        )
        val parsed = """{"msg_body":{"details":{"time":0},"status":"success"},"msg_type":"ast_construction_result"}"""
        lines.head shouldBe firstLine
        lines.takeRight(ofBackend.size) shouldBe ofBackend
        lines.count(_ == parsed) shouldBe 1
        lines.indexOf(parsed) should be < lines.size - ofBackend.size
      }

      "send each line when it is produced, not when the verification has ended" in {
        val journal = newFile("journal")
        val release = newFile("release")
        val id = submit(port, scripted(verified, "--scriptName", "L", "--scriptJournal", quoted(journal), "--scriptWaitFor", quoted(release)))
        val stream = openStream(port, id)
        try {
          stream.head.status shouldBe 200
          // these lines arrive while the verification cannot end: it waits for the file
          stream.readUntil(firstLine)
          stream.readUntil("scripted: waiting")
          journalOf(journal) shouldBe Seq("L started")
          Files.createFile(release)
          val (text, ending) = stream.finish()
          ending shouldBe Complete
          text.split("\n").last should endWith(overallResult("success", "scripted"))
          journalOf(journal) shouldBe Seq("L started", "L finished")
        } finally {
          stream.close()
        }
      }

      "send the first byte only when the file has been parsed" in {
        val slow = write("slow_parse_2s.vpr", "// sleep 2000\nmethod m() {}\n")
        val start = System.nanoTime()
        val id = submit(port, scripted(slow, "--plugin", SleepingPlugin.className))
        val stream = openStream(port, id)
        try {
          val waited = (System.nanoTime() - start) / 1000000
          stream.head.status shouldBe 200
          waited should be >= 1900L
          stream.finish()._2 shouldBe Complete
        } finally {
          stream.close()
        }
      }

      "end the stream of a program that verifies with the overall result" in {
        val lines = linesOf(fetch(port, submit(port, silicon(verified))))
        lines.head shouldBe firstLine
        lines.count(_.endsWith(messageOfType("verification_result"))) shouldBe 2
        lines.filter(_.contains(""""kind":"for_entity"""")).foreach(_ should include(""""status":"success""""))
        withoutTimes(lines.last) shouldBe
          """{"msg_body":{"details":{"time":0},"kind":"overall","status":"success","verifier":"silicon"},"msg_type":"verification_result"}"""
      }

      "end the stream of a program that does not verify with the overall result and its errors" in {
        val lines = linesOf(fetch(port, submit(port, silicon(failing))))
        lines.head shouldBe firstLine
        lines.last should startWith("""{"msg_body":{"details":{"result":{"errors":[{""")
        lines.last should include(""""tag":"postcondition.violated:assertion.false"""")
        lines.last should endWith(overallResult("failure", "silicon"))
      }

      "end the stream of a file with a syntax error with the parse result" in {
        val lines = linesOf(fetch(port, submit(port, silicon(parseError))))
        lines.size shouldBe 2
        lines.head shouldBe firstLine
        lines.last should startWith("""{"msg_body":{"details":{"result":{"errors":[{""")
        lines.last should include(""""tag":"parser.error"""")
        lines.last should endWith(""""status":"failure"},"msg_type":"ast_construction_result"}""")
      }

      "end the stream of a file with a type error after the parse result with a `pprogram` line" in {
        val lines = linesOf(fetch(port, submit(port, silicon(typeError))))
        lines.size shouldBe 4
        lines.head shouldBe firstLine
        lines(1) should endWith(messageOfType("warnings_during_parsing"))
        lines(2) should include(""""tag":"typechecker.error"""")
        lines(2) should endWith(""""status":"failure"},"msg_type":"ast_construction_result"}""")
        lines.last shouldBe """{"msg_body":false,"msg_type":"pprogram"}"""
      }

      "end the stream of a request for a backend it does not know with an exception report" in {
        val lines = linesOf(fetch(port, submit(port, s"nobackend ${quoted(verified)}")))
        lines.head shouldBe firstLine
        lines.last should startWith("""{"msg_body":{"message":"Verification backend (<: SilFrontend) `nobackend` could not be found.","stacktrace":[""")
        lines.last should endWith(messageOfType("exception_report"))
      }

      "answer with one JSON object instead of a stream when the parse threw" in {
        // a plugin that cannot be loaded makes the parser throw
        val id = submit(port, silicon(verified, "--plugin", "no.such.Plugin"))
        val reply = fetch(port, id)
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.header("Content-Length") shouldBe Some(reply.body.length.toString)
        reply.text should startWith(s"""{"msg":"The verification job #$id resulted in a terrible error: """)
        reply.text should endWith(""""}""")
      }

      "forget a request whose parse threw once that has been reported" in wantedButNotYet {
        // today every later GET gets the same reply, and the request keeps its place for good
        val id = submit(port, silicon(verified, "--plugin", "no.such.Plugin"))
        fetch(port, id).text should include("terrible error")
        eventually("the request is forgotten", 5000)(fetch(port, id).text == doesNotExist(id))
      }

      "say that a request does not exist once its result has been fetched to the end" in {
        val id = submit(port, scripted(verified))
        linesOf(fetch(port, id))
        // the server forgets the request just after it has sent the last byte
        eventually("the request is forgotten")(fetch(port, id).text == doesNotExist(id))
        get(port, s"/discard/$id").text shouldBe doesNotExist(id)
      }

      "tell a second reader of a request that it does not exist" in wantedButNotYet {
        // today the second reader gets a 200 and then a connection reset
        val release = newFile("release")
        val id = submit(port, scripted(verified, "--scriptWaitFor", quoted(release)))
        val first = openStream(port, id)
        try {
          first.readUntil("scripted: waiting")
          val second = try fetch(port, id, timeoutMs = 5000) finally Files.createFile(release)
          first.finish()._2 shouldBe Complete
          second.status shouldBe 200
          second.text shouldBe doesNotExist(id)
        } finally {
          first.close()
        }
      }

      "not wait for a reader that has fallen behind" in wantedButNotYet {
        // today a verification is held up once 10,000 of its messages are unread
        val journal = newFile("journal")
        val messages = 30000
        val id = submit(port, scripted(verified, "--scriptName", "F", "--scriptJournal", quoted(journal),
          "--scriptMessages", messages.toString, "--scriptMessageSize", "1000"))
        val stream = openStream(port, id)
        try {
          eventually("the verification ends although nobody reads its messages", 5000)(journalOf(journal) == Seq("F started", "F finished"))
          val (text, ending) = stream.finish(60000)
          ending shouldBe Complete
          val lines = text.split("\n")
          lines.count(_.contains("scripted: message ")) shouldBe messages
          lines.last should endWith(overallResult("success", "scripted"))
        } finally {
          stream.close()
        }
      }

      "answer although the file's parse takes longer than 5 s" in wantedButNotYet {
        // today: 503 after 5 s
        val slow = write("slow_parse_7s.vpr", "// sleep 7000\nmethod m() {}\n")
        val id = submit(port, scripted(slow, "--plugin", SleepingPlugin.className))
        linesOf(fetch(port, id)).last should endWith(overallResult("success", "scripted"))
      }
    }

    "it gets a `GET /ast/<id>`" should {

      "send the lines of the parse and leave the request's result alone" in wantedButNotYet {
        // today the result can no longer be fetched afterwards: the connection is reset
        val journal = newFile("journal")
        val id = submit(port, scripted(verified, "--scriptName", "T", "--scriptJournal", quoted(journal)))
        val parse = linesOf(get(port, s"/ast/$id"))
        parse.head shouldBe firstLine
        parse.last should endWith(messageOfType("program_definitions"))
        // it does not start the verification, today and afterwards
        Thread.sleep(1500)
        journalOf(journal) shouldBe empty
        val lines = linesOf(fetch(port, id))
        lines.head shouldBe firstLine
        lines.last should endWith(overallResult("success", "scripted"))
        journalOf(journal) shouldBe Seq("T started", "T finished")
      }
    }

    "it gets a `GET /discard/<id>`" should {

      "interrupt a running verification, whose stream then ends properly" in {
        val journal = newFile("journal")
        val id = submit(port, scripted(verified, "--scriptName", "D", "--scriptJournal", quoted(journal),
          "--scriptWaitFor", quoted(newFile("never"))))
        val stream = openStream(port, id)
        try {
          stream.readUntil("scripted: waiting")
          val reply = get(port, s"/discard/$id")
          reply.status shouldBe 200
          reply.header("Content-Type") shouldBe Some("application/json")
          reply.text shouldBe s"""{"msg":"ver_id_$id has been successfully interrupted."}"""
          val (text, ending) = stream.finish()
          ending shouldBe Complete
          text should not include "scripted: released"
          journalOf(journal) shouldBe Seq("D started", "D aborted java.lang.InterruptedException")
          eventually("the request is forgotten")(get(port, s"/discard/$id").text == doesNotExist(id))
        } finally {
          stream.close()
        }
      }

      "leave a verification alone that has ended, and say so, while its result is still being read" in {
        // more than a connection holds, so that the server cannot send the result to the end
        // before this test reads it
        val journal = newFile("journal")
        val messages = 5000
        val id = submit(port, scripted(verified, "--scriptName", "E", "--scriptJournal", quoted(journal),
          "--scriptMessages", messages.toString, "--scriptMessageSize", "4000"))
        val stream = openStream(port, id)
        try {
          eventually("the verification ends")(journalOf(journal) == Seq("E started", "E finished"))
          Thread.sleep(500) // from the journal's last line to the end of the verification's task
          get(port, s"/discard/$id").text shouldBe s"""{"msg":"ver_id_$id has already been finalized."}"""
          val (text, ending) = stream.finish(60000)
          ending shouldBe Complete
          val lines = text.split("\n")
          lines.count(_.contains("scripted: message ")) shouldBe messages
          lines.last should endWith(overallResult("success", "scripted"))
        } finally {
          stream.close()
        }
      }

      "drop a request that has not been fetched, without ever verifying it" in wantedButNotYet {
        // today the verification is started and interrupted, and one more fetch returns lines
        val journal = newFile("journal")
        val id = submit(port, scripted(verified, "--scriptName", "N", "--scriptJournal", quoted(journal)))
        Thread.sleep(1500) // this small file has been parsed by now
        get(port, s"/discard/$id").text shouldBe s"""{"msg":"ver_id_$id has been successfully interrupted."}"""
        fetch(port, id).text shouldBe doesNotExist(id)
        journalOf(journal) shouldBe empty
      }

      "answer while the file is being parsed, without waiting for the parser, and end the request" in wantedButNotYet {
        // today: 503 after 5 s
        val slow = write("slow_parse_discard.vpr", "// sleep 7000\nmethod m() {}\n")
        val id = submit(port, scripted(slow, "--plugin", SleepingPlugin.className))
        val reply = get(port, s"/discard/$id", timeoutMs = 3000)
        reply.status shouldBe 200
        reply.text shouldBe s"""{"msg":"ver_id_$id has been successfully interrupted."}"""
        fetch(port, id).text shouldBe doesNotExist(id)
      }
    }

    "a reader goes away" should {

      "end a verification that keeps reporting, and forget the request" in {
        val journal = newFile("journal")
        val id = submit(port, scripted(verified, "--scriptName", "R", "--scriptJournal", quoted(journal),
          "--scriptWaitFor", quoted(newFile("never")), "--scriptReportEveryMs", "100"))
        val stream = openStream(port, id)
        try stream.readUntil("scripted: still waiting") finally stream.close()
        eventually("the verification is ended")(journalOf(journal).exists(_.startsWith("R aborted")))
        journalOf(journal).head shouldBe "R started"
        eventually("the request is forgotten")(fetch(port, id).text == doesNotExist(id))
      }
    }

    "several requests are waiting" should {

      "not verify a request before its result is fetched" in {
        val journal = newFile("journal")
        val id = submit(port, scripted(verified, "--scriptName", "A", "--scriptJournal", quoted(journal)))
        Thread.sleep(1500)
        journalOf(journal) shouldBe empty
        linesOf(fetch(port, id))
        journalOf(journal) shouldBe Seq("A started", "A finished")
      }

      "verify requests that are fetched one after the other one after the other" in {
        val journal = newFile("journal")
        val ids = Seq("A", "B", "C").map(name =>
          submit(port, scripted(verified, "--scriptName", name, "--scriptJournal", quoted(journal), "--scriptRunMs", "200")))
        ids.foreach(id => linesOf(fetch(port, id)))
        journalOf(journal) shouldBe Seq("A started", "A finished", "B started", "B finished", "C started", "C finished")
      }

      "verify requests that are fetched together at the same time" in {
        val journal = newFile("journal")
        val release = newFile("release")
        val ids = Seq("A", "B").map(name =>
          submit(port, scripted(verified, "--scriptName", name, "--scriptJournal", quoted(journal), "--scriptWaitFor", quoted(release))))
        val streams = ids.map(id => openStream(port, id))
        try {
          // both run before either can end
          eventually("both verifications run")(journalOf(journal).toSet == Set("A started", "B started"))
          Files.createFile(release)
          streams.foreach(_.finish()._2 shouldBe Complete)
          journalOf(journal).drop(2).toSet shouldBe Set("A finished", "B finished")
        } finally {
          streams.foreach(_.close())
        }
      }
    }

    "it has as many requests as `--maximumActiveJobs` allows" should {

      "refuse a `POST /verify` with the ids -1" in {
        postVerify(smallServer.port, scripted(verified)).text shouldBe """{"ast_id":0,"id":0}"""
        val reply = postVerify(smallServer.port, scripted(verified))
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.text shouldBe """{"ast_id":-1,"id":-1}"""
        get(smallServer.port, "/verify/-1").status shouldBe 404
      }

      "accept a fourth request with the default limit of three, once the earlier ones have been fetched (issue #375)" in wantedButNotYet {
        // today a request never gives its place back
        val server = defaultServerA
        warmUp(server, scripted(verified))
        for (_ <- 2 to 5) linesOf(fetch(server.port, submit(server.port, scripted(verified))))
      }

      "accept a fourth request after three whose files had a type error (issue #376)" in wantedButNotYet {
        val server = defaultServerB
        warmUp(server, scripted(typeError))
        for (_ <- 2 to 3) linesOf(fetch(server.port, submit(server.port, scripted(typeError))))
        linesOf(fetch(server.port, submit(server.port, scripted(verified))))
      }

      "refuse a request only while it is full, and never one that it has accepted" in wantedButNotYet {
        // today a request never gives its place back: after the warm-up and one more request this
        // server is full for good
        val server = startServer("two-places", "--maximumActiveJobs", "2")
        warmUp(server, scripted(verified))
        val port = server.port
        val journal = newFile("journal")
        val release = newFile("release")
        def slow(name: String, waitFor: Path, more: String*): String =
          scripted(verified, Seq("--scriptName", name, "--scriptJournal", quoted(journal), "--scriptWaitFor", quoted(waitFor)) ++ more: _*)
        val refused = """{"ast_id":-1,"id":-1}"""

        // two requests fill the server: the next one is refused in its reply
        val a = submit(port, slow("A", release))
        val b = submit(port, slow("B", newFile("never"), "--scriptStopMs", "1500"))
        postVerify(port, slow("X", release)).text shouldBe refused

        // a request that has ended makes room for another
        val streamA = openStream(port, a)
        val streamB = openStream(port, b)
        try {
          streamA.readUntil("scripted: waiting")
          Files.createFile(release)
          streamA.finish()._2 shouldBe Complete
          val c = submit(port, slow("C", release))
          postVerify(port, slow("X", release)).text shouldBe refused

          // a request that is discarded makes room once its verification has stopped, which this
          // one takes a second and a half to do
          streamB.readUntil("scripted: waiting")
          get(port, s"/discard/$b").text shouldBe s"""{"msg":"ver_id_$b has been successfully interrupted."}"""
          postVerify(port, slow("X", release)).text shouldBe refused
          streamB.finish()._2 shouldBe Complete
          var d = -1
          eventually("a request is accepted after the discard", 5000) {
            postVerify(port, slow("D", release)).text match {
              case Accepted(_, id) => d = id.toInt; true
              case _ => false
            }
          }

          // a refused request has used up no id, and none of them was ever started
          Seq(a, b, c, d) shouldBe Seq(a, a + 1, a + 2, a + 3)
          // what was accepted while the server was nearly full is carried out when it is fetched
          linesOf(fetch(port, c)).last should endWith(overallResult("success", "scripted"))
          linesOf(fetch(port, d)).last should endWith(overallResult("success", "scripted"))
          journalOf(journal).filter(_.startsWith("X")) shouldBe empty
          journalOf(journal) should contain allOf("A finished", "B aborted java.lang.InterruptedException", "C finished", "D finished")
        } finally {
          streamA.close()
          streamB.close()
        }
      }

      "answer a discard, a refused request and `/exit` within a second while a result is being fetched for each of its places" in wantedButNotYet {
        // today the server is full for good after the warm-up and one more request, so that it
        // never gets as far as two results that are fetched at the same time
        val server = startServer("two-streams", "--maximumActiveJobs", "2")
        warmUp(server, scripted(verified))
        val port = server.port
        val never = newFile("never")
        def withinASecond[T](what: String)(action: => T): T = {
          val start = System.nanoTime()
          val result = action
          val took = (System.nanoTime() - start) / 1000000
          withClue(s"$what took $took ms. ") { took should be < 1000L }
          result
        }
        val ids = (1 to 2).map(_ => submit(port, scripted(verified, "--scriptWaitFor", quoted(never))))
        val streams = ids.map(openStream(port, _))
        try {
          streams.foreach(_.readUntil("scripted: waiting"))
          withinASecond("the refusal")(postVerify(port, scripted(verified))).text shouldBe """{"ast_id":-1,"id":-1}"""
          withinASecond("the discard")(get(port, s"/discard/${ids.head}")).text shouldBe
            s"""{"msg":"ver_id_${ids.head} has been successfully interrupted."}"""
          withinASecond("the exit")(get(port, "/exit")).text shouldBe """{"msg":"shutting down..."}"""
        } finally {
          streams.foreach(_.close())
        }
      }
    }

    "it gets a `GET /exit`" should {

      "answer `shutting down...` and end with exit code 0" in {
        val server = defaultServerA
        get(server.port, "/cache/flush").status shouldBe 200
        val reply = get(server.port, "/exit")
        reply.status shouldBe 200
        reply.header("Content-Type") shouldBe Some("application/json")
        reply.text shouldBe """{"msg":"shutting down..."}"""
        server.awaitExit(20000) shouldBe Some(0)
        intercept[IOException](new Connection(server.port))
      }

      "end also when a request has never been fetched" in wantedButNotYet {
        // today: 503 after 5 s, and the server stays
        val server = smallServer
        postVerify(server.port, scripted(verified)) // accepted or not, one request is waiting now
        val reply = get(server.port, "/exit", timeoutMs = 20000)
        reply.status shouldBe 200
        reply.text shouldBe """{"msg":"shutting down..."}"""
        server.awaitExit(20000) shouldBe Some(0)
      }

      "answer also when it is the first request of a server" in wantedButNotYet {
        // today the server exits before it has written the reply
        val server = startServer("first-exit")
        val reply = get(server.port, "/exit")
        reply.status shouldBe 200
        reply.text shouldBe """{"msg":"shutting down..."}"""
        server.awaitExit(20000) shouldBe Some(0)
      }

      "end an open stream properly when its verification ends in time" in wantedButNotRun {
        // today the stream is ended properly in one run and cut in the next
        val server = startServer("exit-stream")
        warmUp(server, scripted(verified))
        val stream = openStream(server.port, submit(server.port, scripted(verified, "--scriptWaitFor", quoted(newFile("never")))))
        try {
          stream.readUntil("scripted: waiting")
          get(server.port, "/exit").text shouldBe """{"msg":"shutting down..."}"""
          stream.finish()._2 shouldBe Complete
          server.awaitExit(20000) shouldBe Some(0)
        } finally {
          stream.close()
        }
      }
    }

    "nothing is sent for three quarters of a minute" should {

      "keep an idle connection and a silent stream open" in {
        // Both were opened when the suite started. 45 s, because an HTTP server may close what is
        // idle after a time of its own: the JDK's does after 30 to 40 s unless it is told not to.
        val wait = idleSince + 45000 - System.currentTimeMillis()
        if (wait > 0) Thread.sleep(wait)
        idleConnection.isClosedByServer(timeoutMs = 100) shouldBe false
        idleConnection.send("GET", "/cache/flush")
        idleConnection.readResponse().text shouldBe """{"msg":"The cache has been flushed successfully."}"""
        Files.createFile(silentRelease)
        val (text, ending) = silentStream.finish()
        ending shouldBe Complete
        text.split("\n").last should endWith(overallResult("success", "scripted"))
      }
    }
  }
}
