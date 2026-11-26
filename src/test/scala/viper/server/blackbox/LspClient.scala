// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.net.{InetAddress, Socket}
import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, Executors, TimeUnit, TimeoutException}

import com.google.gson.{JsonElement, JsonObject}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.LanguageServer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/** What a ViperServer in LSP mode offers beyond the Language Server Protocol, written down a
  * second time on purpose: the tests use none of the server's own classes, so that a change of
  * the protocol does not go unnoticed. Custom messages travel as plain JSON objects.
  */
trait ViperLanguageServer extends LanguageServer {
  @JsonRequest("GetVersion")
  def getVersion(request: JsonObject): CompletableFuture[JsonObject]

  @JsonNotification("Verify")
  def verify(params: JsonObject): Unit

  @JsonRequest("StopVerification")
  def stopVerification(request: JsonObject): CompletableFuture[JsonObject]
}

/** A scripted IDE: an LSP4J client that is connected to a ViperServer over a socket, does what a
  * test tells it, answers the server's requests the way the Viper IDE does, and keeps everything
  * the server sends in the order of arrival.
  */
final class LspClient(port: Int) extends AutoCloseable {

  import LspClient._

  private val socket = new Socket(InetAddress.getLoopbackAddress, port)
  private val startedAt = System.nanoTime()
  private val received = ArrayBuffer[Received]()
  private var logLines = 0
  private val texts = mutable.Map[String, String]()
  private val versions = mutable.Map[String, Int]()
  private val verifiesSent = mutable.Map[String, Int]().withDefaultValue(0)
  private val verifiesGivenUp = mutable.Map[String, Int]().withDefaultValue(0)

  // LSP4J reports the end of a connection with a stack trace, also when the test ends it
  private val quietLogger = java.util.logging.Logger.getLogger("org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer")
  quietLogger.setLevel(java.util.logging.Level.OFF)

  private def record(make: Long => Received): Unit = received.synchronized {
    received += make((System.nanoTime() - startedAt) / 1000000)
    received.notifyAll()
  }

  private def string(o: JsonObject, name: String): Option[String] =
    Option(o.get(name)).filterNot(_.isJsonNull).map(_.getAsString)

  private def number(o: JsonObject, name: String): Double =
    Option(o.get(name)).filterNot(_.isJsonNull).map(_.getAsDouble).getOrElse(-1)

  private def done[T](value: T): CompletableFuture[T] = CompletableFuture.completedFuture(value)

  /** What the server may call. This is not LSP4J's `LanguageClient`, because a Scala class that
    * implements it gets a second copy of each of that interface's default methods, which LSP4J
    * refuses. A call of anything that is not listed here is answered with an error.
    */
  private object ide {
    @JsonNotification("textDocument/publishDiagnostics")
    def publishDiagnostics(p: PublishDiagnosticsParams): Unit =
      record(Diagnostics(_, p.getUri, p.getDiagnostics.asScala.toSeq))

    @JsonNotification("window/showMessage")
    def showMessage(p: MessageParams): Unit = record(Other(_, "window/showMessage", p.getMessage))

    @JsonNotification("window/logMessage")
    def logMessage(p: MessageParams): Unit = if (p != null) received.synchronized { logLines += 1 }

    // the server asks the IDE to fetch these again whenever a parse has given it new data
    @JsonRequest("workspace/codeLens/refresh")
    def refreshCodeLenses(): CompletableFuture[JsonElement] = { record(ServerRequest(_, "workspace/codeLens/refresh", "")); done(null) }

    @JsonRequest("workspace/inlayHint/refresh")
    def refreshInlayHints(): CompletableFuture[JsonElement] = { record(ServerRequest(_, "workspace/inlayHint/refresh", "")); done(null) }

    @JsonRequest("workspace/semanticTokens/refresh")
    def refreshSemanticTokens(): CompletableFuture[JsonElement] = { record(ServerRequest(_, "workspace/semanticTokens/refresh", "")); done(null) }

    @JsonNotification("StateChange")
    def stateChange(p: JsonObject): Unit = record(at => StateChange(at,
      state = states.lift(number(p, "newState").toInt).getOrElse(number(p, "newState").toString),
      success = verdicts.lift(number(p, "success").toInt).getOrElse(number(p, "success").toString),
      uri = string(p, "uri"), filename = string(p, "filename"),
      verificationCompleted = number(p, "verificationCompleted"), progress = number(p, "progress"),
      time = number(p, "time"), error = string(p, "error").getOrElse("")))

    @JsonNotification("VerificationNotStarted")
    def verificationNotStarted(p: JsonObject): Unit = record(NotStarted(_, string(p, "uri").getOrElse("")))

    @JsonNotification("UnhandledViperServerMessageType")
    def unhandledMessage(p: JsonObject): Unit = record(Unhandled(_, string(p, "msgType").getOrElse(""), string(p, "msg").getOrElse("")))

    @JsonNotification("Log")
    def log(p: JsonObject): Unit = if (p != null) received.synchronized { logLines += 1 }

    @JsonNotification("Hint")
    def hint(p: JsonObject): Unit = record(Other(_, "Hint", p.toString))

    @JsonRequest("GetViperFileEndings")
    def fileEndings(): CompletableFuture[JsonObject] = {
      record(ServerRequest(_, "GetViperFileEndings", ""))
      val endings = new com.google.gson.JsonArray()
      Seq(".vpr", ".sil").foreach(e => endings.add(e))
      val answer = new JsonObject()
      answer.add("fileEndings", endings)
      done(answer)
    }

    @JsonRequest("SetupProject")
    def setupProject(p: JsonObject): CompletableFuture[JsonElement] = {
      record(ServerRequest(_, "SetupProject", p.toString))
      done(null)
    }

    @JsonRequest("GetIdentifier")
    def identifier(p: JsonObject): CompletableFuture[JsonElement] = { record(ServerRequest(_, "GetIdentifier", p.toString)); done(null) }

    @JsonRequest("GetRange")
    def range(p: JsonObject): CompletableFuture[JsonElement] = { record(ServerRequest(_, "GetRange", p.toString)); done(null) }
  }

  private val threads = Executors.newCachedThreadPool(runnable => {
    val thread = new Thread(runnable, "lsp-test-client")
    thread.setDaemon(true)
    thread
  })

  private val launcher = new Launcher.Builder[ViperLanguageServer]()
    .setLocalService(ide)
    .setRemoteInterface(classOf[ViperLanguageServer])
    .setInput(socket.getInputStream)
    .setOutput(socket.getOutputStream)
    .setExecutorService(threads)
    .create()

  val server: ViperLanguageServer = launcher.getRemoteProxy
  private val listening = launcher.startListening()

  /** true once the server has closed the connection */
  def connectionEnded: Boolean = listening.isDone

  // ---------------------------------------------------------------------------------------------
  // what the server has sent

  /** the number of messages received so far: a point in time to wait from */
  def mark: Int = received.synchronized(received.size)

  def events(from: Int = 0): Seq[Received] = received.synchronized(received.drop(from).toList)

  def logCount: Int = received.synchronized(logLines)

  /** The first message since `from` that `pick` is defined for; waits for it. */
  def await[T](what: String, from: Int, timeoutMs: Long = 60000)(pick: PartialFunction[Received, T]): T = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var next = from
    received.synchronized {
      while (true) {
        while (next < received.size) {
          if (pick.isDefinedAt(received(next))) return pick(received(next))
          next += 1
        }
        val remaining = deadline - System.currentTimeMillis()
        if (remaining <= 0) {
          throw new AssertionError(s"within $timeoutMs ms the server did not send: $what. It sent:\n${describe(from)}")
        }
        received.wait(remaining)
      }
      throw new IllegalStateException("unreachable")
    }
  }

  /** Waits until the server has sent nothing for `quietMs`. */
  def awaitQuiet(quietMs: Long = 500, timeoutMs: Long = 60000): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var seen = (mark, logCount)
    var since = System.currentTimeMillis()
    while (System.currentTimeMillis() - since < quietMs) {
      if (System.currentTimeMillis() > deadline) throw new AssertionError(s"the server kept sending for $timeoutMs ms")
      Thread.sleep(20)
      val now = (mark, logCount)
      if (now != seen) {
        seen = now
        since = System.currentTimeMillis()
      }
    }
  }

  /** the messages since `from`, one per line, for a failure message */
  def describe(from: Int = 0): String = events(from).map(e => f"  ${e.at / 1000.0}%8.3f $e").mkString("\n")

  // ---------------------------------------------------------------------------------------------
  // what an IDE does

  def initialize(): Unit = {
    val params = new InitializeParams()
    params.setCapabilities(new ClientCapabilities())
    server.initialize(params).get(60, TimeUnit.SECONDS)
    server.initialized(new InitializedParams())
  }

  def open(file: Path, text: String): Unit = {
    val uri = uriOf(file)
    texts(uri) = text
    versions(uri) = 1
    server.getTextDocumentService.didOpen(new DidOpenTextDocumentParams(new TextDocumentItem(uri, "viper", 1, text)))
  }

  /** replaces the whole text of an open file, as one edit */
  def edit(file: Path, newText: String): Unit = {
    val uri = uriOf(file)
    val oldLines = texts(uri).split("\n", -1)
    val whole = new Range(new Position(0, 0), new Position(oldLines.length - 1, oldLines.last.length))
    texts(uri) = newText
    versions(uri) += 1
    server.getTextDocumentService.didChange(new DidChangeTextDocumentParams(
      new VersionedTextDocumentIdentifier(uri, versions(uri)),
      java.util.List.of(new TextDocumentContentChangeEvent(whole, newText))))
  }

  def close(file: Path): Unit = {
    val uri = uriOf(file)
    texts -= uri
    server.getTextDocumentService.didClose(new DidCloseTextDocumentParams(new TextDocumentIdentifier(uri)))
  }

  /** `Verify` as the IDE sends it: with the file's current text */
  def verify(file: Path, backend: String = "silicon", customArgs: String = "--disableCaching", text: Option[String] = None): Unit = {
    val uri = uriOf(file)
    val params = new JsonObject()
    params.addProperty("uri", uri)
    params.addProperty("content", text.orElse(texts.get(uri)).getOrElse(""))
    params.addProperty("manuallyTriggered", true)
    params.addProperty("workspace", "")
    params.addProperty("backend", backend)
    params.addProperty("customArgs", customArgs)
    received.synchronized { verifiesSent(uri) += 1 }
    server.verify(params)
  }

  /** `StopVerification`; None if the server does not answer in time */
  def stop(file: Path, timeoutMs: Long = 30000): Option[Boolean] = {
    val request = new JsonObject()
    request.addProperty("uri", uriOf(file))
    try {
      Some(server.stopVerification(request).get(timeoutMs, TimeUnit.MILLISECONDS).get("success").getAsBoolean)
    } catch {
      case _: TimeoutException => None
    }
  }

  /** Notes that the test has stopped the running verification of `file`, with `StopVerification`
    * or by closing the file. The server answers such a `Verify` with no verdict. */
  def stoppedVerify(file: Path): Unit = received.synchronized { verifiesGivenUp(uriOf(file)) += 1 }

  /** Notes that the test has sent another `Verify` while the one of `file` was not answered yet.
    * The server gives the earlier one up. */
  def supersededVerify(file: Path): Unit = stoppedVerify(file)

  /** how many `Verify` of a file the test has stopped or replaced */
  def givenUp(uri: String): Int = received.synchronized(verifiesGivenUp(uri))

  /** for each file, how many `Verify` were sent and how many were answered: with a result or with
    * `VerificationNotStarted` */
  def verifiesAndAnswers: Map[String, (Int, Int)] = received.synchronized {
    verifiesSent.toMap.map { case (uri, sent) =>
      val answers = received.count {
        case s: StateChange => s.isResult && s.uri.contains(uri)
        case NotStarted(_, u) => u == uri
        case _ => false
      }
      uri -> (sent, answers)
    }
  }

  /** closes the connection without a word, as an IDE that crashes */
  override def close(): Unit = {
    socket.close()
    threads.shutdownNow()
  }
}

object LspClient {

  /** the names of the numbers in a `StateChange` */
  val states: Vector[String] = Vector("Stopped", "Starting", "VerificationRunning", "VerificationPrintingHelp",
    "VerificationReporting", "PostProcessing", "Ready", "Stopping", "Stage", "ConstructingAst")
  val verdicts: Vector[String] = Vector("NA", "Success", "ParsingFailed", "TypecheckingFailed", "VerificationFailed",
    "Aborted", "Error", "Timeout")

  def uriOf(file: Path): String = file.toUri.toString

  /** something the server sent, with the time since the client connected, in milliseconds */
  sealed trait Received { def at: Long }

  final case class StateChange(at: Long, state: String, success: String, uri: Option[String], filename: Option[String],
                               verificationCompleted: Double, progress: Double, time: Double, error: String) extends Received {
    /** the notification that ends a `Verify` */
    def isResult: Boolean = state == "Ready" && verificationCompleted == 1.0
    override def toString: String = s"StateChange($state" + (if (success != "NA") s", $success" else "") +
      uri.map(u => s", uri=${u.split('/').last}").getOrElse("") + filename.map(f => s", filename=$f").getOrElse("") +
      (if (verificationCompleted >= 0) s", verificationCompleted=$verificationCompleted" else "") +
      (if (progress >= 0) s", progress=$progress" else "") + ")"
  }

  final case class Diagnostics(at: Long, uri: String, items: Seq[Diagnostic]) extends Received {
    def messages: Seq[String] = items.map(_.getMessage)
    override def toString: String = s"Diagnostics(${uri.split('/').last}, ${items.size}: ${messages.map(_.take(70)).mkString(" | ")})"
  }

  final case class NotStarted(at: Long, uri: String) extends Received

  final case class Unhandled(at: Long, msgType: String, msg: String) extends Received {
    override def toString: String = s"Unhandled($msgType)"
  }

  final case class ServerRequest(at: Long, method: String, params: String) extends Received

  final case class Other(at: Long, method: String, params: String) extends Received
}
