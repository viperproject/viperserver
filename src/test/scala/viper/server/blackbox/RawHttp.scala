// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import java.io.{BufferedInputStream, ByteArrayOutputStream, EOFException, IOException, OutputStream}
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}

import scala.collection.mutable.ArrayBuffer

/** An HTTP/1.1 client on a plain socket. It sends exactly the bytes a test asks for and shows
  * what an HTTP library hides: the header lines as they were sent, the single chunks of a chunked
  * body, whether a body was ended properly, and how a connection ended.
  */
object RawHttp {

  /** status line and headers of a response */
  final case class Head(status: Int, reason: String, headers: Seq[(String, String)]) {
    /** header names are compared without regard to case, as HTTP defines them */
    def header(name: String): Option[String] = headers.collectFirst { case (n, v) if n.equalsIgnoreCase(name) => v }
    def isChunked: Boolean = header("Transfer-Encoding").exists(_.equalsIgnoreCase("chunked"))
    def contentLength: Option[Int] = header("Content-Length").map(_.trim.toInt)
  }

  /** How a body ended. */
  sealed trait Ending
  /** as HTTP wants it: all bytes of the announced length, or the terminating chunk */
  case object Complete extends Ending
  /** the connection was closed or reset before the body was complete */
  final case class Cut(how: String) extends Ending

  final case class Response(head: Head, body: Array[Byte], chunks: Seq[Array[Byte]], ending: Ending) {
    def status: Int = head.status
    def header(name: String): Option[String] = head.header(name)
    /** the body, which every reply of ViperServer encodes as UTF-8 */
    def text: String = new String(body, UTF_8)
    override def toString: String =
      s"${head.status} ${head.reason} ${head.headers.map { case (n, v) => s"$n: $v" }.mkString("[", ", ", "]")} $ending\n$text"
  }

  /** the headers that the `requests` library of Python adds to every request, which is what
    * `client.py` of `viperproject/viper_client` sends */
  val clientHeaders: Seq[(String, String)] = Seq(
    "User-Agent" -> "python-requests/2.31.0",
    "Accept-Encoding" -> "gzip, deflate",
    "Accept" -> "*/*",
    "Connection" -> "keep-alive")

  val jsonContentType: (String, String) = "Content-Type" -> "application/json"

  final class Connection(val port: Int, host: String = "127.0.0.1", connectTimeoutMs: Int = 5000) extends AutoCloseable {
    private val socket = new Socket()
    socket.connect(new InetSocketAddress(host, port), connectTimeoutMs)
    socket.setTcpNoDelay(true)
    private val in = new BufferedInputStream(socket.getInputStream)
    private val out: OutputStream = socket.getOutputStream

    /** sends bytes as they are */
    def sendRaw(bytes: Array[Byte]): Unit = {
      out.write(bytes)
      out.flush()
    }

    /** Sends a request. `Host` is added first and `Content-Length` last, the latter only when
      * there is a body, as `requests` does it.
      */
    def send(method: String, path: String, headers: Seq[(String, String)] = clientHeaders, body: Option[Array[Byte]] = None): Unit = {
      val all = Seq("Host" -> s"localhost:$port") ++ headers ++ body.map(b => "Content-Length" -> b.length.toString)
      val head = s"$method $path HTTP/1.1\r\n" + all.map { case (n, v) => s"$n: $v\r\n" }.mkString + "\r\n"
      val bytes = new ByteArrayOutputStream()
      bytes.write(head.getBytes(ISO_8859_1))
      body.foreach(b => bytes.write(b))
      sendRaw(bytes.toByteArray)
    }

    /** true iff bytes of a response are waiting to be read; waits up to `waitMs` for them */
    def hasAnswered(waitMs: Int = 0): Boolean = {
      val deadline = System.currentTimeMillis() + waitMs
      while (in.available() == 0 && System.currentTimeMillis() < deadline) Thread.sleep(5)
      in.available() > 0
    }

    /** Sends a request with a large body the way a careful client does: it stops sending as soon
      * as the server answers, which a server may do before it has read a body it refuses, and it
      * does not mind if the server stops reading. The answer is then read as usual.
      *
      * @param chunked    send the body with `Transfer-Encoding: chunked`, one chunk per piece,
      *                   instead of announcing its length
      * @param headStartMs how long to wait for an answer to the headers alone
      */
    def sendLarge(method: String, path: String, headers: Seq[(String, String)], pieces: Seq[Array[Byte]],
                  chunked: Boolean, headStartMs: Int = 500): Unit = {
      val length = if (chunked) "Transfer-Encoding" -> "chunked" else "Content-Length" -> pieces.map(_.length.toLong).sum.toString
      val all = Seq("Host" -> s"localhost:$port") ++ headers :+ length
      sendRaw((s"$method $path HTTP/1.1\r\n" + all.map { case (n, v) => s"$n: $v\r\n" }.mkString + "\r\n").getBytes(ISO_8859_1))
      if (hasAnswered(headStartMs)) return
      try {
        pieces.foreach { piece =>
          if (hasAnswered()) return
          if (chunked) out.write((Integer.toHexString(piece.length) + "\r\n").getBytes(ISO_8859_1))
          out.write(piece)
          if (chunked) out.write("\r\n".getBytes(ISO_8859_1))
        }
        if (chunked) out.write("0\r\n\r\n".getBytes(ISO_8859_1))
        out.flush()
      } catch {
        case _: IOException => // the server has stopped reading; what it answered is still to be read
      }
    }

    private def readLine(): String = {
      val line = new ByteArrayOutputStream()
      var previous = -1
      while (true) {
        val b = in.read()
        if (b < 0) throw new EOFException("the connection was closed in the middle of a line")
        if (previous == '\r' && b == '\n') {
          val bytes = line.toByteArray
          return new String(bytes, 0, bytes.length - 1, ISO_8859_1)
        }
        line.write(b)
        previous = b
      }
      throw new IllegalStateException("unreachable")
    }

    private def readExactly(n: Int): Array[Byte] = {
      val bytes = new Array[Byte](n)
      var read = 0
      while (read < n) {
        val r = in.read(bytes, read, n - read)
        if (r < 0) throw new EOFException(s"the connection was closed after $read of $n bytes")
        read += r
      }
      bytes
    }

    /** Reads the status line and the headers. Throws `SocketTimeoutException` if the server sends
      * nothing for `timeoutMs`, and `IOException` if the connection ends first.
      */
    def readHead(timeoutMs: Int = 30000): Head = {
      socket.setSoTimeout(timeoutMs)
      val statusLine = readLine()
      val parts = statusLine.split(" ", 3)
      if (parts.length < 2 || !parts(0).startsWith("HTTP/")) throw new IOException(s"not a status line: `$statusLine`")
      val headers = ArrayBuffer[(String, String)]()
      var line = readLine()
      while (line.nonEmpty) {
        val colon = line.indexOf(':')
        if (colon < 0) throw new IOException(s"not a header line: `$line`")
        headers += line.substring(0, colon) -> line.substring(colon + 1).trim
        line = readLine()
      }
      Head(parts(1).toInt, if (parts.length > 2) parts(2) else "", headers.toSeq)
    }

    /** The data of the next chunk of a chunked body; None for the terminating chunk. */
    def readChunk(timeoutMs: Int = 30000): Option[Array[Byte]] = {
      socket.setSoTimeout(timeoutMs)
      val sizeLine = readLine()
      val size = Integer.parseInt(sizeLine.takeWhile(_ != ';').trim, 16)
      if (size == 0) {
        while (readLine().nonEmpty) {} // trailers
        None
      } else {
        val data = readExactly(size)
        val end = readLine()
        if (end.nonEmpty) throw new IOException(s"a chunk of $size bytes is followed by `$end`")
        Some(data)
      }
    }

    /** Reads the rest of a body to its end, or to the point where the connection breaks. A server
      * that sends nothing for `timeoutMs` makes this throw `SocketTimeoutException`.
      */
    def readBody(head: Head, hasBody: Boolean = true, timeoutMs: Int = 30000): Response = {
      socket.setSoTimeout(timeoutMs)
      val body = new ByteArrayOutputStream()
      val chunks = ArrayBuffer[Array[Byte]]()
      val ending: Ending = try {
        if (!hasBody || head.status / 100 == 1 || head.status == 204 || head.status == 304) {
          Complete
        } else if (head.isChunked) {
          var chunk = readChunk(timeoutMs)
          while (chunk.isDefined) {
            chunks += chunk.get
            body.write(chunk.get)
            chunk = readChunk(timeoutMs)
          }
          Complete
        } else head.contentLength match {
          case Some(n) =>
            // read in pieces, so that what arrived is kept if the connection breaks:
            var missing = n
            while (missing > 0) {
              val piece = readExactly(math.min(missing, 8192))
              body.write(piece)
              missing -= piece.length
            }
            Complete
          case None =>
            // the end of the connection is the end of the body
            val buffer = new Array[Byte](8192)
            var r = in.read(buffer)
            while (r >= 0) {
              body.write(buffer, 0, r)
              r = in.read(buffer)
            }
            Complete
        }
      } catch {
        case e: SocketTimeoutException => throw e
        case e: IOException => Cut(e.toString)
      }
      Response(head, body.toByteArray, chunks.toSeq, ending)
    }

    def readResponse(hasBody: Boolean = true, timeoutMs: Int = 30000): Response = readBody(readHead(timeoutMs), hasBody, timeoutMs)

    /** true iff the server has closed the connection: waits up to `timeoutMs` for that */
    def isClosedByServer(timeoutMs: Int): Boolean = {
      socket.setSoTimeout(timeoutMs)
      try {
        in.mark(1)
        val b = in.read()
        if (b >= 0) in.reset()
        b < 0
      } catch {
        case _: SocketTimeoutException => false
        case _: IOException => true
      }
    }

    override def close(): Unit = socket.close()
  }

  /** One request on a connection of its own. */
  def request(port: Int, method: String, path: String, headers: Seq[(String, String)] = clientHeaders,
              body: Option[Array[Byte]] = None, timeoutMs: Int = 30000, host: String = "127.0.0.1"): Response = {
    val connection = new Connection(port, host)
    try {
      connection.send(method, path, headers, body)
      connection.readResponse(hasBody = method != "HEAD", timeoutMs)
    } finally {
      connection.close()
    }
  }

  def get(port: Int, path: String, timeoutMs: Int = 30000): Response = request(port, "GET", path, timeoutMs = timeoutMs)

  /** a JSON string literal, escaped as Python's `json.dumps` escapes it */
  def jsonString(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case c if c < 0x20 || c > 0x7e => sb ++= f"\\u${c.toInt}%04x"
      case c => sb += c
    }
    sb += '"'
    sb.toString
  }
}
