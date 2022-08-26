// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server

import java.io.IOException
import java.net.{ServerSocket, Socket}
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.Launcher.Builder
import viper.server.core.{DefaultVerificationExecutionContext, VerificationExecutionContext}
import viper.server.frontends.http.ViperHttpServer
import viper.server.frontends.lsp.{CustomReceiver, IdeLanguageClient, ViperServerService}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object ViperServerRunner {

  var viperServerHttp: ViperHttpServer = _

  def main(args: Array[String]): Unit = {
    val config = new ViperConfig(args.toIndexedSeq)
    val executor = new DefaultVerificationExecutionContext(threadPoolSize = Some(config.nThreads()))
    if (config.serverMode() == config.SERVER_MODE_LSP) {
      runLspServer(config)(executor)
    } else if (config.serverMode() == config.SERVER_MODE_HTTP) {
      runHttpServer(config)(executor)
    } else {
      throw new RuntimeException(s"unknown server mode ${config.serverMode()}")
    }
  }

  /**
    * Run VCS in HTTP mode.
    */
  def runHttpServer(config: ViperConfig)(executor: VerificationExecutionContext): Unit = {
    viperServerHttp = new ViperHttpServer(config)(executor)
    viperServerHttp.start()
    // wait until server has been stopped:
    Await.ready(viperServerHttp.stopped(), Duration.Inf)
    println("HTTP server has been stopped")
    executor.terminate()
    println("executor service has been shut down")
    // the following `exit` call is required such that the server eventually terminates for `longDuration.vpr` in the
    // test suite of viper-ide
    System.exit(0)
  }

  /**
    * Run VCS in LSP mode.
    */
  private def runLspServer(config: ViperConfig)(implicit executor: VerificationExecutionContext): Unit = {
    try {
      val done = startServer(config)
        .map { case (serverSocket, server) =>
          val url = serverSocket.getInetAddress.getHostAddress
          val port = serverSocket.getLocalPort
          val serverUrl = s"$url:$port"
          announcePort(port)
          println(s"going to listen on port $port for LSP")
          processRequests(serverSocket, server, serverUrl)
        }

      // wait until server is done:
      Await.ready(done, Duration.Inf)
      println("all clients have been processed")
      executor.terminate()
      println("executor service has been shut down")
      System.exit(0)
    } catch {
      case e: IOException =>
        println(s"IOException occurred: ${e.toString}")
        System.exit(1)
    }
  }

  private def startServer(config: ViperConfig)(implicit executor: VerificationExecutionContext): Future[(ServerSocket, ViperServerService)] = {
    val serverSocket = new ServerSocket(config.port())
    val server = new ViperServerService(config)(executor)
    server.start()
      .map(_ => (serverSocket, server))
  }

  private def announcePort(port: Int): Unit = {
    // write port number in a predefined format to standard output such that clients can parse it
    // do not change this format without adapting clients such as the Viper-IDE client
    // regex for parsing: "<ViperServerPort:(\d+)>"
    println(s"<ViperServerPort:$port>")
  }

  private def processRequests(serverSocket: ServerSocket, server: ViperServerService, serverUrl: String)(implicit executor: VerificationExecutionContext): Future[Unit] = {
    var clientFutures: Set[Future[Unit]] = Set.empty

    while (!serverSocket.isClosed) {
      val socket = serverSocket.accept()
      val clientFuture = handleClient(server, socket, serverUrl)
      clientFutures += clientFuture
    }

    Future.sequence[Unit, Set, Set[Unit]](clientFutures)
      .map(_ => ())
  }

  /**
    * processes client requests arriving on `socket`.
    * The returned future completes when stream is closed.
    */
  private def handleClient(server: ViperServerService, socket: Socket, serverUrl: String)(implicit executor: VerificationExecutionContext): Future[Unit] = {
    println(s"client connected: ${socket.toString}")
    val receiver: CustomReceiver = new CustomReceiver(server, serverUrl)
    val launcher = createLauncher(receiver, socket)(executor)
    receiver.connect(launcher.getRemoteProxy)
    // convert Java Future to Scala Future:
    Future {
      launcher.startListening().get()
      println(s"client disconnected: ${socket.toString}")
      receiver.disconnected()
      socket.close()
    }
  }

  private def createLauncher(server: CustomReceiver, socket: Socket)(executor: VerificationExecutionContext): Launcher[IdeLanguageClient] =
    // Launcher.createLauncher cannot be called as we want to pass in an executor service
    // Hence, we directly use Launcher.Builder:
    new Builder[IdeLanguageClient]()
      .setLocalService(server)
      .setRemoteInterface(classOf[IdeLanguageClient])
      .setInput(socket.getInputStream)
      .setOutput(socket.getOutputStream)
      .setExecutorService(executor.executorService)
      .create()
}
