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
import viper.viperserver.BuildInfo

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object ViperServerRunner {

  var viperServerHttp: ViperHttpServer = _

  def main(args: Array[String]): Unit = {
    // Banner emitted before the server (and thus its logger) is constructed.
    println(s"${BuildInfo.projectName} ${BuildInfo.projectVersionExtended}")
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
    viperServerHttp.globalLogger.info("HTTP server has been stopped")
    executor.terminate()
    viperServerHttp.globalLogger.info("executor service has been shut down")
    // the following `exit` call is required such that the server eventually terminates for `longDuration.vpr` in the
    // test suite of viper-ide
    System.exit(0)
  }

  /**
    * Run VCS in LSP mode.
    */
  private def runLspServer(config: ViperConfig)(implicit executor: VerificationExecutionContext): Unit = {
    var serverOpt: Option[ViperServerService] = None
    try {
      val done = startServer(config)
        .flatMap { case (serverSocket, server) =>
          serverOpt = Some(server)
          val url = serverSocket.getInetAddress.getHostAddress
          val port = serverSocket.getLocalPort
          val serverUrl = s"$url:$port"
          announcePort(port)
          server.globalLogger.info(s"going to listen on port $port for LSP")
          processRequests(config, serverSocket, server, serverUrl)
        }

      // wait until server is done:
      Await.result(done, Duration.Inf)
      serverOpt.foreach(_.globalLogger.info("all clients have been processed"))
      executor.terminate()
      serverOpt.foreach(_.globalLogger.info("executor service has been shut down"))
      System.exit(0)
    } catch {
      case e: IOException =>
        // Logger may not exist yet if startup itself failed; use stderr.
        System.err.println(s"IOException occurred: ${e.toString}")
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
    // Machine-parsed contract — must stay on stdout in this exact format.
    // Clients (e.g. Viper-IDE) match the regex "<ViperServerPort:(\d+)>".
    println(s"<ViperServerPort:$port>")
  }

  private def processRequests(config: ViperConfig, serverSocket: ServerSocket, server: ViperServerService, serverUrl: String)(implicit executor: VerificationExecutionContext): Future[Unit] = {
    var clientFutures: Seq[Future[Unit]] = Seq.empty

    do {
      val socket = serverSocket.accept()
      val clientFuture = handleClient(config, server, socket, serverUrl)
      clientFutures = clientFutures :+ clientFuture
    } while (!serverSocket.isClosed && !config.singleClientMode())

    Future.sequence(clientFutures).map(_ => ())
  }

  /**
    * processes client requests arriving on `socket`.
    * The returned future completes when stream is closed.
    */
  private def handleClient(config: ViperConfig, server: ViperServerService, socket: Socket, serverUrl: String)(implicit executor: VerificationExecutionContext): Future[Unit] = {
    server.globalLogger.info(s"client connected: ${socket.toString}")
    val receiver: CustomReceiver = new CustomReceiver(config, server, serverUrl)
    val launcher = createLauncher(receiver, socket)(executor)
    receiver.connect(launcher.getRemoteProxy)
    // convert Java Future to Scala Future:
    Future {
      launcher.startListening().get()
      server.globalLogger.info(s"client disconnected: ${socket.toString}")
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
