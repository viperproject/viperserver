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
import viper.server.frontends.lsp.{Coordinator, CustomReceiver, IdeLanguageClient}

import scala.concurrent.Await
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
  private def runLspServer(config: ViperConfig)(executor: VerificationExecutionContext): Unit = {
    try {
      val serverSocket = new ServerSocket(config.port())
      announcePort(serverSocket.getLocalPort)
      println(s"going to listen on port ${serverSocket.getLocalPort} for LSP")

      Coordinator.port = serverSocket.getLocalPort
      Coordinator.url = serverSocket.getInetAddress.getHostAddress

      val socket = serverSocket.accept()
      println(s"client got connected")
      // TODO add support for multiple clients connecting to this server

      val server: CustomReceiver = new CustomReceiver()(executor)
      val launcher = createLauncher(server, socket)(executor)
      server.connect(launcher.getRemoteProxy)
      // start listening on input stream in a new thread:
      val future = launcher.startListening()
      // wait until stream is closed again
      future.get()
      println("listener thread from server has stopped")
      executor.terminate()
      println("executor service has been shut down")
      System.exit(0)
    } catch {
      case e: IOException => {
        println(s"IOException occurred: ${e.toString}")
        System.exit(1)
      }
    }
  }

  private def announcePort(port: Int): Unit = {
    // write port number in a predefined format to standard output such that clients can parse it
    // do not change this format without adapting clients such as the Viper-IDE client
    // regex for parsing: "<ViperServerPort:(\d+)>"
    println(s"<ViperServerPort:$port>")
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
