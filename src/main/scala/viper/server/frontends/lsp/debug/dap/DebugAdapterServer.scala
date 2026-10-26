// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug.dap

import org.eclipse.lsp4j.debug.launch.DSPLauncher
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.ClientCoordinator

import java.net.{ServerSocket, SocketTimeoutException}
import scala.util.{Failure, Success, Try}

/**
  * Opens a socket for a single Debug Adapter Protocol connection and serves a [[ViperDebugAdapter]] on it.
  *
  * Like the LSP socket of ViperServer, the socket listens on all interfaces, so that an IDE attached to a
  * remote ViperServer can reach it. It accepts exactly one connection and stops listening afterwards, or after
  * [[acceptTimeoutMs]] if nobody connects.
  */
object DebugAdapterServer {

  val acceptTimeoutMs: Int = 30000

  /** Starts listening and returns the port, or why no socket could be opened. */
  def start(coordinator: ClientCoordinator)(implicit executor: VerificationExecutionContext): Try[Int] =
    Try(new ServerSocket(0)).map { serverSocket =>
      serverSocket.setSoTimeout(acceptTimeoutMs)
      val thread = new java.lang.Thread(() => serve(serverSocket, coordinator), "viper-debug-adapter")
      thread.setDaemon(true)
      thread.start()
      serverSocket.getLocalPort
    }

  private def serve(serverSocket: ServerSocket, coordinator: ClientCoordinator)
                   (implicit executor: VerificationExecutionContext): Unit = {
    val accepted = try Success(serverSocket.accept()) catch {
      case e: SocketTimeoutException => Failure(e)
      case e: Throwable => Failure(e)
    } finally {
      Try(serverSocket.close())
    }

    accepted match {
      case Failure(e) =>
        coordinator.logger.info(s"No debug adapter client connected on port ${serverSocket.getLocalPort}: $e")
      case Success(socket) =>
        coordinator.logger.info(s"Debug adapter client connected: $socket")
        val adapter = new ViperDebugAdapter(coordinator)
        try {
          val launcher = DSPLauncher.createServerLauncher(adapter, socket.getInputStream, socket.getOutputStream)
          adapter.connect(launcher.getRemoteProxy)
          // Blocks until the client closes the connection, which it does after `disconnect`.
          launcher.startListening().get()
        } catch {
          case e: Throwable => coordinator.logger.debug(s"The debug adapter connection ended: $e")
        } finally {
          adapter.connectionClosed()
          Try(socket.close())
          coordinator.logger.info("Debug adapter client disconnected")
        }
    }
  }
}
