// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server

import java.io.IOException
import java.net.Socket

import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient
import viper.server.frontends.http.ViperHttpServer
import viper.server.frontends.lsp.{Coordinator, CustomReceiver, IdeLanguageClient}

object ViperServerRunner {

  var viperServerHTTP: ViperHttpServer = _

  /** Start VCS in HTTP mode.
    * */
  def startHttpServer(args: Array[String]): Unit = {

    viperServerHTTP = new ViperHttpServer(args)
    viperServerHTTP.start()
  }

  def main(args: Array[String]): Unit = {
    try {
      val port = Integer.parseInt(args.head)
      runServer(port)
    } catch {
      case _: NoSuchElementException => {
        println("No port number provided")
        sys.exit(1)
      }
      case _: NumberFormatException => {
        println("Invalid port number")
        sys.exit(1)
        return
      }
    }
  }

  def runServer(port: Int): Unit = {
    // start listening on port
    try {
      val socket = new Socket("localhost", port)
      val localAddress = socket.getLocalAddress.getHostAddress
      println(s"going to listen on $localAddress:$port")

      Coordinator.port = port
      Coordinator.url = localAddress


      val server: CustomReceiver = new CustomReceiver()
      val launcher = Launcher.createLauncher(server, classOf[IdeLanguageClient], socket.getInputStream, socket.getOutputStream)
      server.connect(launcher.getRemoteProxy)

      // start listening on input stream in a new thread:
      val fut = launcher.startListening()
      // wait until stream is closed again
      fut.get()
    } catch {
      case e: IOException => println(s"IOException occurred: ${e.toString}")
    }
  }
}
