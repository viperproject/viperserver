package viper.server.utility

import java.io.IOException
import java.net.ServerSocket

object Sockets {
  /**
    * Returns a free port number on localhost.
    *
    * Heavily inspired from org.eclipse.jdt.launching.SocketUtil (to avoid a dependency to JDT just because of this).
    * Slightly improved with close() missing in JDT. And throws exception instead of returning -1.
    *
    * Source: https://gist.github.com/vorburger/3429822
    * TODO: check license
    *
    * @return a free port number on localhost
    * @throws IllegalStateException if unable to find a free port
    */
  def findFreePort: Int = {
    var socket: ServerSocket = null
    try {
      socket = new ServerSocket (0)
      socket.setReuseAddress (true)
      val port: Int = socket.getLocalPort
      try {
        socket.close ()
      } catch {
        case e: IOException =>

        // Ignore IOException on close()
      }
      return port
    } catch {
      case e: IOException =>

    } finally {
      if (socket != null) {
        try {
          socket.close ()
        } catch {
          case e: IOException =>

        }
      }
    }
    throw new IllegalStateException ("Could not find a free TCP/IP port to start embedded Jetty HTTP Server on")
  }
}