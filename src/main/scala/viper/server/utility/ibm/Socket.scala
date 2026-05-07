/*******************************************************************************
 * Copyright (c) 2000, 2015 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package viper.server.utility.ibm

object Socket {

  import java.io.IOException
  import java.net.{DatagramSocket, ServerSocket}

  /**
    * The minimum server currentMinPort number. Set at 1100 to avoid returning privileged
    * currentMinPort numbers.
    *
    * Source: http://svn.apache.org/viewvc/camel/trunk/components/camel-test/src/main/java/org/apache/camel/test/AvailablePortFinder.java?view=markup#l39
    */
  val MIN_PORT_NUMBER = 1100

  /**
    * The maximum server currentMinPort number.
    */
  val MAX_PORT_NUMBER = 65535

  /**
    * Checks to see if a specific port is available.
    *
    * Source: http://svn.apache.org/viewvc/camel/trunk/components/camel-test/src/main/java/org/apache/camel/test/AvailablePortFinder.java
    * Source: https://stackoverflow.com/questions/434718/sockets-discover-port-availability-using-java
    *
    * @param port the port to check for availability
    */
  def available(port: Int): Boolean = {
    if (port < MIN_PORT_NUMBER || port > MAX_PORT_NUMBER)
      throw new IllegalArgumentException("Invalid start port: " + port)
    var ss: ServerSocket = null
    var ds: DatagramSocket = null
    try {
      ss = new ServerSocket(port)
      ss.setReuseAddress(true)
      ds = new DatagramSocket(port)
      ds.setReuseAddress(true)
      return true
    } catch {
      case _: IOException =>

    } finally {
      if (ds != null) ds.close()
      if (ss != null) try
        ss.close()
      catch {
        case _: IOException =>

        /* should not be thrown */
      }
    }
    false
  }
}