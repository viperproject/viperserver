/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package viper.server.utility.apache

import java.io.IOException
import java.net.{DatagramSocket, ServerSocket}

object Socket {

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