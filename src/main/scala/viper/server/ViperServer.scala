// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server


// === HTTP SERVER =====================================

object ViperServerRunner {

  var viperServerHTTP: ViperHttpServer = _

  /** Start VCS in HTTP mode.
    * */
  def startNewHttpServer(args: Array[String]): Unit = {

    viperServerHTTP = new ViperHttpServer(args)
    viperServerHTTP.start()
  }

  def main(args: Array[String]): Unit = {
    startNewHttpServer(args)
  } // method main
}
