/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2020 ETH Zurich.
  */

import viper.server.frontends.http.ViperHttpServer

object HttpServerRunner {
  var viperServerHttp: ViperHttpServer = _

  /** Start VCS in HTTP mode.
    * */
  def startHttpServer(args: Array[String]): Unit = {
    viperServerHttp = new ViperHttpServer(args)
    viperServerHttp.start()
  }

  def main(args: Array[String]): Unit = {
    startHttpServer(args)
  }
}