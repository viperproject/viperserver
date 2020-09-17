// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server

import org.rogach.scallop.ScallopOption

import scala.concurrent.ExecutionContext


// === HTTP SERVER =====================================

object ViperServerRunner {

  var newViperHttpServer: ViperHttpServer = _

  /** Creates batch script to run a <a href="https://github.com/viperproject/viper_client">viper_client</a> written in python.
    * */
  private def writeBatchScripts(portOption: ScallopOption[Int], file: Option[String]): Unit = {
    if(!portOption.isDefined){
      println("port was not defined, batch files won't be created.")
      return
    }

    val port = portOption.apply()

    import java.io.{File, FileWriter}

    val term_file = new File("ter.bat")
    val ver_file = new File("ver.bat")

    val term_writer = new FileWriter(term_file)
    val ver_writer = new FileWriter(ver_file)

    term_writer.write(s"cd ..\\viper_client-master & python client.py -c terminate -p $port & cd ..\\viperserver")
    file match {
      case Some(fileName) =>
        ver_writer.write(s"cd ..\\viper_client-master & " +
          s"python client.py -c verify -p $port -f ${fileName} &" +
          s"cd ..\\viperserver")
      case None =>
        ver_writer.write(s"cd ..\\viper_client-master & python client.py -c verify -p $port & cd ..\\viperserver")
    }

    term_writer.close()
    ver_writer.close()
  }

  /** Start VCS in HTTP mode.
    * */
  def startNewHttpServer(args: Array[String]): Unit = {
    implicit val executionContext = ExecutionContext.global
    val config = new ViperConfig(args)
    config.verify()

    newViperHttpServer = new ViperHttpServer(config)
    newViperHttpServer.start()

    writeBatchScripts(config.port, Some("sum_method.vpr"))
  }

  def main(args: Array[String]): Unit = {
    startNewHttpServer(args)
  } // method main
}
