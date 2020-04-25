/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package  viper.server

import java.io.File

import org.rogach.scallop.{ScallopConf, ScallopOption, singleArgConverter}
import viper.server.utility.ibm
import viper.server.utility.ibm.Socket

class ViperConfig(args: Seq[String]) extends ScallopConf(args) {

  private val logging_levels = Array("ALL", "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "OFF")

  val logLevel: ScallopOption[String] = opt[String]("logLevel",
    descr = s"One of the log levels: ${logging_levels.mkString(",")}.",
    default = Some("ERROR"),
    validate = (ll: String) => logging_levels.contains(ll.toUpperCase),
    noshort = true,
    hidden = false
  )(singleArgConverter(level => level.toUpperCase))

  val logFile: ScallopOption[String] = opt[String]("logFile", 'l',
    descr = "Specifies the location of the log file to be used by ViperServer and the verification " +
            "backends it creates.",
    default = {
      val temp: File = java.io.File.createTempFile("viperserver_journal_" + System.currentTimeMillis(), ".log")
      Some(temp.getAbsolutePath)
    },
    validate = (path: String) => {
      val f = canonizedLogFile(path)
      (f.isFile || f.isDirectory) && f.canWrite || f.getParentFile.canWrite
    },
    noshort = true,
    hidden = false)

  private def canonizedLogFile(some_file_path: String): File = {
    val f = new File(some_file_path)
    if (f.isAbsolute) {
      f
    } else {
      java.nio.file.Paths.get(System.getProperty("user.dir"), some_file_path).toFile
    }
  }
  def getLogFileWithGuarantee: String = {
    val cf: File = canonizedLogFile(logFile())
    if ( cf.isDirectory ) {
      val log: File = java.io.File.createTempFile("viperserver_journal_" + System.currentTimeMillis(), ".log", cf)
      log.toString
    } else {
      cf.toString
    }
  }

  @deprecated
  val ideMode: ScallopOption[Boolean] = opt[Boolean]("ideMode",
    descr = ("Used for VS Code IDE. Report errors in json format, and write"
      + "errors in the format '<file>,<line>:<col>,<line>:<col>,<message>' to"
      + "a file (see option ideModeErrorFile)."),
    default = Some(false),
    noshort = true,
    hidden = false
  )

  val backendSpecificCache: ScallopOption[Boolean] = opt[Boolean]("backendSpecificCache",
    descr = "Use a separate cache for each backend?",
    default = Some(false),
    noshort = true,
    hidden = false
  )

  @deprecated
  val ideModeAdvanced: ScallopOption[Boolean] = opt[Boolean]("ideModeAdvanced",
    descr = ("Used for VS Code IDE. Write symbolic execution log into .vscode/executionTreeData.js file, "
      + "write execution tree graph into .vscode/dot_input.dot, "
      + "and output z3's counter example models."),
    default = Some(false),
    noshort = true,
    hidden = true
  )

  dependsOnAll(ideModeAdvanced, ideMode :: Nil)

  var bindInterface: ScallopOption[String] = opt[String]("bind-interface",
    descr = ("Specifies the interface that ViperServer will listen on."
      + "The default is \"localhost\" where only connections from the local machine are accepted."
      + "Use 0.0.0.0 to accept connections from any machine (not a good idea without HTTPS or a firewall)."),
    default = Some("localhost"),
    noshort=true
  )

  val port: ScallopOption[Int] = opt[Int]("port", 'p',
    descr = ("Specifies the port on which ViperServer will be started."
      + s"The port must be an integer in range [${Socket.MIN_PORT_NUMBER}-${ibm.Socket.MAX_PORT_NUMBER}]"
      + "If the option is omitted, an available port will be selected automatically."),
    default = Some(ibm.Socket.findFreePort),
    validate = p => try {
      ibm.Socket.available(p)
    } catch {
      case e: Exception =>
        println(s"Invalid port $p: $e")
        false
    },
    noshort = false,
    hidden = false)
}
