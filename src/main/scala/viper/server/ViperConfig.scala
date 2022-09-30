// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server

import java.io.File

import org.rogach.scallop.{ScallopConf, ScallopOption, numberHandler, singleArgConverter}
import viper.server.core.DefaultVerificationExecutionContext
import viper.server.utility.Helpers.{canonizedFile, validatePath}
import viper.server.utility.ibm

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
    validate = validatePath,
    noshort = true,
    hidden = false)

  def getLogFileWithGuarantee: String = {
    val cf: File = canonizedFile(logFile())
    if ( cf.isDirectory ) {
      val log: File = java.io.File.createTempFile("viperserver_journal_" + System.currentTimeMillis(), ".log", cf)
      log.toString
    } else {
      cf.toString
    }
  }

  val backendSpecificCache: ScallopOption[Boolean] = opt[Boolean]("backendSpecificCache",
    descr = "Use a separate cache for each backend?",
    default = Some(false),
    noshort = true,
    hidden = false
  )

  val SERVER_MODE_LSP = "LSP"
  val SERVER_MODE_HTTP = "HTTP"
  private val server_modes = Array(SERVER_MODE_LSP, SERVER_MODE_HTTP)

  val serverMode: ScallopOption[String] = opt[String]("serverMode",
    descr = s"One of the supported protocols: ${server_modes.mkString(",")}.",
    default = Some(SERVER_MODE_HTTP),
    validate = (mode: String) => server_modes.contains(mode.toUpperCase),
    noshort = true,
    hidden = false
  )(singleArgConverter(mode => mode.toUpperCase))

  val singleClientMode: ScallopOption[Boolean] = opt[Boolean]("singleClient",
    descr = "Handles only a single client in LSP mode and terminates automatically afterwards",
    // single client can only be specified for LSP mode:
    validate = input => if (input) {
      val isLSP = serverMode.map(_ == SERVER_MODE_LSP).getOrElse(false)
      if (!isLSP) {
        println(s"${singleClientMode.name} is only valid in server mode '$SERVER_MODE_LSP'")
      }
      isLSP
    } else true,
    default = Some(false),
    noshort = true
  )

  val disableClientVersionCheck: ScallopOption[Boolean] = opt[Boolean]("disableVersionCheck",
    descr = "Disables the client's version check.",
    // version check can only be disable for LSP mode:
    validate = input => if (input) {
      val isLSP = serverMode.map(_ == SERVER_MODE_LSP).getOrElse(false)
      if (!isLSP) {
        println(s"${disableClientVersionCheck.name} is only valid in server mode '$SERVER_MODE_LSP'")
      }
      isLSP
    } else true,
    default = Some(false),
    noshort = true
  )

  val port: ScallopOption[Int] = opt[Int]("port", 'p',
    descr = ("Specifies the port on which ViperServer will be started."
      + s"The port must be an integer in range [${ibm.Socket.MIN_PORT_NUMBER}-${ibm.Socket.MAX_PORT_NUMBER}]"
      + "If the option is omitted, an available port will be selected automatically."),
    validate = p => try {
      if (p != 0) {
        ibm.Socket.available(p)
      } else {
        true
      }
    } catch {
      case e: Exception =>
        println(s"Invalid port $p: $e")
        false
    },
    default = Some(0),
    noshort = false,
    hidden = false)

  val actorCommunicationTimeout: ScallopOption[Int] = opt[Int]("actorCommunicationTimeout", 'a',
    descr = ("Specifies the maximal amount of time that actors will wait when communicating requesting messages from other actors."
      + s"The number is of unit milliseconds and must be positive integer."
      + "If the option is omitted, a default timeout of 5000 milliseconds will be set."),
    default = Some(5000),
    noshort = false,
    hidden = true
  )

  val maximumActiveJobs: ScallopOption[Int] = opt[Int]("maximumActiveJobs", 'm',
    descr = ("Specifies the maximal amount of jobs that may run concurrently."
      + s"The number must be positive integer."
      + "If the option is omitted, a default number of 3 jobs will be set."),
    default = Some(3),
    noshort = false,
    hidden = false
  )

  val cacheFile: ScallopOption[String] = opt[String]("cacheFile",
    descr = ("Specifies the file from which the cache gets initialized on startup and to which the resulting cache gets written to."
      + "If it is not set, the cache will be initially empty and is only kept in memory, so it is not persisted during runs"
      ),
    default = None,
    noshort = true,
    hidden = false
  )

  val nThreads: ScallopOption[Int] = opt[Int](
    name = "nThreads",
    descr = s"Maximal number of threads that should be used (not taking threads used by backend into account)\n" +
      s"Values below ${DefaultVerificationExecutionContext.minNumberOfThreads} (the minimum) will be set to the minimum.\n" +
      s"The default value is the maximum of ${DefaultVerificationExecutionContext.minNumberOfThreads} and the number of available processors",
    default = Some(Math.max(DefaultVerificationExecutionContext.minNumberOfThreads, Runtime.getRuntime.availableProcessors())),
    noshort = true
  )(singleArgConverter(input => {
    // parse option as int and check bounds
    val n = input.toInt
    n match {
      case n if n < DefaultVerificationExecutionContext.minNumberOfThreads => DefaultVerificationExecutionContext.minNumberOfThreads
      case n => n
    }
  }, numberHandler("Int")))

  /**
    * Exception handling
    */
  /**
    * Epilogue
    */

  verify()
}
