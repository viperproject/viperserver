/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import ch.qos.logback.classic.Logger
import com.typesafe.scalalogging.LazyLogging
import org.rogach.scallop.{ScallopOption, singleArgConverter}
import org.slf4j.LoggerFactory
import viper.server.ViperServerProtocol._
import viper.silicon.Silicon
import viper.silver.frontend.SilFrontendConfig
import viper.silver.verifier.Verifier
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object ViperServerRunner {

  def logger: Logger = LoggerFactory.getLogger(getClass.getName + "_IDE").asInstanceOf[Logger]

  private var _config: ViperConfig = _

  final def config: ViperConfig = _config

  private var _mainActor: ActorRef = null

  final def mainActor: ActorRef = _mainActor

  private var _actorSystem: ActorSystem = null

  final def actorSystem: ActorSystem = _actorSystem

  private var _running = true

  def main(args: Array[String]) {
    try {
      initialize()
      logger.info("This is the Viper Server.")
      parseCommandLine(args)
      while (_running) {
        serve()
      }
    } finally {
      cleanUp()
    }
    sys.exit(0)
  }

  private def initialize(): Unit = {
    //  logger.setLevel(org.apache.log4j.Level.INFO)
    _actorSystem = ActorSystem("MyActorSystem")
    _mainActor = actorSystem.actorOf(Props[MainActor])
  }

  private def cleanUp(): Unit = {
    actorSystem.terminate()
  }

  private def serve(): Unit = {
    val input: String = scala.io.StdIn.readLine()
    if (input == null || input.length == 0) {
      return
    }

    val args = splitCommandLineArgs(input.trim())

    if (args.nonEmpty) {
      if (args.head == "stop") {
        mainActor ! Stop
      } else if (args.head == "exit") {
        mainActor ! Stop
        _running = false
      } else {
        mainActor ! Verify(args)
      }
    }
  }

  private def splitCommandLineArgs(args: String): List[String] = {
    var res = new ListBuffer[String]()
    if (args != null) {
      var last: Char = '\u0000'
      var inQuotes = false
      var arg: StringBuilder = new StringBuilder()

      val trimmedArgs: String = args.trim() + " "

      trimmedArgs foreach {
        case char@'"' =>
          if (inQuotes) {
            if (last == '"') {
              arg += char
              last = '\u0000'
            } else {
              last = char
            }
          } else {
            inQuotes = true
          }
        case char@' ' =>
          if (last == '"') {
            inQuotes = false
          }
          if (inQuotes) {
            arg += char
          } else {
            res += arg.toString()
            arg = new StringBuilder()
          }
          last = char
        case char@c =>
          if (last == '"') {
            inQuotes = false
          }
          arg += char
          last = char
      }
    }
    res.toList
  }

  def parseCommandLine(args: Seq[String]) {
    _config = new ViperConfig(args)
  }
}

class MainActor extends Actor with LazyLogging {
  private var _child: ActorRef = null
  private var _args: List[String] = null
  private var _backend: Verifier = null

  def receive: PartialFunction[Any, Unit] = {
    case Stop =>
      if (_backend != null) {
        try {
          //takes care of background processes
          _backend.stop()
        } catch {
          case e: Exception => throw e
        }
      }
      if (_child != null) {
        try {
          _child ! Stop
        } catch {
          case e: Exception => throw e
        }
      }
      _backend = null
    case Verify(args)
    =>
      if (_child != null) {
        _args = args
        self ! Stop
      } else {
        verify(args)
      }
    case Stopped
    =>
      _child = null
      if (_args != null) {
        val args = _args
        _args = null
        verify(args)
      }
    case Terminated(child) =>
    case Backend(backend) => _backend = backend
    case msg => logger.info("Main Actor: unexpected message received: " + msg)
  }

  def verify(args: List[String]): Unit = {
    assert(_child == null)
    _child = context.actorOf(Props[VerificationActor], "RequestHandlerActor")
    context.watch(_child)
    _child ! Verify(args)
  }
}

class ViperConfig(args: Seq[String]) extends SilFrontendConfig(args, "Viper") {

  val logLevel: ScallopOption[String] = opt[String]("logLevel",
    descr = "One of the log levels ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF (default: OFF)",
    default = Some("WARN"),
    noshort = true,
    hidden = Silicon.hideInternalOptions
  )(singleArgConverter(level => level.toUpperCase))
}

object ViperServerProtocol {

  //Main Actor requests Verification
  case class Verify(args: List[String])

  //VerificationActor sends backend to Main Actor
  case class Backend(backend: Verifier)

  //Main Actor requests verification stop
  case object Stop

  //Verification Actor reports end of verification
  case object Stopped

}