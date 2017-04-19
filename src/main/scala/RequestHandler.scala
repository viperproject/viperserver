/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */


package viper.server

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef}
import viper.carbon.{CarbonFrontend, CarbonVerifier}
import viper.server.RequestHandler.Backend
import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.{SilFrontend, TranslatorState}
import viper.silver.verifier.Verifier

object RequestHandler {

  case class Verify(args: List[String])

  case object Stop

  case object StopRequested

  case class ShowException(e: Exception)

  case class Backend(backend: Verifier)

  case object Stopped

}

class RequestHandler extends Actor{

  import RequestHandler._

  private var _exception: Exception = null
  private var _sender: ActorRef = null

  private var _worker: Thread = null

  private var _stopRequested = false

  def receive = {
    case Verify("silicon" :: args) => {
      _sender = sender()
      verifySilicon(args, sender())
    }
    case Verify("carbon" :: args) => {
      _sender = sender()
      verifyCarbon(args, sender())
    }
    case Stop => {
      _stopRequested = true
      context stop self
    }
    case Verify(args) => println("invalid arguments: " + args.mkString(" "))
    case _ => println("invalid arguments")
  }

  override def postStop(): Unit = {
    if (_worker != null) {
      _worker.interrupt()
    }
  }

  def verifySilicon(args: List[String], sender: ActorRef): Unit = {
    val frontend = new ViperSiliconFrontend()
    frontend.setSender(sender)
    startVerification(args, frontend)
  }

  private def startVerification(args: List[String], frontend: ViperFrontend): Unit = {
    //verify in another thread
    _worker = new Thread {
      override def run {
        try {
          frontend.execute(args)
        } catch {
          case e: Exception => {
            if (!_stopRequested) {
              e.printStackTrace(System.err)
            }
          }
        } finally {
          //stop worker after completed verification
          if (context != null) {
            context stop self
          }
          _sender ! Stopped
          frontend.printStopped()
        }
      }
    }
    _worker.start
  }

  def verifyCarbon(args: List[String], sender: ActorRef): Unit = {
    val frontend = new ViperCarbonFrontend()
    frontend.setSender(sender)
    startVerification(args, frontend)
  }
}

trait ViperFrontend extends SilFrontend {
  protected var _sender: ActorRef = null

  def setSender(sender: ActorRef): Unit = {
    _sender = sender
  }

  def printStopped(): Unit = {
    if (config.ideMode()) {
      loggerForIde.info(s"""{"type":"Stopped"}\r\n""")
    } else {
      logger.info(s"${_ver.name} stopped")
    }
  }

  override def execute(args: Seq[String]) {
    setStartTime()

    /* Create the verifier */
    _ver = createVerifier(args.mkString(" "))

    _sender ! Backend(_ver)

    if (!prepare(args)) return

    // initialize the translator
    init(_ver)

    // set the file we want to verify
    reset(Paths.get(_config.file()))

    // run the parser, typechecker, and verifier
    parse()
    typecheck()
    translate()

    if (_errors.nonEmpty) {
      _state = TranslatorState.Verified
    } else {
      doVerify()
    }

    _ver.stop()

    finish()
  }
}

class ViperCarbonFrontend extends CarbonFrontend with ViperFrontend {
}

class ViperSiliconFrontend extends SiliconFrontend with ViperFrontend {
}
