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
}

class RequestHandler extends Actor {
  import RequestHandler._

  private var _exception: Exception = null
  private var _sender: ActorRef = null

  def receive = {
    case Verify("silicon" :: args) => {
      _sender = sender()
      verifySilicon(args, sender())
      context stop self
    }
    case Verify("carbon" :: args) => {
      _sender = sender()
      verifyCarbon(args, sender())
      context stop self
    }
    case Stop =>{
      context stop self
    }
    case Verify(args) => println("invalid arguments: " + args.mkString(" "))
    case _ => println("invalid arguments")
  }

  override def postStop(): Unit = {
    //println("stopped")
    //print("> ")
  }

  def verifySilicon(args: List[String],sender: ActorRef): Unit ={
    try {
      val frontend = new ViperSiliconFrontend()
      frontend.setSender(sender)
      frontend.execute(args)
    }catch {
      case e: Exception => {
        _sender ! ShowException(e)
      }
    }
  }

  def verifyCarbon(args: List[String],sender: ActorRef): Unit ={
    val frontend = new ViperCarbonFrontend()
    frontend.setSender(sender)
    frontend.execute(args)
  }
}


class ViperCarbonFrontend extends CarbonFrontend with Sender{
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
    }else {
      doVerify()
    }

    _ver.stop()

    finish()
  }
}

trait Sender {
  protected var _sender:ActorRef = null

  def setSender(sender:ActorRef): Unit ={
    _sender = sender
  }
}

class ViperSiliconFrontend extends SiliconFrontend with Sender{
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
    }else {
      doVerify()
    }

    _ver.stop()

    finish()
  }
}