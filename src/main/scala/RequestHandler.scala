/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef}
import viper.carbon.CarbonFrontend
import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.SilFrontend
import viper.silver.verifier.Verifier

object RequestHandler {
  case class Verify(args:List[String])
  case object Stop
  case class Backend(backend:Verifier)
}

class RequestHandler extends Actor {
  import RequestHandler._

  private var _backend:Verifier = _

  private var _sender:ActorRef = null

  def receive = {
    case Verify("silicon" :: args) => {
      _sender = sender()
      verifySilicon(args)
      context stop self
    }
    case Verify("carbon" :: args) => {
      _sender = sender()
      verifyCarbon(args)
      context stop self
    }
    case Stop =>{
      //println("stop")
      context stop self
    }
    case Verify(args) => println("invalid arguments: " + args.mkString(" "))
    case _ => println("invalid arguments")
  }

  override def postStop(): Unit = {
    //println("stopped")
    //print("> ")
  }

  def verifySilicon(args: List[String]): Unit ={
    val frontend = new SiliconFrontend()
    verify(frontend, args)
  }

  def verifyCarbon(args: List[String]): Unit ={
    val frontend = new CarbonFrontend()
    verify(frontend, args)
  }

  def verify(frontend:SilFrontend, args: List[String]) {
    try{
      frontend.setStartTime()
      val backend = frontend.createVerifier(args.mkString(" "))
      _sender ! Backend(backend)

      frontend.setVerifier(backend)

      frontend.prepare(args)
      frontend.init(backend)

      val fileName = frontend.config.file()
      val file = Paths.get(fileName)

      frontend.reset(file)
      frontend.parse()
      frontend.typecheck()
      frontend.translate()

      val targetNode: Program = frontend.translatorResult

      frontend.doVerify()
      frontend.finish()

    } catch {
      case e => println("Error: " + e)
    }
  }
}
