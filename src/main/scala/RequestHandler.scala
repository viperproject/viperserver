/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */


package viper.server

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.LazyLogging
import viper.carbon.CarbonFrontend
import viper.server.RequestHandler.Backend
import viper.silicon.SiliconFrontend
import viper.silver.ast._
import viper.silver.frontend.{SilFrontend, TranslatorState}
import viper.silver.verifier.{AbstractError, Failure, Success, Verifier}

import scala.collection.mutable.ListBuffer

object RequestHandler {

  case class Verify(args: List[String])

  case object Stop

  case object StopRequested

  case class ShowException(e: Exception)

  case class Backend(backend: Verifier)

  case object Stopped

}

class RequestHandler extends Actor with LazyLogging {

  import RequestHandler._

  private var _exception: Exception = null
  private var _sender: ActorRef = null

  private var _worker: Thread = null

  private var _stopRequested = false

  def receive: PartialFunction[Any, Unit] = {
    case Verify("silicon" :: args) =>
      _sender = sender()
      verifySilicon(args, sender())
    case Verify("carbon" :: args) =>
      _sender = sender()
      verifyCarbon(args, sender())
    case Stop =>
      _stopRequested = true
      context stop self
    case Verify(args) => logger.info("invalid arguments: " + args.mkString(" "))
    case _ => logger.info("invalid arguments")
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
      override def run() {
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
    _worker.start()
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
      printOutline(_program.get)
      doVerifyCached()
    }

    _ver.stop()

    finish()
  }

  private def getMethodSpecificErrors(m: Method, errors: Seq[AbstractError]): List[AbstractError] = {
    //The position of the error is used to determine to which Method it belongs.
    val methodStart = m.pos.asInstanceOf[SourcePosition].start.line
    val methodEnd = m.pos.asInstanceOf[SourcePosition].end.get.line

    errors.filter(e => {
      e.pos match {
        case pos: HasLineColumn =>
          val errorPos = pos.line
          errorPos >= methodStart && errorPos <= methodEnd
        case _ =>
          false
      }
    }).toList
  }

  private def removeBody(m: Method): Unit = {
    val node: Stmt = Inhale(FalseLit()())()
    m.body_=(Seqn(Seq(node))(m.pos, m.info))
  }

  def doVerifyCached(): Unit = {
    val (methodsToVerify, methodsToCache, cachedErrors) = consultCache()
    //remove method body of methods to cache
    methodsToCache.foreach(m => {
      removeBody(m)
    })
    val program = _program.get
    val file: String = _config.file()

    val nofCachedMethods = program.methods.length - methodsToVerify.length
    if (nofCachedMethods > 0) {
      logger.info("Cached " + nofCachedMethods + " methods.")
    }

    _verificationResult = Some(mapVerificationResult(_verifier.get.verify(program)))
    assert(_verificationResult != null)

    _state = TranslatorState.Verified

    //update cache
    methodsToVerify.foreach(m => {
      _verificationResult.get match {
        case Failure(errors) =>
          val errorsToCache = getMethodSpecificErrors(m, errors)
          ViperCache.update(file, m, errorsToCache)
          logger.info("Cache " + m.name + (if (errorsToCache.nonEmpty) ": Error" else ": Success"))
        case Success =>
          logger.info("Cache " + m.name + ": Success")
          ViperCache.update(file, m, Nil)
      }
    })

    //combine errors:
    if (cachedErrors.nonEmpty) {
      _verificationResult.get match {
        case Failure(errorList) =>
          _verificationResult = Some(Failure(errorList ++ cachedErrors))
        case Success =>
          _verificationResult = Some(Failure(cachedErrors))
      }
    }
    //TODO: how to clean up the cache? -> Arshavir
    //TODO: what to store in the cache such that the position of the error is updated when only whitespaces change? -> Valentin
    // -> store localization of ast Node
  }

  def consultCache(): (List[Method], List[Method], List[AbstractError]) = {
    val errors: collection.mutable.ListBuffer[AbstractError] = ListBuffer()
    val methodsToVerify: collection.mutable.ListBuffer[Method] = ListBuffer()
    val methodsToCache: collection.mutable.ListBuffer[Method] = ListBuffer()
    //perform caching
    val file: String = _config.file()
    _program.get.methods.foreach(m => {
      ViperCache.get(file, m) match {
        case None => {
          methodsToVerify += m
        }
        case Some(cacheEntry) => {
          if (m.dependencyHash != cacheEntry.dependencyHash) {
            //even if the method itself did not change, a re-verification is required if it's dependencies changed
            methodsToVerify += m
          } else {
            errors ++= cacheEntry.errors
            methodsToCache += m
          }
        }
      }
    })
    (methodsToVerify.toList, methodsToCache.toList, errors.toList)
  }
}

class ViperCarbonFrontend extends CarbonFrontend with ViperFrontend {}

class ViperSiliconFrontend extends SiliconFrontend with ViperFrontend {}