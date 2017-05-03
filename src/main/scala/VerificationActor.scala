/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import java.nio.file.Paths

import com.typesafe.scalalogging.LazyLogging
import viper.carbon.CarbonFrontend
import viper.silicon.SiliconFrontend
import viper.silver.ast._
import viper.silver.ast.utility.Visitor
import viper.silver.frontend.{SilFrontend, TranslatorState}
import viper.silver.verifier._

import scala.collection.mutable.ListBuffer

class VerificationWorker(val command: Any) extends Runnable with LazyLogging {

  import ViperServerProtocol._

  //private var _sender: ActorRef = null

  //private var _worker: Thread = null

  //private var _stopRequested = false

  private var _frontend: ViperFrontend = null

  def run(): Unit = {
    try {
      command match {
        case Verify("silicon" :: args) =>
          startVerification(args, new ViperSiliconFrontend())
        case Verify("carbon" :: args) =>
          startVerification(args, new ViperCarbonFrontend())
        case Verify(args) =>
          logger.info("invalid arguments: " + args.mkString(" "))
        case _ =>
          logger.info("invalid arguments")
      }
    } catch {
      case e: InterruptedException =>
      case e: Exception =>
        e.printStackTrace(System.err)
    } finally {
      stop()
      if (_frontend != null) {
        _frontend.printStopped()
      } else {
        ViperFrontend.printStopped()
      }
    }
  }

  private def startVerification(args: List[String], frontend: ViperFrontend): Unit = {
    //frontend.setSender(sender)
    _frontend = frontend
    frontend.execute(args)
  }

  private def stop(): Unit = {
    try {
      _frontend.verifier.stop()
    } catch {
      case _: Throwable =>
    }
  }
}

object ViperFrontend extends ViperSiliconFrontend {
  override def ideMode: Boolean = ViperServerRunner.config.ideMode()
}

trait ViperFrontend extends SilFrontend {
  //protected var _sender: ActorRef = null

  //def setSender(sender: ActorRef): Unit = {
  //  _sender = sender
  //}

  def ideMode: Boolean = config.ideMode()

  def printStopped(): Unit = {
    if (ideMode) {
      loggerForIde.info(s"""{"type":"Stopped"}\r\n""")
    } else {
      logger.info(s"${_ver.name} stopped")
    }
  }

  override def execute(args: Seq[String]) {
    setStartTime()

    /* Create the verifier */
    _ver = createVerifier(args.mkString(" "))

    //_sender ! Backend(_ver)

    if (!prepare(args)) return

    // initialize the translator
    init(_ver)

    // set the file we want to verify
    reset(Paths.get(config.file()))

    // run the parser, typechecker, and verifier
    parse()
    typecheck()
    translate()

    if (_errors.nonEmpty) {
      _state = TranslatorState.Verified
    } else {
      printOutline(_program.get)
      if (config.disableCaching()) {
        doVerify()
      } else {
        doVerifyCached()
      }
    }

    _ver.stop()

    finish()
  }

  private def getMethodSpecificErrors(m: Method, errors: Seq[AbstractError]): List[VerificationError] = {
    //The position of the error is used to determine to which Method it belongs.
    val methodStart = m.pos.asInstanceOf[SourcePosition].start.line
    val methodEnd = m.pos.asInstanceOf[SourcePosition].end.get.line
    val result = scala.collection.mutable.ListBuffer[VerificationError]()

    errors.foreach {
      case e: VerificationError =>
        e.pos match {
          case pos: HasLineColumn =>
            val errorPos = pos.line
            if (errorPos >= methodStart && errorPos <= methodEnd) result += e
          case _ =>
            throw new Exception("Error determining method specific errors for the cache: The reported errors should have a location")
        }
    }
    result.toList
  }

  private def removeBody(m: Method): Unit = {
    val node: Stmt = Inhale(FalseLit()())()
    m.body = Seqn(Seq(node))(m.pos, m.info)
  }

  def doVerifyCached(): Unit = {

    //fill in the entityHashes into the new AST
    _program.get.computeEntityHashes()

    val (methodsToVerify, methodsToCache, cachedErrors) = consultCache()

    //remove method body of methods to cache
    methodsToCache.foreach(removeBody)

    val program = _program.get
    val file: String = _config.file()

    _verificationResult = Some(mapVerificationResult(_verifier.get.verify(program)))
    assert(_verificationResult != null)

    _state = TranslatorState.Verified

    //update cache
    methodsToVerify.foreach(m => {
      _verificationResult.get match {
        case Failure(errors) =>
          val errorsToCache = getMethodSpecificErrors(m, errors)
          ViperCache.update(file, m, errorsToCache)
          logger.trace("Store in cache " + m.name + (if (errorsToCache.nonEmpty) ": Error" else ": Success"))
        case Success =>
          logger.trace("Store in cache " + m.name + ": Success")
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
  }

  def consultCache(): (List[Method], List[Method], List[VerificationError]) = {
    val errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
    val methodsToVerify: collection.mutable.ListBuffer[Method] = ListBuffer()
    val methodsToCache: collection.mutable.ListBuffer[Method] = ListBuffer()

    val file: String = _config.file()
    val program = _program.get

    //read errors from cache
    program.methods.foreach((m: Method) => {
      ViperCache.get(file, m) match {
        case None =>
          methodsToVerify += m
        case Some(cacheEntry) =>
          if (m.dependencyHash != cacheEntry.dependencyHash) {
            //even if the method itself did not change, a re-verification is required if it's dependencies changed
            methodsToVerify += m
          } else {
            errors ++= updateErrorLocation(m, cacheEntry.errors)
            methodsToCache += m
          }
      }
    })
    (methodsToVerify.toList, methodsToCache.toList, errors.toList)
  }

  private def updateErrorLocation(m: Method, errors: List[VerificationError]): List[VerificationError] = {
    errors.map(updateErrorLocation(m, _))
  }

  private def findCorrespondingNode(method: Method, hash: String): Option[errors.ErrorNode] = {
    method.subnodes.foreach(node => {
      Visitor.visit(node, (n: Node) => n.subnodes)({ case n: errors.ErrorNode =>
        if (n.info.entityHash == hash)
          return Some(n)
      })
    })
    None
  }

  private def updateErrorLocation(m: Method, error: VerificationError): VerificationError = {
    if (error.offendingNode == null) return error
    val hash: String = error.offendingNode.info.entityHash
    val reasonHash: String = error.reason.offendingNode.info.entityHash
    assert(hash != null)

    //get the corresponding offending node in the new AST
    val offendingNode = findCorrespondingNode(m, hash)
    val reasonOffendingNode = findCorrespondingNode(m, reasonHash)
    //create a new VerificationError that only differs in its offending Node.
    offendingNode match {
      case Some(n: errors.ErrorNode) =>
        val updatedError = error.updateNode(n, reasonOffendingNode.get)
        updatedError.cached = true
        return updatedError
      case None =>
        throw new Exception("Error updating the location of cached errors: No corresponding Node found")
    }
    null
  }
}

class ViperCarbonFrontend extends CarbonFrontend with ViperFrontend {}

class ViperSiliconFrontend extends SiliconFrontend with ViperFrontend {}