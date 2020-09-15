// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server

import akka.NotUsed
import akka.actor.PoisonPill
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.scaladsl.Source
import akka.util.Timeout
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, TranslateAlloyToKodkod}
import viper.server.ViperRequests.{AlloyGenerationRequest, CacheResetRequest, VerificationRequest}
import viper.server.core.{JobID, ViperCache, ViperCoreServer}
import viper.server.protocol.ViperIDEProtocol._
import viper.server.protocol.ViperServerProtocol
import viper.server.protocol.ViperServerProtocol._
import viper.server.utility.AstGenerator
import viper.silver.logger.ViperLogger
import viper.silver.reporter._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


class ViperHttpServer(private val _args: Array[String]) extends ViperCoreServer(_args) {

  override def start(): Unit = {
    init(Some(routes))
  }
 
  def routes(logger: ViperLogger): Route = {
    
    /**
      * Send GET request to "/exit".
      *
      * This will do the following:
      * 1. Map all existing jobs from [[_jobHandles]] to a list of futures based on the responses of instances of
      *    [[JobActor]] from [[Stop]] messages (with a 5 second timeout)
      * 2. Merge the list into an overall future
      * 3. Wait for the overall future to complete or timeout
      * 4. Send [[Terminator.Exit]] message to the [[Terminator]] actor
      * 5. Complete the request with an appropriate message
      *
      * Some use cases:
      * - The IDE is being shut down
      * - ViperServer receives a deadly signal and needs to kill the running verification jobs
      */
    path("exit") {
      get {
        onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
          err match {
            case Success(_) =>
              _termActor ! Terminator.Exit
              complete( ServerStopConfirmed("shutting down...") )
            case Failure(err_msg) =>
              logger.get.error(s"Interrupting one of the verification threads timed out: $err_msg")
              _termActor ! Terminator.Exit
              complete( ServerStopConfirmed("forcibly shutting down...") )
          }
        }
      }
    }
  } ~ path("verify") {
    /**
      * Send POST request to "/verify".
      *
      * This will do the following:
      * 1. If the limit for allowed active verification jobs has been reached, complete with an appropriate reject message
      * 2. Otherwise, create an actor with a fresh ID and pass the arguments provided by the client
      * 3. Send [[Verify]] message to the newly created instance of [[JobActor]]
      *   ([[bookNewJob]] will add the actor as an entry to the [[_jobHandles]] collection under a fresh ID;
      *
      *    @see [[bookNewJob]])
      * 4. Complete request with accepting message with the ID of the new verification job
      *
      * Use case:
      * - Send a request to verify a specific Viper file from the IDE
      * - Send a request to verify a specific Viper file from any other Viper client implementation,
      *   such as <a href="<a href="https://github.com/viperproject/viper_client">viper_client</a>">viper_client</a> (written in Python)
      */
    post {
        entity(as[VerificationRequest]) { r =>
          val arg_list = getArgListFromArgString(r.arg)

          val file: String = arg_list.last
          val astGen = new AstGenerator(logger)

          astGen.generateViperAst(file) match {
            case Some(prog) =>
              val jobHandler: JobID = createJobHandle(arg_list, prog)
              complete( VerificationRequestAccept(jobHandler.id) )
            case None =>
              complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS})."))
          }
      }
    }
  } ~ path("verify" / IntNumber) { jid =>
    /**
      * Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      *
      * This will do the following:
      * 1. If no job handle future with ID equal to <jid> exists in [[_jobHandles]], complete with an appropriate reject message
      * 2. Otherwise, once the job handle future is complete:
      *   - If the future completed with a failure, complete with an appropriate reject message
      *   - If the future completed successfully:
      *     - Create a [[Source]] <src> full of [[viper.silver.reporter.Message]]s
      *     - Send [[Terminator.WatchJobQueue]] message to the [[Terminator]] actor, awaiting
      *       [[akka.stream.scaladsl.SourceQueueWithComplete.watchCompletion()]] future before removing current job handle from [[_jobHandles]]
      *     - Complete request with <src>
      *
      * Use case:
      * - Ask ViperServer to begin streaming the results corresponding to the verification job with provided <jid>
      */
    get {
      jobs.lookupJob(jid) match {
        case Some(handle_future) =>
          // Found a job with this jid.
          onComplete(handle_future) {
            case Success(handle) =>
              // As soon as messages start being consumed, the terminator actor is triggered.
              // See Terminator.receive for more information
              val src: Source[Message, NotUsed] = Source.fromPublisher(handle.publisher)
              _termActor ! Terminator.WatchJobQueue(jid, handle)
              complete(src)
            case Failure(error) =>
              complete( VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $error") )
          }
        case _ =>
          // Did not find a job with this jid.
          complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
      }
    }
  } ~ path("discard" / IntNumber) { jid =>

    /**
      * Send GET request to "/discard/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      *
      * This will do the following:
      * 1. If no job handle with ID equal to <jid> exists in [[_jobHandles]], complete with an appropriate reject message
      * 2. Otherwise, once the job handle future is complete:
      *   - If the future completed with a failure, complete with an appropriate reject message
      *   - If the future completed successfully:
      *     - Create a new future based on response from a the current instance of [[JobActor]] to a
      *       [[ViperServerProtocol.Stop]] message (with a 5 second timeout)
      *     - Send a [[PoisonPill]] message to the current instance of [[JobActor]]
      *     - Complete request with accepting message
      *
      *  Use case:
      *  - Client decided to kill a verification job they no longer care about
      */
    get {
      jobs.lookupJob(jid) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
              val interrupt_done: Future[String] = (handle.jobActor ? Stop()).mapTo[String]
              onSuccess(interrupt_done) { msg =>
                handle.jobActor ! PoisonPill // the actor played its part.
                complete( JobDiscardAccept(msg) )
              }
            case Failure(error) =>
              complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
          }

        case None =>
          // Did not find a job with this jid.
          complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
      }
    }
  } ~ path("cache" /  "flush") {
    /**
      * Send GET request to "/cache/flush".
      *
      * This will invalidate the entire cache.
      *
      *  Use case:
      *  - Client decided to re-verify several files from scratch.
      */
    get {
      flushCache()
      complete( CacheFlushAccept(s"The cache has been flushed successfully.") )
    }
  } ~ path("cache" /  "flush") {
    /**
      * Send POST request to "/cache/flush".
      *
      * This will invalidate the cache for the tool and file specified.
      *
      *  Use case:
      *  - Client decided to re-verify the entire file from scratch.
      */
    post {
      entity(as[CacheResetRequest]) {
        r =>
          ViperCache.forgetFile(r.backend, r.file) match {
            case Some(_) =>
              complete( CacheFlushAccept(s"The cache for tool (${r.backend}) for file (${r.file}) has been flushed successfully.") )
            case None =>
              complete( CacheFlushReject(s"The cache does not exist for tool (${r.backend}) for file (${r.file}).") )
          }
      }
    }
  } ~ path("alloy") {
    /**
      * Send POST request to "/alloy".
      *
      * This will generate an instance of the given model.
      */
    post {
      entity(as[AlloyGenerationRequest]) { r =>
        try {
          val reporter: A4Reporter = new A4Reporter()

          val world = CompUtil.parseEverything_fromString(reporter, r.arg)

          val options: A4Options = new A4Options()
          options.solver = A4Options.SatSolver.parse(r.solver)
          options.skolemDepth = 1
          options.noOverflow = true
          options.unrolls = -1

          val commands = world.getAllCommands
          if (commands.size() != 1) {
            complete( AlloyGenerationRequestReject(s"Expected only one command, but got ${commands.size()}") )
          }
          val command = commands.get(0)
          val solution = TranslateAlloyToKodkod.execute_command(reporter, world.getAllReachableSigs, command, options)
          if (solution.satisfiable()) {
            complete( AlloyGenerationRequestComplete(solution) )
          } else {
            complete( AlloyGenerationRequestReject(s"Model could not be satisfied.") )
          }
        } catch {
          case e: Throwable => complete( AlloyGenerationRequestReject(s"An exception occurred during model-generation:\n${e.toString}") )
        }
      }
    }
  }


  private def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

}
