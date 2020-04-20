/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package viper.server



import scala.concurrent.{ Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.{NotUsed}
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{PoisonPill}
import akka.stream.scaladsl.{Source}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, TranslateAlloyToKodkod}
import viper.server.ViperServerProtocol._
import viper.server.ViperIDEProtocol._
import viper.silver.reporter._
import viper.silver.logger.ViperLogger
import ViperRequests.{ VerificationRequest, CacheResetRequest, AlloyGenerationRequest }

import scala.util.Try


class ViperHttpServer(private var _config: ViperConfig) extends ViperCoreServer(_config) {

  override def start(): Unit = {
    init(Some(routes))
  }
 
  def routes(logger: ViperLogger): Route = {
    
    /**
      * Send GET request to "/exit".
      *
      * This will do the following:
      * 1. Map all existing jobs from [[_job_handles]] to a list of futures based on the responses of instances of
      *    [[MainActor]] from [[ViperServerProtocol.Stop]] messages (with a 5 second timeout)
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
              _term_actor ! Terminator.Exit
              complete( ServerStopConfirmed("shutting down...") )
            case Failure(err_msg) =>
              println(s"Interrupting one of the verification threads timed out: $err_msg")
              _term_actor ! Terminator.Exit
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
      * 3. Send [[ViperServerProtocol.Verify]] message to the newly created instance of [[MainActor]]
      *   ([[bookNewJob]] will add the actor as an entry to the [[_job_handles]] collection under a fresh ID;
      *    @see [[bookNewJob]])
      * 4. Complete request with accepting message with the ID of the new verification job
      *
      * Use case:
      * - Send a request to verify a specific Viper file from the IDE
      * - Send a request to verify a specific Viper file from any other Viper client implementation,
      *   such as <a href="https://bitbucket.org/viperproject/viper_client">viper_client</a> (written in Python)
      */
    post {
      entity(as[VerificationRequest]) { r =>
        val arg_list = getArgListFromArgString(r.arg)
        val (id, _) = createJobHandle(arg_list)

        id match {
          case Some(id) => complete( VerificationRequestAccept(id) )
          case None => complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS)."))
        }
      }
    } // TEST (allows the communication with Viperserver by simple get request)###################################################################################################################
  } ~ path("test") {
    get {
      println("Hello from Test\n")
      val arg_list = getArgListFromArgString("silicon wrong.sil")
      val (ids, jobHandle) = createJobHandle(arg_list)

      val id = ids match {
        case Some(id) => id
        case None => 0
      }

      jobHandle match {
        case Some(handle_future) => {
          onComplete(handle_future) {
            case Success(handle) => {
              val src: Source[Message, NotUsed] = Source.fromPublisher(handle.publisher)
              _term_actor ! Terminator.WatchJob(id, handle)
              complete(s"the verification was done")
            }
            case Failure(error) =>
              complete( VerificationRequestReject(s"The verification job #$id resulted in a terrible error: $error") )
          }
        }
        case _ =>
          // Did not find a job with this jid.
          complete( VerificationRequestReject(s"The verification job #$id does not exist.") )
      }    
    } //#########################################################################################################################
  } ~ path("verify" / IntNumber) { jid =>

    /**
      * Send GET request to "/verify/<jid>" where <jid> is a non-negative integer.
      * <jid> must be an ID of an existing verification job.
      *
      * This will do the following:
      * 1. If no job handle future with ID equal to <jid> exists in [[_job_handles]], complete with an appropriate reject message
      * 2. Otherwise, once the job handle future is complete:
      *   - If the future completed with a failure, complete with an appropriate reject message
      *   - If the future completed successfully:
      *     - Create a [[Source]] <src> full of [[viper.silver.reporter.Message]]s
      *     - Send [[Terminator.WatchJob]] message to the [[Terminator]] actor, awaiting
      *       [[SourceQueueWithComplete.watchCompletion]] future before removing current job handle from [[_job_handles]]
      *     - Complete request with <src>
      *
      * Use case:
      * - Ask ViperServer to begin streaming the results corresponding to the verification job with provided <jid>
      */
    get {
      lookupJob(jid) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              // Found a job with this jid.
              val src: Source[Message, NotUsed] = Source.fromPublisher(handle.publisher)
              // We do not remove the current entry from [[_job_handles]] because the handle is
              //  needed in order to terminate the job before streaming is completed.
              //  The Terminator actor will delete the entry upon completion of the stream.
              _term_actor ! Terminator.WatchJob(jid, handle)
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
      * 1. If no job handle with ID equal to <jid> exists in [[_job_handles]], complete with an appropriate reject message
      * 2. Otherwise, once the job handle future is complete:
      *   - If the future completed with a failure, complete with an appropriate reject message
      *   - If the future completed successfully:
      *     - Create a new future based on response from a the current instance of [[MainActor]] to a
      *       [[ViperServerProtocol.Stop]] message (with a 5 second timeout)
      *     - Send a [[PoisonPill]] message to the current instance of [[MainActor]]
      *     - Complete request with accepting message
      *
      *  Use case:
      *  - Client decided to kill a verification job they no linger care about
      */
    get {
      lookupJob(jid) match {
        case Some(handle_future) =>
          onComplete(handle_future) {
            case Success(handle) =>
              implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
              val interrupt_done: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
              onSuccess(interrupt_done) { msg =>
                handle.controller_actor ! PoisonPill // the actor played its part.
                complete( JobDiscardAccept(msg) )
              }
            case Failure(error) =>
              complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
          }

        case _ =>
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
      ViperCache.resetCache()
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
          case e => complete( AlloyGenerationRequestReject(s"An exception occurred during model-generation:\n${e.toString}") )
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
