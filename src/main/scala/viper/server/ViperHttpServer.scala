/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package viper.server

import akka.NotUsed
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.PoisonPill
import akka.stream.scaladsl.Source
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, TranslateAlloyToKodkod}

import spray.json.DefaultJsonProtocol

import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.core.{SilverEnvelope, ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.server.protocol.ViperServerProtocol._
import viper.server.protocol.ViperIDEProtocol._
import viper.server.utility.AstGenerator
import viper.server.vsi.Requests.{CacheResetRequest, VerificationRequest}
import viper.server.vsi.{Envelope, HttpServerInterface, VerificationJobHandler}
import viper.silver.reporter._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.util.Try


class ViperHttpServer(private var _config: ViperConfig) extends ViperCoreServer(_config) with HttpServerInterface {

  def routes(): Route = {
    /**
      * Send GET request to "/exit".
      *
      * This will do the following:
      * 1. Map all existing jobs from [[_jobHandles]] to a list of futures based on the responses of instances of
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
              _termActor ! Terminator.Exit
              complete( ServerStopConfirmed("shutting down...") )
            case Failure(err_msg) =>
              println(s"Interrupting one of the verification threads timed out: $err_msg")
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
      * 3. Send [[ViperServerProtocol.Verify]] message to the newly created instance of [[MainActor]]
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
        val ast_option = astGen.generateViperAst(file)

        val backend_option: Option[ViperBackendConfig] = arg_list match {
          case "silicon" :: args => Some(SiliconConfig(args))
          case "carbon" :: args => Some(CarbonConfig(args))
          case custom :: args => Some(CustomConfig(args))
          case args =>
            logger.get.error("invalid arguments: ${args.toString}",
              "You need to specify the verification backend, e.g., `silicon [args]`")
            None
        }

        val id: Int = if (ast_option.isDefined && backend_option.isDefined) {
          val jobHandler: VerificationJobHandler = verify(file, backend_option.get, ast_option.get)
          jobHandler.id
        } else {
          -1
        }
        if (id >= 0) {
          complete( VerificationRequestAccept(id) )
        } else {
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
      *     - Send [[Terminator.WatchJob]] message to the [[Terminator]] actor, awaiting
      *       [[SourceQueueWithComplete.watchCompletion]] future before removing current job handle from [[_jobHandles]]
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
              val src_letter: Source[Envelope, NotUsed] = Source.fromPublisher((handle.publisher))
              // We do not remove the current entry from [[_job_handles]] because the handle is
              //  needed in order to terminate the job before streaming is completed.
              //  The Terminator actor will delete the entry upon completion of the stream.
              val src_message: Source[Message, NotUsed] = src_letter.map({
                case SilverEnvelope(msg) => msg
                case _ => throw new Throwable("Wrong message type")
              })
              src_message
              _termActor ! Terminator.WatchJobQueue(jid, handle)
              complete(src_message)
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
      *     - Create a new future based on response from a the current instance of [[MainActor]] to a
      *       [[ViperServerProtocol.Stop]] message (with a 5 second timeout)
      *     - Send a [[PoisonPill]] message to the current instance of [[MainActor]]
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
      object AlloyRequest extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {
        case class AlloyGenerationRequest(arg: String, solver: String)
        implicit val generateStuff = jsonFormat2(AlloyGenerationRequest.apply)
      }
      entity(as[AlloyRequest.AlloyGenerationRequest]) { r =>
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
