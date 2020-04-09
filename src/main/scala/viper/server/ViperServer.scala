/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package viper.server


import org.reactivestreams.Publisher

import scala.language.postfixOps
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.collection.mutable
import scala.util.{Failure, Success}
import akka.{Done, NotUsed}
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.ast.Module
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, A4Solution, TranslateAlloyToKodkod}
import viper.server.ViperServerProtocol._
import viper.server.ViperIDEProtocol._
import viper.silver.reporter
import viper.silver.reporter._
import viper.silver.logger.ViperLogger

import scala.util.Try
import java.io.{File, FileReader, FileWriter}
import java.nio.file.Paths

import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.{DefaultStates, SilFrontend}
import viper.silver.parser.PProgram
import viper.silver.verifier.VerificationResult

object ViperServerRunner {

	import ViperRequests._

	private var _config: ViperConfig = _
	final def config: ViperConfig = _config

	private var _logger: ViperLogger = _
	final def logger: ViperLogger = _logger

	implicit val system: ActorSystem = ActorSystem("Main")
	implicit val materializer: ActorMaterializer = ActorMaterializer()
	implicit val executionContext: ExecutionContextExecutor = system.dispatcher


	// We can potentially have more than one verification task at the same time.
	// A verification task is distinguished via the corresponding ActorRef,
	//  as well as its unique job_id.

	case class JobHandle(controller_actor: ActorRef,
						 queue: SourceQueueWithComplete[Message],
						 publisher: Publisher[Message])

	private var _job_handles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
	private var _next_job_id: Int = 0
	val MAX_ACTIVE_JOBS: Int = 3

	private def newJobsAllowed = _job_handles.size < MAX_ACTIVE_JOBS

	/** This function
	  *  - updates the job counter
	  *  - adds a (jobID, JobHandle promise) pair to the map containing all jobs
	  *
	  *  It takes as argument a function that maps an integer to a jobHandle promise.
	  *  */
	private def bookNewJob(job_executor: Int => Future[JobHandle]): Int = {

		val new_jid = _next_job_id
		_job_handles(new_jid) = job_executor(new_jid) // get a job handle promise
		_next_job_id = _next_job_id + 1
		new_jid
	}

	private def discardJob(jid: Int): mutable.Map[Int, Future[JobHandle]] = {
		_job_handles -= jid
	}

	/** If the Option is resolved to None, the job does not exist.
	  * If the Option is resolved to Some(_),
	  *   a) The Future is not yet completed ==> verification in progress.
	  *   b) The Future is already completed ==> job done.
	  */
	private def lookupJob(jid: Int): Option[ Future[JobHandle] ] = {
		_job_handles.get(jid)
	}


	// --- Actor: Terminator --- //////////////////////////////////////////////

	private var _term_actor: ActorRef = _

	object Terminator {
		case object Exit
		case class WatchJob(jid: Int, handle: JobHandle)

		def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(bindingFuture))
	}

	class Terminator(bindingFuture: Future[Http.ServerBinding]) extends Actor {

		override def receive: PartialFunction[Any, Unit] = {
			case Terminator.Exit =>
				bindingFuture
				  .flatMap(_.unbind()) // trigger unbinding from the port
				  .onComplete(_ => system.terminate()) // and shutdown when done
			case Terminator.WatchJob(jid, handle) =>
				val queue_completion_future: Future[Done] = handle.queue.watchCompletion()
				queue_completion_future.onComplete( {
					case Failure(e) =>
						println(s"Terminator detected failure in job #$jid: $e")
						throw e
					case Success(_) =>
						discardJob(jid)
						println(s"Terminator deleted job #$jid")
				})
		}
	}

	// --- Actor: MainActor --- //////////////////////////////////////////////
	// (See model description in ViperServerProtocol.scala)

	object MainActor {
		def props(id: Int, logger: ViperLogger): Props = Props(new MainActor(id, logger))
	}

	class MainActor(private val id: Int, private val logger: ViperLogger) extends Actor {
		private var _verificationTask: Thread = _
		private var _args: List[String] = _

		// blocking
		private def interrupt: Boolean = {
			if (_verificationTask != null && _verificationTask.isAlive) {
				_verificationTask.interrupt()
				_verificationTask.join()
				println(s"Job #$id has been successfully interrupted.")
				return true
			}
			false
		}

		override def receive: PartialFunction[Any, Unit] = {
			case Stop(call_back_needed) =>
				val did_I_interrupt = interrupt
				if (call_back_needed) {
					// If a callback is expected, then the caller must decide when to kill the actor.
					if (did_I_interrupt) {
						sender ! s"Job #$id has been successfully interrupted."
					} else {
						sender ! s"Job #$id has already been finalized."
					}
				}
			// main actor receives message asking him to verify
			case Verify(args) =>
				if (_verificationTask != null && _verificationTask.isAlive) {
					_args = args
					_verificationTask.interrupt()
					_verificationTask.join()
				}
				_verificationTask = null
				sender ! verify(args) //execute actor's verify function and send result (JobHandle) back to sender
			case msg => throw new Exception("Main Actor: unexpected message received: " + msg)
		}

		private def verify(args: List[String]): JobHandle = {

			// The maximum number of messages in the reporter's message buffer is 10000.
			val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure).toMat(Sink.asPublisher(false))(Keep.both).run()

			//Create reporter actor that holds previously created SourceQueue
			val my_reporter = system.actorOf(ReporterActor.props(id, queue), s"reporter_$id")

			//start a new thread via VerificationWorker (which implements Runnable)
			_verificationTask = new Thread(new VerificationWorker(my_reporter, logger.get, args))
			_verificationTask.start()

			println(s"Starting job #$id...")

			JobHandle(self, queue, publisher) //returns Jobhandle with reference to itself attached
		}
	}

	// --- Actor: ReporterActor ---  //////////////////////////////////////////////

	object ReporterActor {
		case object ClientRequest
		case class ServerReport(msg: reporter.Message)
		case class FinalServerReport(success: Boolean)

		def props(jid: Int, queue: SourceQueueWithComplete[Message]): Props = Props(new ReporterActor(jid, queue))
	}

	class ReporterActor(jid: Int, queue: SourceQueueWithComplete[Message]) extends Actor {

		//reporter actor only has one funciton. Namely, receiving a message
		override def receive: PartialFunction[Any, Unit] = {
			case ReporterActor.ClientRequest =>
			case ReporterActor.ServerReport(msg) =>
				//                println(s"Reporter Actor reports: $msg")
				queue.offer(msg)
			case ReporterActor.FinalServerReport(success) =>
				queue.complete()
				if ( success )
					println(s"Job #$jid has been completed successfully.")
				else
					println(s"Job #$jid has been completed ERRONEOUSLY.")
				self ! PoisonPill
			case _ =>
		}
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
				val interrupt_future_list: List[Future[String]] = _job_handles map {
					case (jid, handle_future) => handle_future.flatMap {
						case JobHandle(actor, _, _) => implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
							(actor ? Stop(true)).mapTo[String]
					}
				} toList

				val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)

				onComplete(overall_interrupt_future) { err: Try[List[String]] =>
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
				println("hi from verify post thingy")

				if (newJobsAllowed) {
					def job_executor (new_jid:Int) : Future[JobHandle] = {
						implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
						//Ask (Actor)system to create a main actor passing along the logger and the jid
						val main_actor = system.actorOf(MainActor.props(new_jid, logger), s"main_actor_$new_jid")
						//arglist contains verifier backend, whether cache is enabled, z3 env var and file to be verified.
						var arg_list = getArgListFromArgString(r.arg)
						//Asking the main actor for a Future by sending a message (consisting of the VSProtocol Verify case)
						val new_job_handle: Future[JobHandle] = (main_actor ? ViperServerProtocol.Verify(arg_list)).mapTo[JobHandle]
						new_job_handle
					}
					val id = bookNewJob(job_executor)
					complete( VerificationRequestAccept(id) )

				} else {
					complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS).") )
				}
			}
		}
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
							_term_actor ! Terminator.WatchJob(jid, handle) //send msg to terminator actor
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

	def main(args: Array[String]): Unit =   {
		try {
			init(args)
		} catch { case e: Throwable =>
			println(s"Cannot parse CMD arguments: $e")
			sys.exit(1)
		}

		ViperCache.initialize(logger.get, config.backendSpecificCache())
		val port = config.port()
		val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes(logger),
			"localhost",
			port)
		_term_actor = system.actorOf(Terminator.props(bindingFuture), "terminator")

		writeBatchScripts(port)
		println(s"ViperServer online at http://localhost:$port")

//		parseTest()
		verificationTest()

	} // method main

	def init(cmdArgs: Seq[String]) {
		_config = new ViperConfig(cmdArgs)
		_config.verify()
		_logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
		println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")
	}

	private def getArgListFromArgString(arg_str: String): List[String] = {
		val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
		val quoted_string = """^["'](.*)["']$""".r
		possibly_quoted_string.findAllIn(arg_str).toList.map {
			case quoted_string(noqt_a) => noqt_a
			case a => a
		}
	}

	def parseTest(): Unit = {
		var astgen = new ASTGenerator("test.vpr")
		println("Parsing---")
		println(astgen.parse_AST)
		for (m <- astgen.parse_AST.methods) println(m)

		println("Translating---")
		println(astgen.translated_AST)
	}

	def verificationTest() : Unit = {
		var astGen = new ASTGenerator("test.vpr")
		var astVer = new VerificationFromAST(astGen.translated_AST)
		println("Verifying---")
		println(astVer.ver_result)
	}

	class VerificationFromAST(var ast : Program){
		var ver_backend: SilFrontend = initialize_backend()
		var ver_result: Option[VerificationResult] = verify()

		private def initialize_backend() : SilFrontend = {
			val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
										   .toMat(Sink.asPublisher(false))(Keep.both)
										   .run()

			val some_reporter = system.actorOf(ReporterActor.props(0, queue), s"verification_reporter")

			new SiliconFrontend(new ActorReporter(some_reporter, "silicon"), _logger.get)
		}

		private def verify(): Option[VerificationResult] = {
			ver_backend.setState(DefaultStates.Translation)
			ver_backend.doConsistencyCheck(ast)
			ver_backend.verification()
			ver_backend.getVerificationResult
		}
	}

	class ASTGenerator(private val vpr_file_path:String){
		var ver_backend: SilFrontend = initialize_backend()
		var parse_AST : PProgram = parse()
		var translated_AST : Program = translate()

		private def initialize_backend() : SilFrontend = {
			val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
										   .toMat(Sink.asPublisher(false))(Keep.both)
										   .run()

			val some_reporter = system.actorOf(ReporterActor.props(0, queue), s"parse_reporter")

			new SiliconFrontend(new ActorReporter(some_reporter, "silicon"), _logger.get)
		}

		private def parse(): PProgram = {
			val args:Array[String] = Array(vpr_file_path)
			ver_backend.setVerifier( ver_backend.createVerifier(args.mkString(" ")) )
			ver_backend.prepare(args)
			ver_backend.init( ver_backend.verifier )
			ver_backend.reset(Paths.get(ver_backend.config.file()))
			ver_backend.parsing()
			ver_backend.parsingResult
		}

		private def translate() = {
			ver_backend.semanticAnalysis()
			ver_backend.translation()
			ver_backend.translationResult
		}
	}

	private def writeBatchScripts(port: Int): Unit ={
		val term_file = new File("ter.bat")
		val ver_file = new File("ver.bat")

		val term_writer = new FileWriter(term_file)
		val ver_writer = new FileWriter(ver_file)

		term_writer.write(s"cd ..\\viper_client-master & python client.py -c terminate -p $port & cd ..\\viperserver")
		ver_writer.write(s"cd ..\\viper_client-master & python client.py -c verify -p $port & cd ..\\viperserver")

		term_writer.close()
		ver_writer.close()
	}
} // object ViperServerRunner
