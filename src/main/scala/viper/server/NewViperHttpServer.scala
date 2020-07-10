package viper.server

import akka.NotUsed
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source

import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, TranslateAlloyToKodkod}

import spray.json.DefaultJsonProtocol

import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.core.{SilverEnvelope, ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.silver.logger.{ViperLogger, ViperStdOutLogger}
import viper.server.protocol.ViperIDEProtocol.{AlloyGenerationRequestComplete, AlloyGenerationRequestReject, CacheFlushAccept, CacheFlushReject, JobDiscardAccept, JobDiscardReject, ServerStopConfirmed, VerificationRequestAccept, VerificationRequestReject}
import viper.silver.reporter.Message
import viper.server.utility.AstGenerator
import viper.server.vsi.Requests.CacheResetRequest
import viper.server.vsi.{Envelope, HttpVerificationServerInterface, JobNotFoundException, Requests, VerificationJobHandler}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class NewViperHttpServer(private var _config: ViperConfig) extends ViperCoreServer(_config)
                                                              with HttpVerificationServerInterface {

  override def start(){
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    ViperCache.initialize(logger.get, config.backendSpecificCache())
    println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    val port = config.port()
    val all_routes: Route = addRoute(routes(), flushCacheRoute())
    val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(all_routes, "localhost", port)

    _termActor = system.actorOf(Terminator.props(bindingFuture), "terminator")
    println(s"ViperServer online at http://localhost:$port")
  }

  override def serverStopConfirmation(interrupt: Try[List[String]]): ToResponseMarshallable = {
    interrupt match {
      case Success(_) =>
        ServerStopConfirmed("shutting down...")
      case Failure(err_msg) =>
        println(s"Interrupting one of the verification threads timed out: $err_msg")
        ServerStopConfirmed("forcibly shutting down...")
    }
  }

  override def onVerifyPost(vr: Requests.VerificationRequest): ToResponseMarshallable = {
    val logger = ViperStdOutLogger("temp Logger")

    val arg_list = getArgListFromArgString(vr.arg)
    val file: String = arg_list.last
    val arg_list_partial = arg_list.dropRight(1)
    val astGen = new AstGenerator(logger)
    val ast_option = astGen.generateViperAst(file)

    val backend_option: Option[ViperBackendConfig] = arg_list_partial match {
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
      VerificationRequestAccept(id)
    } else {
      VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS}).")
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

  override def unpackMessages(s: Source[Envelope, NotUsed]): ToResponseMarshallable = {
    import viper.server.protocol.ViperIDEProtocol._
    val src_message: Source[Message, NotUsed] = s.map({
      case SilverEnvelope(msg) => msg
      case _ => throw new Throwable("Wrong message type")
    })
    src_message
  }

  override def verificationRequestRejection(jid: Int, e: Throwable): ToResponseMarshallable = {
    e match {
      case JobNotFoundException() => VerificationRequestReject(s"The verification job #$jid does not exist.")
      case _ => VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $e")
    }
  }

  override def discardJObConfirmation(jid: Int, msg: String): ToResponseMarshallable = {
    JobDiscardAccept(msg)
  }

  override def discardJobRejection(jid: Int): ToResponseMarshallable = {
    JobDiscardReject(s"The verification job #$jid does not exist.")
  }

  def flushCacheRoute(): Route =  path("cache" /  "flush") {
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
    }~ path("cache" /  "flush") {
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
  }
}
