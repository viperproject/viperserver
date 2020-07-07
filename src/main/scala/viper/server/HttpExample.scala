package viper.server

import akka.actor.{ActorSystem, PoisonPill}
import akka.pattern.ask
import akka.NotUsed
import akka.util.Timeout
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Source}
import viper.server.protocol.ViperServerProtocol._
import viper.server.protocol.ViperIDEProtocol._
import viper.server.core.{ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.server.core.ViperBackendConfigs.{CarbonConfig, CustomConfig, SiliconConfig}
import viper.server.protocol.ViperServerProtocol
import viper.server.utility.AstGenerator
import viper.server.vsi.{CustomizableHttpServer, Requests, VerificationJobHandler}
import viper.silver.reporter._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.util.Try


//class ViperHttpBasic(private var _config: ViperConfig) extends ViperCoreServer(_config) with BasicHttpServer {
//
//  import viper.server.ViperRequests.{VerificationRequest}
//  override def onExit(): Route = {
//    onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
//      err match {
//        case Success(_) =>
//          _termActor ! Terminator.Exit
//          complete( ServerStopConfirmed("shutting down...") )
//        case Failure(err_msg) =>
//          println(s"Interrupting one of the verification threads timed out: $err_msg")
//          _termActor ! Terminator.Exit
//          complete( ServerStopConfirmed("forcibly shutting down...") )
//      }
//    }
//  }
//
//  override def onPostRequest(): Route = {
//    entity(as[VerificationRequest]) { r =>
//      val arg_list = getArgListFromArgString(r.arg)
//
//      val file: String = arg_list.last
//      val astGen = new AstGenerator(logger)
//      val ast_option = astGen.generateViperAst(file)
//
//      val backend_option: Option[ViperBackendConfig] = arg_list match {
//        case "silicon" :: args => Some(SiliconConfig(args))
//        case "carbon" :: args => Some(CarbonConfig(args))
//        case custom :: args => Some(CustomConfig(args))
//        case args =>
//          logger.get.error("invalid arguments: ${args.toString}",
//            "You need to specify the verification backend, e.g., `silicon [args]`")
//          None
//      }
//
//      val id: Int = if (ast_option.isDefined && backend_option.isDefined) {
//        val jobHandler: VerificationJobHandler = verify(file, backend_option.get, ast_option.get)
//        jobHandler.id
//      } else {
//        -1
//      }
//      if (id >= 0) {
//        complete( VerificationRequestAccept(id) )
//      } else {
//        complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS})."))
//      }
//    }
//  }
//
//  override def onGetRequest(jid: Int): Route = {
//    jobs.lookupJob(jid) match {
//      case Some(handle_future) =>
//        // Found a job with this jid.
//        onComplete(handle_future) {
//          case Success(handle) =>
//            val src_letter: Source[Letter, NotUsed] = Source.fromPublisher((handle.publisher))
//            val src_msg: Source[Message, NotUsed] = src_letter.map({
//              case SilverLetter(msg) => msg
//              case _ => throw new Throwable("Wrong message type")
//            })
//            // We do not remove the current entry from [[_job_handles]] because the handle is
//            //  needed in order to terminate the job before streaming is completed.
//            //  The Terminator actor will delete the entry upon completion of the stream.
//            _termActor ! Terminator.WatchJobQueue(jid, handle)
//            complete(src_msg)
//          case Failure(error) =>
//            complete( VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $error") )
//        }
//      case _ =>
//        // Did not find a job with this jid.
//        complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
//    }
//  }
//
//  private def getArgListFromArgString(arg_str: String): List[String] = {
//    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
//    val quoted_string = """^["'](.*)["']$""".r
//    possibly_quoted_string.findAllIn(arg_str).toList.map {
//      case quoted_string(noqt_a) => noqt_a
//      case a => a
//    }
//  }
//}
//
//class ViperHttpStandard(private var _config: ViperConfig) extends ViperCoreServer(_config) with StandardHttpServer {
//
//  override def onExit(): Route = {
//    onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
//      err match {
//        case Success(_) =>
//          _termActor ! Terminator.Exit
//          complete( ServerStopConfirmed("shutting down...") )
//        case Failure(err_msg) =>
//          println(s"Interrupting one of the verification threads timed out: $err_msg")
//          _termActor ! Terminator.Exit
//          complete( ServerStopConfirmed("forcibly shutting down...") )
//      }
//    }
//  }
//
//  override def onPostRequest(r: Requests.VerificationRequest): Route = {
//    val arg_list = getArgListFromArgString(r.arg)
//
//    val file: String = arg_list.last
//    val astGen = new AstGenerator(logger)
//    val ast_option = astGen.generateViperAst(file)
//
//    val backend_option: Option[ViperBackendConfig] = arg_list match {
//      case "silicon" :: args => Some(SiliconConfig(args))
//      case "carbon" :: args => Some(CarbonConfig(args))
//      case custom :: args => Some(CustomConfig(args))
//      case args =>
//        logger.get.error("invalid arguments: ${args.toString}",
//          "You need to specify the verification backend, e.g., `silicon [args]`")
//        None
//    }
//
//    val id: Int = if (ast_option.isDefined && backend_option.isDefined) {
//      val jobHandler: VerificationJobHandler = verify(file, backend_option.get, ast_option.get)
//      jobHandler.id
//    } else {
//      -1
//    }
//    if (id >= 0) {
//      complete( VerificationRequestAccept(id) )
//    } else {
//      complete( VerificationRequestReject(s"the maximum number of active verification jobs are currently running (${jobs.MAX_ACTIVE_JOBS})."))
//    }
//  }
//
//  override def onGetRequest(jid: Int): Route = {
//    jobs.lookupJob(jid) match {
//      case Some(handle_future) =>
//        // Found a job with this jid.
//        onComplete(handle_future) {
//          case Success(handle) =>
//            val src_letter: Source[Letter, NotUsed] = Source.fromPublisher((handle.publisher))
//            val src_msg: Source[Message, NotUsed] = src_letter.map({
//              case SilverLetter(msg) => msg
//              case _ => throw new Throwable("Wrong message type")
//            })
//            // We do not remove the current entry from [[_job_handles]] because the handle is
//            //  needed in order to terminate the job before streaming is completed.
//            //  The Terminator actor will delete the entry upon completion of the stream.
//            _termActor ! Terminator.WatchJobQueue(jid, handle)
//            complete(src_msg)
//          case Failure(error) =>
//            complete( VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $error") )
//        }
//      case _ =>
//        // Did not find a job with this jid.
//        complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
//    }
//  }
//
//  override def onDiscard(jid: Int): Route = {
//    jobs.lookupJob(jid) match {
//      case Some(handle_future) =>
//        onComplete(handle_future) {
//          case Success(handle) =>
//            implicit val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)
//            val interrupt_done: Future[String] = (handle.controller_actor ? Stop(true)).mapTo[String]
//            onSuccess(interrupt_done) { msg =>
//              handle.controller_actor ! PoisonPill // the actor played its part.
//              complete( JobDiscardAccept(msg) )
//            }
//          case Failure(error) =>
//            complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
//        }
//
//      case _ =>
//        // Did not find a job with this jid.
//        complete( JobDiscardReject(s"The verification job #$jid does not exist.") )
//    }
//  }
//
//  override def onFlushGet(): Route = {
//    flushCache()
//    complete( CacheFlushAccept(s"The cache has been flushed successfully.") )
//  }
//
//  override def onFlushPost(r: Requests.CacheResetRequest): Route = {
//    ViperCache.forgetFile(r.backend, r.file) match {
//      case Some(_) =>
//        complete( CacheFlushAccept(s"The cache for tool (${r.backend}) for file (${r.file}) has been flushed successfully.") )
//      case None =>
//        complete( CacheFlushReject(s"The cache does not exist for tool (${r.backend}) for file (${r.file}).") )
//    }
//  }
//
//  private def getArgListFromArgString(arg_str: String): List[String] = {
//    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
//    val quoted_string = """^["'](.*)["']$""".r
//    possibly_quoted_string.findAllIn(arg_str).toList.map {
//      case quoted_string(noqt_a) => noqt_a
//      case a => a
//    }
//  }
//}
//
//class ViperHttpCustom(private var _config: ViperConfig) extends ViperCoreServer(_config) with CustomizableHttpServer {
//
////  override def onGetRequest(jid: Int): Route = {
////    jobs.lookupJob(jid) match {
////      case Some(handle_future) =>
////        // Found a job with this jid.
////        onComplete(handle_future) {
////          case Success(handle) =>
////            val src_letter: Source[Letter, NotUsed] = Source.fromPublisher((handle.publisher))
////            val src_msg: Source[Message, NotUsed] = src_letter.map({
////              case SilverLetter(msg) => msg
////              case _ => throw new Throwable("Wrong message type")
////            })
////            // We do not remove the current entry from [[_job_handles]] because the handle is
////            //  needed in order to terminate the job before streaming is completed.
////            //  The Terminator actor will delete the entry upon completion of the stream.
////            _termActor ! Terminator.WatchJobQueue(jid, handle)
////            complete(src_msg)
////          case Failure(error) =>
////            complete( VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $error") )
////        }
////      case _ =>
////        // Did not find a job with this jid.
////        complete( VerificationRequestReject(s"The verification job #$jid does not exist.") )
////    }
////  }
//
//  def exitRoute(): Route = path("exit") {
//    get {
//      onComplete(getInterruptFutureList()) { err: Try[List[String]] =>
//        err match {
//          case Success(_) =>
//            _termActor ! Terminator.Exit
//            complete(ServerStopConfirmed("shutting down..."))
//          case Failure(err_msg) =>
//            println(s"Interrupting one of the verification threads timed out: $err_msg")
//            _termActor ! Terminator.Exit
//            complete(ServerStopConfirmed("forcibly shutting down..."))
//        }
//      }
//    }
//  }
//
//  override def routes(): Route = addRoute(super.routes(), exitRoute())
//}
