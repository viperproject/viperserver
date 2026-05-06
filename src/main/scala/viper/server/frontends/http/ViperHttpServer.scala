// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.http

import java.net.InetSocketAddress
import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.{A4Options, TranslateAlloyToKodkod}
import io.undertow.Undertow
import spray.json._
import viper.server.ViperConfig
import viper.server.core.{AstConstructionFailureException, VerificationExecutionContext, ViperBackendConfig, ViperCache, ViperCoreServer}
import viper.server.frontends.http.jsonWriters.ViperIDEProtocol._
import viper.server.utility.Helpers.{getArgListFromArgString, validateViperFile}
import viper.server.vsi._

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class ViperHttpServer(config: ViperConfig)(executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) {

  var port: Int = 0
  private var undertow: Undertow = _
  private val stoppedPromise: Promise[Unit] = Promise()

  private val routes: ViperCaskRoutes = new ViperCaskRoutes(this)

  override def start(): Future[Unit] = {
    port = config.port.toOption.getOrElse(0)
    super.start().map { _ =>
      println(s"ViperServer online at http://localhost:$port")
      ()
    }(executor)
  }

  override def start(active_jobs: Int): Future[Unit] = {
    ast_jobs = new JobPool("AST-pool", active_jobs)
    ver_jobs = new JobPool("Verification-pool", active_jobs)
    undertow = Undertow.builder()
      .addHttpListener(port, "localhost")
      .setHandler(routes.defaultHandler)
      .build()
    undertow.start()
    val boundAddr = undertow.getListenerInfo.get(0).getAddress.asInstanceOf[InetSocketAddress]
    port = boundAddr.getPort
    isRunning = true
    Future.unit
  }

  override protected def onExit(): Future[Unit] = {
    if (undertow != null) {
      val u = undertow
      undertow = null
      Future {
        u.stop()
      }(executor).map(_ => ())(executor)
    } else {
      Future.unit
    }
  }

  /** Future that resolves once the server has been shut down via /exit. */
  def stopped(): Future[Unit] = stoppedPromise.future

  /** Triggered by the `/exit` route. Stops jobs, unbinds the listener, then
    * completes the `stopped()` future.
    */
  private[http] def handleExit(): String = {
    val confirmation = scala.util.Try(scala.concurrent.Await.result(
      stop(), scala.concurrent.duration.Duration.Inf))
    val msg = confirmation match {
      case Success(_) =>
        println("shutting down...")
        "shutting down..."
      case Failure(err_msg) =>
        globalLogger.error(s"Interrupting one of the verification threads timed out: $err_msg")
        println("forcibly shutting down...")
        "forcibly shutting down..."
    }
    stoppedPromise.complete(confirmation.map(_ => ()))
    ServerStopConfirmed(msg).toJson.compactPrint
  }

  private[http] def handleVerifyPost(arg: String): String = {
    val arg_list = getArgListFromArgString(arg)
    val file: String = arg_list.last
    val arg_list_partial: List[String] = arg_list.dropRight(1)

    if (!validateViperFile(file)) {
      return VerificationRequestReject("File not found").toJson.compactPrint
    }

    val backend = try {
      ViperBackendConfig(arg_list_partial)
    } catch {
      case _: IllegalArgumentException =>
        globalLogger.error(s"Invalid arguments: $arg " +
          s"You need to specify the verification backend, e.g., `silicon [args]`")
        return VerificationRequestReject("Invalid arguments for backend.").toJson.compactPrint
    }

    val ast_id = requestAst(file, backend)
    val ver_id = verifyWithAstJob(file, ast_id, backend)
    VerificationRequestAccept(ast_id, ver_id).toJson.compactPrint
  }

  private[http] def streamAst(id: Int): cask.Response[geny.Writable] = {
    val ast_id = AstJobId(id)
    ast_jobs.lookupJob(ast_id) match {
      case Some(handle_future) =>
        val handle = scala.concurrent.Await.result(handle_future, scala.concurrent.duration.Duration.Inf)
        jsonLineResponse(handle.stream.iterator)
      case None =>
        textJsonResponse(VerificationRequestReject(s"The verification job #$id does not exist.").toJson.compactPrint)
    }
  }

  private[http] def streamVerify(id: Int): cask.Response[geny.Writable] = {
    val ver_id = VerJobId(id)
    ver_jobs.lookupJob(ver_id) match {
      case None =>
        textJsonResponse(verificationRequestRejectionString(id, JobNotFoundException))
      case Some(handle_future) =>
        val ver_handle = scala.concurrent.Await.result(handle_future, scala.concurrent.duration.Duration.Inf)
        val ast_id = ver_handle.prev_job_id
        val ast_iter: Iterator[Envelope] = ast_id.flatMap(id => ast_jobs.lookupJob(id)) match {
          case Some(astFut) =>
            val astHandle = scala.concurrent.Await.result(astFut, scala.concurrent.duration.Duration.Inf)
            astHandle.stream.iterator
          case None => Iterator.empty
        }
        val ver_iter: Iterator[Envelope] = ver_handle match {
          case VerHandle(null, null, _) => Iterator.empty
          case _ => ver_handle.stream.iterator
        }
        jsonLineResponse(ast_iter ++ ver_iter)
    }
  }

  private[http] def handleDiscard(id: Int): String = {
    val ver_id = VerJobId(id)
    ver_jobs.lookupJob(ver_id) match {
      case Some(handle_future) =>
        scala.util.Try(scala.concurrent.Await.result(handle_future, scala.concurrent.duration.Duration.Inf)) match {
          case Success(handle) =>
            val msg = formatInterruptResult(ver_id, handle.execution.cancel())
            globalLogger.info(s"The verification job #$id was successfully stopped.")
            JobDiscardAccept(msg).toJson.compactPrint
          case Failure(_) =>
            JobDiscardReject(s"The verification job #$id does not exist.").toJson.compactPrint
        }
      case _ =>
        JobDiscardReject(s"The verification job #$id does not exist.").toJson.compactPrint
    }
  }

  private[http] def handleCacheFlushAll(): String = {
    flushCache()
    CacheFlushAccept(s"The cache has been flushed successfully.").toJson.compactPrint
  }

  private[http] def handleCacheFlushFile(backend: String, file: String): String = {
    ViperCache.forgetFile(backend, file) match {
      case Some(_) =>
        globalLogger.info(s"The cache for tool ($backend) for file ($file) has been flushed successfully.")
        CacheFlushAccept(s"The cache for tool ($backend) for file ($file) has been flushed successfully.").toJson.compactPrint
      case None =>
        globalLogger.error(s"The cache does not exist for tool ($backend) for file ($file).")
        CacheFlushReject(s"The cache does not exist for tool ($backend) for file ($file).").toJson.compactPrint
    }
  }

  private[http] def handleAlloy(arg: String, solver: String): String = {
    try {
      val reporter: A4Reporter = new A4Reporter()
      val world = CompUtil.parseEverything_fromString(reporter, arg)

      val options: A4Options = new A4Options()
      options.solver = A4Options.SatSolver.parse(solver)
      options.skolemDepth = 1
      options.noOverflow = true
      options.unrolls = -1

      val commands = world.getAllCommands
      if (commands.size() != 1) {
        globalLogger.error(s"Expected only one command, but got ${commands.size()}")
        return AlloyGenerationRequestReject(s"Expected only one command, but got ${commands.size()}").toJson.compactPrint
      }
      val command = commands.get(0)
      val solution = TranslateAlloyToKodkod.execute_command(reporter, world.getAllReachableSigs, command, options)
      if (solution.satisfiable()) {
        globalLogger.info("Model is satisfiable")
        AlloyGenerationRequestComplete(solution).toJson.compactPrint
      } else {
        globalLogger.info(s"Model could not be satisfied.")
        AlloyGenerationRequestReject(s"Model could not be satisfied.").toJson.compactPrint
      }
    } catch {
      case e: Throwable =>
        globalLogger.error(s"An exception occurred during model-generation:\n${e.toString}")
        AlloyGenerationRequestReject(s"An exception occurred during model-generation:\n${e.toString}").toJson.compactPrint
    }
  }

  private def verificationRequestRejectionString(jid: Int, e: Throwable): String = e match {
    case JobNotFoundException =>
      globalLogger.error(s"The verification job #$jid does not exist.")
      VerificationRequestReject(s"The verification job #$jid does not exist.").toJson.compactPrint
    case AstConstructionFailureException =>
      globalLogger.error(s"The verification job #$jid could not be created since the AST is inconsistent.")
      VerificationRequestReject(s"The verification job #$jid could not be created since the AST is inconsistent.").toJson.compactPrint
    case _ =>
      globalLogger.error(s"The verification job #$jid resulted in a terrible error: $e")
      VerificationRequestReject(s"The verification job #$jid resulted in a terrible error: $e").toJson.compactPrint
  }

  private def jsonLineResponse(envelopes: Iterator[Envelope]): cask.Response[geny.Writable] = {
    val writable = new geny.Writable {
      override def writeBytesTo(out: java.io.OutputStream): Unit = {
        val pw = new java.io.PrintWriter(new java.io.OutputStreamWriter(out, "UTF-8"))
        try {
          var first = true
          for (env <- envelopes) {
            if (!first) pw.print('\n')
            else first = false
            pw.print(unpack(env).toJson.compactPrint)
            pw.flush()
          }
        } finally {
          pw.flush()
        }
      }
      override def httpContentType: Option[String] = Some("application/json")
    }
    cask.Response(data = writable, statusCode = 200, headers = Seq("Content-Type" -> "application/json"))
  }

  private def textJsonResponse(body: String): cask.Response[geny.Writable] = {
    val writable = new geny.Writable {
      override def writeBytesTo(out: java.io.OutputStream): Unit = {
        out.write(body.getBytes("UTF-8"))
      }
      override def httpContentType: Option[String] = Some("application/json")
    }
    cask.Response(data = writable, statusCode = 200, headers = Seq("Content-Type" -> "application/json"))
  }
}

/** cask routes for the HTTP frontend. Annotations are evaluated by cask macros
  * which require this be a concrete class with `initialize()` called.
  */
class ViperCaskRoutes(server: ViperHttpServer) extends cask.MainRoutes {
  override def host: String = "localhost"
  override def port: Int = server.port

  @cask.get("/exit")
  def exit(): cask.Response[geny.Writable] = jsonString(server.handleExit())

  @cask.post("/verify")
  def verify(req: cask.Request): cask.Response[geny.Writable] = {
    val body = req.text().parseJson.asJsObject
    val arg = body.fields("arg").asInstanceOf[JsString].value
    jsonString(server.handleVerifyPost(arg))
  }

  @cask.get("/ast/:id")
  def ast(id: Int): cask.Response[geny.Writable] = server.streamAst(id)

  @cask.get("/verify/:id")
  def verifyResult(id: Int): cask.Response[geny.Writable] = server.streamVerify(id)

  @cask.get("/discard/:id")
  def discard(id: Int): cask.Response[geny.Writable] = jsonString(server.handleDiscard(id))

  @cask.get("/cache/flush")
  def cacheFlushAll(): cask.Response[geny.Writable] = jsonString(server.handleCacheFlushAll())

  @cask.post("/cache/flush")
  def cacheFlushFile(req: cask.Request): cask.Response[geny.Writable] = {
    val body = req.text().parseJson.asJsObject
    val backend = body.fields("backend").asInstanceOf[JsString].value
    val file = body.fields("file").asInstanceOf[JsString].value
    jsonString(server.handleCacheFlushFile(backend, file))
  }

  @cask.post("/alloy")
  def alloy(req: cask.Request): cask.Response[geny.Writable] = {
    val body = req.text().parseJson.asJsObject
    val arg = body.fields("arg").asInstanceOf[JsString].value
    val solver = body.fields("solver").asInstanceOf[JsString].value
    jsonString(server.handleAlloy(arg, solver))
  }

  private def jsonString(body: String): cask.Response[geny.Writable] = {
    val writable = new geny.Writable {
      override def writeBytesTo(out: java.io.OutputStream): Unit = {
        out.write(body.getBytes("UTF-8"))
      }
      override def httpContentType: Option[String] = Some("application/json")
    }
    cask.Response(data = writable, statusCode = 200, headers = Seq("Content-Type" -> "application/json"))
  }

  initialize()
}
