// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.Done
import akka.actor.ActorRef
import akka.util.Timeout
import ch.qos.logback.classic.Logger
import viper.server.ViperConfig
import viper.server.vsi.{AstHandle, AstJobId, VerJobId, VerificationServer}
import viper.silver.ast.Program
import viper.silver.ast.utility.FileLoader
import viper.silver.logger.ViperLogger

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.postfixOps

abstract class ViperCoreServer(val config: ViperConfig)(implicit val executor: VerificationExecutionContext) extends VerificationServer with ViperPost {

  override type AST = Program

  // --- VCS : Configuration ---

  override lazy val askTimeout: Timeout = Timeout(config.actorCommunicationTimeout() milliseconds)

  /** global logger that should be used for the server's entire lifetime. Log messages are reported to the global logger as well as local one (if they exist) */
  val globalLogger: Logger = getGlobalLogger(config)

  /** allows subclasses to return their own global logger */
  def getGlobalLogger(config: ViperConfig): Logger = {
    val logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    println(s"Writing [level:${config.logLevel()}] logs into " +
      s"${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")
    logger.get
  }

  def combineLoggers(localLogger: Option[Logger]): Logger = {
    localLogger match {
      case Some(ll) => MultiLogger("Combined logger for ViperCoreServer", Seq(globalLogger, ll)).get
      case _ => globalLogger
    }
  }

  /** Configures an instance of ViperCoreServer.
    *
    * This function must be called before any other. Calling any other function before this one
    * will result in an IllegalStateException.
    */
  def start(): Future[Done] = {
    ViperCache.initialize(globalLogger, config.backendSpecificCache(), config.cacheFile.toOption)
    start(config.maximumActiveJobs()) map { _ =>
      globalLogger.info(s"ViperCoreServer has started.")
      Done
    }
  }

  def requestAst(file: String, backend_config: ViperBackendConfig, localLogger: Option[Logger] = None, loader: Option[FileLoader] = None): AstJobId = {
    require(config != null)
    val logger = combineLoggers(localLogger)
    val args: List[String] = backend_config.toList
    val task_backend = new AstWorker(file, args, logger, config, loader)(executor)
    val ast_id = initializeAstConstruction(task_backend)

    if (ast_id.id >= 0) {
      logger.info(s"AST process #${ast_id.id} has successfully started.")
    } else {
      logger.error(s"Could not start AST process. " +
        s"The maximum number of active AST jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ast_id
  }

  def verifyWithAstJob(programId: String, ast_id: AstJobId, backend_config: ViperBackendConfig, localLogger: Option[Logger] = None): VerJobId = {
    val logger = combineLoggers(localLogger)

    if (!isRunning) throw new IllegalStateException("Instance of VerificationServer already stopped")
    require(backend_config != null)

    val args: List[String] = backend_config.toList

    ast_jobs.lookupJob(ast_id) match {
      case Some(handle_future) =>
        val task_backend_maybe_fut: Future[Option[VerificationWorker]] =
          handle_future.map((handle: AstHandle[Option[Program]]) => {
            val program_maybe_fut: Future[Option[Program]] = handle.artifact
            program_maybe_fut.map(_.map(new VerificationWorker(args, programId, _, logger, config)(executor))).recover({
              case e: Throwable =>
                logger.error(s"### An exception has occurred while constructing Viper AST: $e")
                throw e
            })
          }).flatten

        initializeVerificationProcess(task_backend_maybe_fut, Some(ast_id))

      case None =>
        logger.error(s"Could not start verification process for non-existent $ast_id")
        VerJobId(-1)
    }
  }

  /** Verifies a Viper AST using the specified backend.
    *
    * Expects a non-null backend config and Viper AST.
    * */
  def verify(programId: String, backend_config: ViperBackendConfig, program: Program, localLogger: Option[Logger] = None): VerJobId = {
    require(program != null && backend_config != null)

    val logger = combineLoggers(localLogger)

    val args: List[String] = backend_config.toList
    val task_backend = new VerificationWorker(args, programId, program, logger, config)(executor)
    val ver_id = initializeVerificationProcess(Future.successful(Some(task_backend)), None)

    if (ver_id.id >= 0) {
      logger.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.error(s"Could not start verification process. " +
        s"The maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  override def streamMessages(jid: VerJobId, clientActor: ActorRef, include_ast: Boolean): Option[Future[Done]] = {
    globalLogger.info(s"Streaming results for job #${jid.id}.")
    super.streamMessages(jid, clientActor, include_ast)
  }
  override def streamMessages(jid: AstJobId, clientActor: ActorRef): Option[Future[Done]] = {
    globalLogger.info(s"Streaming results for job #${jid.id}.")
    super.streamMessages(jid, clientActor)
  }

  def flushCache(localLogger: Option[Logger] = None): Boolean = {
    val logger = combineLoggers(localLogger)
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    val res = ViperCache.resetCache()
    logger.info(s"The cache has been flushed.")
    res
  }

  /** if a file path is provided, only the cache for the particular file is flushed */
  def flushCachePartially(backendAndFilePath: Option[(String, String)], localLogger: Option[Logger] = None): Boolean = {
    val logger = combineLoggers(localLogger)
    backendAndFilePath match {
      case Some((backend, file)) =>
        logger.info(s"Requesting ViperServer to flush the cache for backend $backend and file $file...")
        val flushed_file_opt = ViperCache.forgetFile(backend, file)

        if (flushed_file_opt.isDefined) {
          logger.debug(s"ViperServer has confirmed that the cache for backend $backend and file $file has been flushed.")
        } else {
          logger.debug(s"Error while requesting ViperServer to flush the cache for backend $backend and $file: not found.")
        }
        flushed_file_opt.isDefined
      case None =>
        logger.info("Flush entire cache...")
        flushCache(localLogger)
    }
  }

  override def stop(): Future[List[String]] = {
    globalLogger.info(s"Stopping ViperCoreServer")
    super.stop()
  }
}
