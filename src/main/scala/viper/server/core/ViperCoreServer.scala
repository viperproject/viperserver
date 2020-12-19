// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.actor.ActorRef
import viper.server.ViperConfig
import viper.server.vsi.{AstHandle, AstJobId, VerJobId, VerificationServer}
import viper.silver.ast.Program
import viper.silver.logger.ViperLogger

import scala.concurrent.Future
import scala.language.postfixOps

class ViperCoreServer(val _args: Array[String]) extends VerificationServer with ViperPost {

  override type AST = Program

  // --- VCS : Configuration ---
  protected var _config: ViperConfig = _
  final def config: ViperConfig = _config

  protected var _logger: ViperLogger = _
  final def logger: ViperLogger = _logger

  /** Configures an instance of ViperCoreServer.
    *
    * This function must be called before any other. Calling any other function before this one
    * will result in an IllegalStateException.
    * */
  def start(): Unit = {
    _config = new ViperConfig(_args)
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    println(s"Writing [level:${config.logLevel()}] logs into " +
      s"${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    ViperCache.initialize(logger.get, config.backendSpecificCache())

    super.start(config.maximumActiveJobs())
    println(s"ViperCoreServer started.")
  }

  def requestAst(arg_list: List[String]): AstJobId = {
    require(config != null)

    val task_backend = new AstWorker(arg_list, logger.get)
    val ast_id = initializeAstConstruction(task_backend)

    if (ast_id.id >= 0) {
      logger.get.info(s"Verification process #${ast_id.id} has successfully started.")
    } else {
      logger.get.error(s"Could not start verification process. " +
        s"The maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ast_id
  }

  def verify(ast_id: AstJobId, backend_config: ViperBackendConfig): VerJobId = {

    if (!isRunning) throw new IllegalStateException("Instance of VerificationServer already stopped")
    require(backend_config != null)

    val programId = s"ViperAst#${ast_id.id}"
    val args: List[String] = backend_config.toList

    ast_jobs.lookupJob(ast_id) match {
      case Some(handle_future) =>
        val task_backend_fut =
          handle_future.map((handle: AstHandle[Program]) => {
            val art: Future[Program] = handle.artifact
            art.map(program => {
              new VerificationWorker(logger.get, args :+ programId, program)
            }).recover({
              case e: Throwable =>
                println(s"### As exception has occurred while constructing Viper AST: $e")
                throw e
            })

          }).flatten

        initializeVerificationProcess(task_backend_fut, Some(ast_id))

      case None =>
        logger.get.error(s"Could not start verification process for non-existent $ast_id")
        VerJobId(-1)
    }
  }

  /** Verifies a Viper AST using the specified backend.
    *
    * Expects a non-null backend config and Viper AST.
    * */
  def verify(programId: String, backend_config: ViperBackendConfig, program: Program): VerJobId = {
    require(program != null && backend_config != null)

    val args: List[String] = backend_config.toList
    val task_backend = new VerificationWorker(logger.get, args :+ programId, program)
    val ver_id = initializeVerificationProcess(Future.successful(task_backend), None)

    if (ver_id.id >= 0) {
      logger.get.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.get.error(s"Could not start verification process. " +
        s"The maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  override def streamMessages(jid: VerJobId, clientActor: ActorRef): Option[Future[Unit]] = {
    logger.get.info(s"Streaming results for job #${jid.id}.")
    super.streamMessages(jid, clientActor)
  }

  def flushCache(): Unit = {
    if(!isRunning) {
      throw new IllegalStateException("Instance of ViperCoreServer already stopped")
    }
    ViperCache.resetCache()
    logger.get.info(s"The cache has been flushed.")
  }

  override def stop(): Unit = {
    logger.get.info(s"Stopping ViperCoreServer")
    super.stop()
  }


}