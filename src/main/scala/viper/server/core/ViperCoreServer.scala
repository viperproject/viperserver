// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.actor.ActorRef
import viper.server.ViperConfig
import viper.server.vsi.{VerJobId, VerificationServer}
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




//  def requestAst(input: String): AstJobId = {
//    require(config != null)
//
//    if (!ast_jobs.newJobsAllowed) {
//      logger.get.error(s"Could not start AST construction process. " +
//        s"The maximum number of active jobs are currently running (${ast_jobs.MAX_ACTIVE_JOBS}).")
//      return AstJobId(-1)
//    }
//
//    val task_backend = new AstWorker(input, logger.get)
//    val ast_id = initializeAstConstruction(Future.successful(task_backend))
//    ast_jobs.run_job(ast_id)
//
//    if (ast_id.id >= 0) {
//      logger.get.info(s"AST construction of $ast_id has successfully started.")
//    }
//    ast_id
//  }

  /** Verifies a Viper AST using the specified backend.
    *
    * Expects a non-null backend config and Viper AST.
    * */
  def verify(programId: String, backend_config: ViperBackendConfig, program: Program): VerJobId = {
    require(program != null && backend_config != null)

    val args: List[String] = backend_config.toList
    val task_backend = new VerificationWorker(_config, logger.get, args :+ programId, program)
    val jid = initializeVerificationProcess(Future.successful(task_backend))
    if(jid.id >= 0) {
      logger.get.info(s"Verification process #${jid.id} has successfully started.")
    } else {
      logger.get.error(s"Could not start verification process. " +
        s"The maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    jid
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