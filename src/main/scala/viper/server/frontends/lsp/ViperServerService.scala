// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import ch.qos.logback.classic.Logger
import viper.server.ViperConfig
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig, ViperCoreServer}
import viper.server.frontends.lsp.file.RelayHandler
import viper.server.utility.ReformatterAstGenerator
import viper.server.utility.Helpers.{getArgListFromArgString, validateViperFile}
import viper.server.utility.Helpers.validateViperFile
import viper.server.vsi.{AstJobId, DefaultVerificationServerStart, VerHandle, VerJobId}
import viper.silver.parser.ReformatPrettyPrinter
import viper.silver.ast.utility.FileLoader

import scala.concurrent.Future

class ViperServerService(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with DefaultVerificationServerStart {

  def constructAst(file: String, backend: ViperBackendConfig, localLogger: Option[Logger] = None, loader: Option[FileLoader]): AstJobId = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to start new job...")

    if (!validateViperFile(file)) {
      logger.debug(s"file not found: $file")
      return AstJobId(-1)
    }

    requestAst(file, backend, localLogger, loader)
  }

  def verifyAst(astJob: AstJobId, file: String, backend: ViperBackendConfig, localLogger: Option[Logger] = None): VerJobId = {
    if (astJob.id < 0) {
      return VerJobId(-1)
    }
    val logger = combineLoggers(localLogger)

    val ver_id = verifyWithAstJob(file, astJob, backend, localLogger)
    if (ver_id.id >= 0) {
      logger.info(s"Verification process #${ver_id.id} has successfully started.")
    } else {
      logger.debug(s"Could not start verification process. " +
        s"the maximum number of active verification jobs are currently running (${ver_jobs.MAX_ACTIVE_JOBS}).")
    }
    ver_id
  }

  def reformatFile(file: String, localLogger: Option[Logger] = None): Option[String] = {
    val logger = combineLoggers(localLogger)
    logger.debug("Requesting ViperServer to create a reformatted file.");

    val ast_generator = new ReformatterAstGenerator(logger);
    val parse_ast = ast_generator.generateViperParseAst(file);
    parse_ast match {
      case Some(p) => Some(ReformatPrettyPrinter.showProgram(p))
      case _ => {
        logger.error("Failed to generate parse AST for reformatting the program.")
        None
      }
    }
  }

  def startStreaming(jid: VerJobId, handler: RelayHandler, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    logger.debug("Sending verification request to ViperServer...")
    runStreamWithHandler(jid, handler, include_ast = true)
  }
  def startStreamingAst(jid: AstJobId, handler: RelayHandler, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    logger.debug(s"Sending ast construct request to ViperServer...")
    messageEnvelopes(jid).map(_.flatMap { case (_, iter) =>
      drainIteratorTo(iter, handler)
    })
  }
  def startStreamingVer(jid: VerJobId, handler: RelayHandler, localLogger: Option[Logger] = None): Option[Future[Unit]] = {
    val logger = combineLoggers(localLogger)
    logger.debug(s"Sending verification request to ViperServer...")
    runStreamWithHandler(jid, handler, include_ast = false).map(_.map(r => {
      logger.debug("Done verification request to ViperServer...")
      r
    }))
  }

  private def runStreamWithHandler(jid: VerJobId, handler: RelayHandler, include_ast: Boolean): Option[Future[Unit]] = {
    messageEnvelopes(jid, include_ast).map(_.flatMap { case (_, iter) =>
      drainIteratorTo(iter, handler)
    })
  }

  /** Drains `iter` on the executor's thread pool, dispatching messages to
    * `handler`. The returned future completes when the iterator is exhausted
    * or fails, after the corresponding terminal handler callback has run.
    */
  private def drainIteratorTo(iter: Iterator[viper.server.vsi.Envelope], handler: RelayHandler): Future[Unit] = {
    Future {
      try {
        for (env <- iter) handler.handleMessage(unpack(env))
        handler.onStreamSuccess()
      } catch {
        case e: Throwable =>
          handler.onStreamFailure(e)
          throw e
      }
    }(executor)
  }

  def stopVerification(jid: VerJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val logger = combineLoggers(localLogger)
    ver_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        // Free the ver slot so new jobs can be added immediately
        ver_jobs.discardJob(jid)
        handle_future.flatMap(handle => {
          // Stop ast construction
          handle.prev_job_id.foreach(astJobId => stopAstConstruction(astJobId, localLogger))
          stopOnlyVerification(handle, logger)
            .map(verResult => {
              logger.info(s"verification stopped for job #$jid")
              verResult
            })
        })
      case _ =>
        // Did not find a job with this jid.
        logger.warn(s"stopVerification - The verification job #$jid does not exist and can thus not be stopped.")
        Future.successful(false)
    }
  }

  private def stopOnlyVerification(handle: VerHandle, combinedLogger: Logger): Future[Boolean] = {
    handle match {
      // If AST construction failed, a verification handle will be returned where the execution field is null.
      case VerHandle(null, _, _) => Future.successful(false)
      case _ =>
        val interrupted = handle.execution.cancel()
        combinedLogger.info(formatInterruptResult(VerJobId(-1), interrupted))
        Future.successful(true)
    }
  }

  // Discards an AST job if it exists, the job will keep running but frees up a slot in the allowed number of jobs.
  def discardAstJobLookup(jid: AstJobId): Unit = {
    ast_jobs.lookupJob(jid).map({job =>
      ast_jobs.discardJob(jid)
      job
    })
  }

  def stopAstConstruction(jid: AstJobId, localLogger: Option[Logger] = None): Unit = {
    stopOnlyAstConstruction(jid, localLogger).map { found =>
      if (found) discardAstJob(jid)
    }
  }

  def stopOnlyAstConstruction(jid: AstJobId, localLogger: Option[Logger] = None): Future[Boolean] = {
    val combinedLogger = combineLoggers(localLogger)
    ast_jobs.lookupJob(jid) match {
      case Some(handle_future) =>
        handle_future.map { handle =>
          handle.execution.cancel()
          combinedLogger.info(s"ast construction stopped for job #$jid")
          true
        }
      case _ =>
        // Did not find a job with this jid.
        combinedLogger.warn(s"stopVerification - The AST construction job #$jid does not exist and can thus not be stopped.")
        Future.successful(false)
    }
  }

  def isSupportedType(t: String): Boolean = {
    if (t == null) {
      return false
    }
    t.toLowerCase() == "carbon" || t.toLowerCase() == "silicon" || t.toLowerCase() == "other"
  }

  def supportedTypes(): String = {
    "'carbon', 'silicon', 'other'"
  }
}
