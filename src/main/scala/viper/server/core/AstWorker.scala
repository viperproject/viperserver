// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import viper.server.ViperConfig
import viper.server.utility.AstGenerator
import viper.server.vsi.AstConstructionException
import viper.silver.ast.Program
import viper.silver.ast.utility.FileLoader
import viper.silver.reporter.{Entity, ExceptionReport}
import viper.silver.verifier.VerificationResult


object ViperFileNotFoundException extends AstConstructionException
object AstConstructionInterrupted extends AstConstructionException
object InvalidArgumentsException extends AstConstructionException
object AstConstructionFailureException extends AstConstructionException
object OutOfResourcesException extends AstConstructionException

case class ServerCrashException(e: Throwable) extends Exception(e)


class AstWorker(val file: String,
                override val logger: Logger,
                private val config: ViperConfig,
                private val loader: Option[FileLoader]
               )(override val executor: VerificationExecutionContext)
  extends MessageReportingTask[Option[Program]] {

  private def constructAst(): Option[Program] = {
    val reporter = new ActorReporter("AstGenerationReporter")
    val astGen = new AstGenerator(logger, reporter, disablePlugins = config.disablePlugins())

    val ast_option: Option[Program] = try {
      astGen.generateViperAst(file, loader)
    } catch {
      case _: java.nio.file.NoSuchFileException =>
        logger.error(s"The file ($file) for which verification has been requested was not found.")
        registerTaskEnd(false)
        throw ViperFileNotFoundException
      case e@ (_: InterruptedException | _: java.nio.channels.ClosedByInterruptException) =>
        logger.info(s"AstWorker ($file) has been interrupted: $e")
        registerTaskEnd(false)
        throw AstConstructionInterrupted
      case e: Throwable =>
        reporter report ExceptionReport(e)
        logger.error(s"Creation/Execution of an AstGenerator instance ($file) resulted in $e.")
        registerTaskEnd(false)
        throw ServerCrashException(e)
    }

    registerTaskEnd(true)
    ast_option match {
      case None =>
        logger.info(s"The file ($file) contained syntax errors, type errors, or is simply inconsistent.")
      case _ =>
    }
    ast_option
  }

  override def mapEntityVerificationResult(entity: Entity, result: VerificationResult): VerificationResult = {
    logger.error(s"unexpected operation: AstWorker received an entity success or entity failure message, which should not occur while constructing a Viper AST")
    ???
  }

  override def call(): Option[Program] = constructAst()
}
