// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.utility

import ch.qos.logback.classic.Logger
import viper.silver.ast.Program
import viper.silver.frontend.{SilFrontend, ViperAstProvider}
import viper.silver.parser.PProgram
import viper.silver.reporter.{NoopReporter, Reporter}

class AstGenerator(private val _logger: Logger,
                   private val _reporter: Reporter = NoopReporter) extends ProgramDefinitionsProvider {

  /** Creates a backend that reads and parses the file
    */
  protected override val _frontend: SilFrontend = {
    _logger.info(s"Creating new verification backend.")
    new ViperAstProvider(_reporter)
  }

  /** Parses and translates a Viper file into a Viper AST.
    *
    * Throws an exception when passed an inexistent file!
    */
  def generateViperAst(vpr_file_path: String): Option[Program] = {
    val args: Array[String] = Array(vpr_file_path)
    _logger.info(s"Parsing viper file.")
    _frontend.execute(args)
    if (_frontend.errors.isEmpty) {
      reportProgramStats()
      Some(_frontend.translationResult)
    } else {
      None
    }
  }

  /** Parses a Viper file
    */
  private def parse(): Option[PProgram] = {
    _frontend.parsing()
    if(_frontend.errors.isEmpty) {
      _logger.info("There was no error while parsing!")
      Some(_frontend.parsingResult)
    } else {
      _logger.error(s"There was some error while parsing: ${_frontend.errors}")
      None
    }
  }

  /** Translates a Parsed Viper file into a Viper AST
    */
  private def translate(): Option[Program] = {
    _logger.info(s"Translating parsed file.")
    _frontend.semanticAnalysis()
    _frontend.translation()
    _frontend.consistencyCheck()
    _frontend.verifier.stop()

    if (_frontend.errors.isEmpty) {
      _logger.info("There was no error while translating!")
      Some(_frontend.translationResult)
    } else {
      _logger.error(s"There was some error while translating ${_frontend.errors}")
      None
    }
  }
}
