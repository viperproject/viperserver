// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.utility

import ch.qos.logback.classic.Logger
import viper.silver.ast.Program
import viper.silver.ast.utility.FileLoader
import viper.silver.frontend.{SilFrontend, ViperAstProvider}
import viper.silver.parser.PProgram
import viper.silver.reporter.{NoopReporter, Reporter}

class ViperAstGenerator(private val _logger: Logger,
                        private val _reporter: Reporter = NoopReporter,
                        private val argList: Seq[String] = Seq(),
                        private val disablePlugins: Boolean = false) extends AstGeneratorBase[Program](_logger, _reporter, argList, disablePlugins) {

  /** Creates a backend that reads and parses the file
    */
  protected override val _frontend: SilFrontend = {
    _logger.info(s"Creating new ViperAstGenerator instance.")
    new ViperAstProvider(_reporter, disablePlugins = disablePlugins)
  }

  /** Extracts the fully translated Program from the frontend
    */
  protected override def extractResult(): Program = _frontend.translationResult

  /** Parses and translates a Viper file into a Viper AST (Program).
    *
    * Throws an exception when passed a non-existent file!
    */
  def generateViperAst(vpr_file_path: String, loader: Option[FileLoader] = None): Option[Program] = {
    generateViperAstImpl(vpr_file_path, loader)
  }

  /** Parses a Viper file into a parse AST (PProgram) without full translation.
    *
    * Throws an exception when passed a non-existent file!
    */
  def generateViperParseAst(vpr_file_path: String, loader: Option[FileLoader] = None): Option[PProgram] = {
    generateViperAstImpl(vpr_file_path, loader)
    if (_frontend.errors.isEmpty) {
      Some(_frontend.parsingResult)
    } else {
      None
    }
  }
}