// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.utility

import ch.qos.logback.classic.Logger
import viper.silver.ast.utility.FileLoader
import viper.silver.frontend.{SilFrontend, ReformatterAstProvider}
import viper.silver.parser.PProgram
import viper.silver.reporter.{NoopReporter, Reporter}

class ReformatterAstGenerator(private val _logger: Logger,
                               private val _reporter: Reporter = NoopReporter,
                               private val argList: Seq[String] = Seq(),
                               private val disablePlugins: Boolean = false) extends AstGeneratorBase[PProgram](_logger, _reporter, argList, disablePlugins) {

  protected override val _frontend: SilFrontend = {
    _logger.info(s"Creating new ReformatterAstGenerator instance.")
    new ReformatterAstProvider(_reporter)
  }

  /** Extracts the parse AST (PProgram) from the frontend
    */
  protected override def getResult: PProgram = _frontend.parsingResult

  /** Parses a Viper file into a parse AST (PProgram) without macro expansion.
    *
    * Throws an exception when passed a non-existent file!
    */
  def generateViperParseAst(vpr_file_path: String, loader: Option[FileLoader] = None): Option[PProgram] = {
    generateViperAstImpl(vpr_file_path, loader)
  }
}