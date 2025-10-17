// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.utility

import ch.qos.logback.classic.Logger

import java.nio.file.NoSuchFileException
import viper.server.utility.Helpers.validateViperFile
import viper.silver.ast.utility.FileLoader
import viper.silver.frontend.SilFrontend
import viper.silver.reporter.{NoopReporter, Reporter, PProgramReport}

abstract class AstGeneratorBase[T](private val _logger: Logger,
                                    private val _reporter: Reporter = NoopReporter,
                                    private val argList: Seq[String] = Seq(),
                                    private val disablePlugins: Boolean = false) extends ProgramDefinitionsProvider {

  /** Creates a backend that reads and parses the file
    */
  protected val _frontend: SilFrontend

  /** Extracts the result of type T from the frontend
    */
  protected def extractResult(): T

  /** Parses and translates a Viper file into the appropriate AST type.
    *
    * Throws an exception when passed a non-existent file!
    */
  def generateViperAstImpl(vpr_file_path: String, loader: Option[FileLoader] = None): Option[T] = {

    if (!validateViperFile(vpr_file_path)) {
      _logger.error(s"No such file: `$vpr_file_path`")
      throw new NoSuchFileException(vpr_file_path)
    }

    _logger.info(s"Parsing `$vpr_file_path` ...")

    // We need to pass all arguments relevant to AST creation to the frontend (e.g. everything plugin-related), but we
    // cannot pass any backend-specific arguments, since the argument parser used here does not know those and argument
    // parsing would fail.
    // So for now, we use a whitelist of arguments that are relevant for AST creation and pass only those.
    var filteredArgs: Seq[String] = Seq(vpr_file_path)
    for (option <- optionWhiteList) {
      val optionArgIndex = argList.indexOf(option)
      if (optionArgIndex != -1) {
        // argument is the next item in the argList
        filteredArgs = argList.slice(optionArgIndex, optionArgIndex + 2) ++ filteredArgs
      } else {
        argList.find(_.startsWith(s"${option}=")) match {
          case Some(arg) =>
            filteredArgs = Seq(arg) ++ filteredArgs
          case _ =>
        }
      }
    }
    for (flag <- flagWhiteList) {
      val flagArgIndex = argList.indexOf(flag)
      if (flagArgIndex != -1) {
        filteredArgs = Seq(argList(flagArgIndex)) ++ filteredArgs
      }
    }
    _frontend.execute(filteredArgs, loader)
    if (_frontend.saProgram.isDefined || _frontend.pProgram.isDefined) {
      val pprog = _frontend.saProgram.getOrElse(_frontend.pProgram.get)
      _frontend.reporter.report(PProgramReport(_frontend.saProgram.isDefined, pprog))
    }
    if (_frontend.program.isDefined) {
      reportProgramStats()
    }
    if (_frontend.errors.isEmpty) {
      Some(extractResult())
    } else {
      _logger.info(s"Errors occurred while translating `$vpr_file_path`: ${_frontend.errors}")
      None
    }
  }

  // Parameters that are relevant for AST creation and are boolean flags
  val flagWhiteList: Seq[String] = Seq("--disableDefaultPlugins", "--disableAdtPlugin", "--disableTerminationPlugin")
  // Parameters that are relevant for AST creation and are passed additional values
  val optionWhiteList: Seq[String] = Seq("--plugin")
}