package viper.server

import java.nio.file.Paths

import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.SilFrontend
import viper.silver.parser.PProgram
import viper.silver.reporter.{Message, StdIOReporter}
import viper.silver.logger.{SilentLogger, ViperLogger}


//This should eventually take only an AST (and a Logger) and verify it.
class AstVerifier (private val ver_cmd: Seq[String], private val ast_opt: Option[Program], private val _logger: ViperLogger) {
  val ver_backend = new SiliconFrontend(StdIOReporter(), _logger.get)
  val vpr_backend = new ViperBackend(ver_backend, ast_opt)
  vpr_backend.execute(ver_cmd)
}
