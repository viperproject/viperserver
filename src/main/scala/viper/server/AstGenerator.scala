package viper.server

import java.nio.file.Paths

import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.SilFrontend
import viper.silver.parser.PProgram
import viper.silver.reporter.{Message, StdIOReporter}
import viper.silver.logger.ViperLogger

import scala.concurrent.ExecutionContextExecutor

class AstGenerator (private val vpr_file_path:String, private val _logger: ViperLogger){
  var ver_backend: SilFrontend = initialize_backend()
  var parse_ast : Option[PProgram] = parse()
  var translated_ast : Option[Program] = translate()

  //TODO deal with non-existing files.
  private def initialize_backend() : SilFrontend = {
    _logger.get.info(s"Creating new verification backend.")
    new SiliconFrontend(StdIOReporter("some_name", true), _logger.get)
  }

  private def parse(): Option[PProgram] = {
    _logger.get.info(s"Parsing viper file.")
    val args: Array[String] = Array(vpr_file_path)
    ver_backend.setVerifier(ver_backend.createVerifier(args.mkString(" ")))
    ver_backend.prepare(args)
    ver_backend.init(ver_backend.verifier)
    ver_backend.reset(Paths.get(ver_backend.config.file()))
    ver_backend.parsing()
    if(ver_backend.errors.isEmpty){
      _logger.get.info("There was NO error while parsing!")
      Some(ver_backend.parsingResult)
    }else{
      _logger.get.info("There was SOME error while parsing!")
      None
    }
  }

  private def translate() : Option[Program] = {
    if(parse_ast.isDefined){
      _logger.get.info(s"Translating parsed file.")
      ver_backend.semanticAnalysis()
      ver_backend.translation()
      if(ver_backend.errors.isEmpty){
        _logger.get.info("There was NO error while translating!")
        return Some(ver_backend.translationResult)
      }else {
        _logger.get.info ("There was SOME error while translating!")
      }
    }
    None
  }
}

