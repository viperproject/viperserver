package viper.server

import java.nio.file.Paths

import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.SilFrontend
import viper.silver.parser.PProgram
import viper.silver.reporter.{StdIOReporter}
import viper.silver.logger.ViperLogger

class AstGenerator (private val _logger: ViperLogger){
  private var ver_backend: SilFrontend = create_backend()

  /** Creates a backend to work with the file.
    *
    */
  private def create_backend() : SilFrontend = {
    _logger.get.info(s"Creating new verification backend.")
    new SiliconFrontend(StdIOReporter("Parsing Reporter", true), _logger.get)
  }

  /** Parses and translates a Viper file into a Viper AST.
    *
    */
  def generateViperAst(vpr_file_path: String): Option[Program] = {
    val args: Array[String] = Array(vpr_file_path)
    _logger.get.info(s"Parsing viper file.")
    ver_backend.setVerifier(ver_backend.createVerifier(args.mkString(" ")))
    ver_backend.prepare(args)
    ver_backend.init(ver_backend.verifier)
    ver_backend.reset(Paths.get(ver_backend.config.file()))
    val parse_ast = parse()
    translate(parse_ast)
  }

  /** Parses a Viper file
    *
    */
  private def parse(): Option[PProgram] = {
    ver_backend.parsing()
    if(ver_backend.errors.isEmpty){
      _logger.get.info("There was no error while parsing!")
      Some(ver_backend.parsingResult)
    } else{
      _logger.get.error("There was some error while parsing!")
      None
    }
  }

  /** Translates a Parsed Viper file into a Viper AST
    *
    */
  private def translate(parse_ast : Option[PProgram]) : Option[Program] = {
    if(parse_ast.isDefined){
      _logger.get.info(s"Translating parsed file.")
      ver_backend.semanticAnalysis()
      ver_backend.translation()
      ver_backend.consistencyCheck()
      if(ver_backend.errors.isEmpty){
        _logger.get.info("There was no error while translating!")
        ver_backend.verifier.stop()
        return Some(ver_backend.translationResult)
      } else {
        _logger.get.error ("There was some error while translating!")
      }
    }
    ver_backend.verifier.stop()
    None
  }
}

