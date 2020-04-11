package viper.server

import java.nio.file.Paths

import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink, Source}
import viper.server.ViperServerRunner.{ReporterActor, _logger, system}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import viper.silicon.SiliconFrontend
import viper.silver.ast.Program
import viper.silver.frontend.SilFrontend
import viper.silver.parser.PProgram
import viper.silver.reporter.Message
import viper.silver.logger.ViperLogger
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}

import scala.concurrent.ExecutionContextExecutor

class AstGenerator (private val vpr_file_path:String, private val _logger: ViperLogger){
  var ver_backend: SilFrontend = initialize_backend()
  var parse_ast : PProgram = parse()
  var translated_ast : Program = translate()

  implicit val system = ViperServerRunner.system
  implicit val materializer: ActorMaterializer = ViperServerRunner.materializer
  implicit val executionContext: ExecutionContextExecutor = ViperServerRunner.executionContext

  private def initialize_backend() : SilFrontend = {
    //		val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure)
    //									   .toMat(Sink.asPublisher(false))(Keep.both)
    //									   .run()
    val some_reporter = system.actorOf(ReporterActor.props(0, null), s"parse_reporter")
    new SiliconFrontend(new ActorReporter(some_reporter, "silicon"), _logger.get)
  }

  private def parse(): PProgram = {
    val args:Array[String] = Array(vpr_file_path)
    ver_backend.setVerifier( ver_backend.createVerifier(args.mkString(" ")) )
    ver_backend.prepare(args)
    ver_backend.init( ver_backend.verifier )
    ver_backend.reset(Paths.get(ver_backend.config.file()))
    ver_backend.parsing()
    ver_backend.parsingResult
  }

  private def translate() = {
    ver_backend.semanticAnalysis()
    ver_backend.translation()
    ver_backend.translationResult
  }
}
