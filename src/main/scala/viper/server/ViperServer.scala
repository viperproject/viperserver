package viper.server

import java.nio.file.Paths

import akka.actor.ActorRef
import org.rogach.scallop.ScallopOption
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.silicon.SiliconFrontend
import viper.silver.logger.{SilentLogger, ViperLogger, ViperStdOutLogger}
import viper.silver.reporter
import viper.silver.reporter.{ConfigurationConfirmation, CopyrightReport, EntityFailureMessage, EntitySuccessMessage, ExceptionReport, ExecutionTraceReport, ExternalDependenciesReport, InternalWarningMessage, InvalidArgumentsReport, Message, NoopReporter, OverallFailureMessage, OverallSuccessMessage, Reporter, SimpleMessage, StdIOReporter, Time, WarningsDuringParsing, format}
import viper.silver.verifier.{AbstractError, VerificationError, VerificationResult, Failure => VerificationFailure, Success => VerificationSuccess}
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object ViperServerRunner {

  var httpServer: ViperHttpServer = _

  /** Creates batch script to run a <a href="https://github.com/viperproject/viper_client">viper_client</a> written in python.
    * */
  private def writeBatchScripts(portOption: ScallopOption[Int], file: Option[String]): Unit ={
    if(!portOption.isDefined){
      println("port was not defined, batch files won't be created.")
      return
    }

    val port = portOption.apply()

    import java.io.{File, FileReader, FileWriter}
    import java.nio.file.Paths

    val term_file = new File("ter.bat")
    val ver_file = new File("ver.bat")

    val term_writer = new FileWriter(term_file)
    val ver_writer = new FileWriter(ver_file)

    term_writer.write(s"cd ..\\viper_client-master & python client.py -c terminate -p $port & cd ..\\viperserver")
    file match {
      case Some(fileName) =>
      ver_writer.write(s"cd ..\\viper_client-master & " +
        s"python client.py -c verify -p $port -f ${fileName} &" +
        s"cd ..\\viperserver")
      case None =>
        ver_writer.write(s"cd ..\\viper_client-master & python client.py -c verify -p $port & cd ..\\viperserver")
    }

    term_writer.close()
    ver_writer.close()
  }

  /** Start VCS in HTTP mode.
    * */
  def startHttpServer(args: Array[String]): Unit ={
    implicit val executionContext = ExecutionContext.global
    val config = new ViperConfig(args)
    config.verify()

    httpServer = new ViperHttpServer(config)
    httpServer.start()
    writeBatchScripts(config.port, Some("sum_method.vpr"))
  }

  def main(args: Array[String]): Unit = {
    startHttpServer(args)
  } // method main
}
