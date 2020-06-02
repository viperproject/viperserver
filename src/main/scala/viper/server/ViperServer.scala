package viper.server

import java.nio.file.Paths

import akka.actor.ActorRef
import org.rogach.scallop.ScallopOption
import viper.server.ViperBackendConfigs.{CustomConfig, SiliconConfig}
import viper.silicon.SiliconFrontend
import viper.silver.logger.{SilentLogger, ViperLogger, ViperStdOutLogger}
import viper.silver.reporter
import viper.silver.reporter.{ConfigurationConfirmation, CopyrightReport, EntityFailureMessage, EntitySuccessMessage, ExceptionReport, ExecutionTraceReport, ExternalDependenciesReport, InternalWarningMessage, InvalidArgumentsReport, Message, NoopReporter, OverallFailureMessage, OverallSuccessMessage, PongMessage, Reporter, SimpleMessage, StdIOReporter, Time, WarningsDuringParsing, format}
import viper.silver.verifier.{AbstractError, VerificationError, VerificationResult, Failure => VerificationFailure, Success => VerificationSuccess}

import scala.util.{Failure, Success, Try}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

case class TestReporter(name: String = "stdout_reporter", timeInfo: Boolean = true) extends Reporter {

  var counter = 0

  // includes the unit name (e.g., seconds, sec, or s).
  private def timeStr: Time => String = format.formatMillisReadably

  private def plurals(num_things: Int): String = if (num_things==1) "" else "s"

  private def bulletFmt(num_items: Int): String = s"%${num_items.toString.length}d"

  def report(msg: Message): Unit = {
//    println("sleep")
//    Thread.sleep(2000)
    println("Reporting to Console from TestReporter =====================================================")
    msg match {
      case OverallFailureMessage(v, t, res) =>
        val num_errors = res.errors.length
        if (!timeInfo)
          println( s"$v found $num_errors error${plurals(num_errors)}:" )
        else
          println( s"$v found $num_errors error${plurals(num_errors)} in ${timeStr(t)}:" )
        res.errors.zipWithIndex.foreach { case(e, n) => println( s"  [${bulletFmt(num_errors).format(n)}] ${e.readableMessage}" ) }

      case OverallSuccessMessage(v, t) =>
        if (!timeInfo)
          println( s"$v finished verification successfully." )
        else
          println( s"$v finished verification successfully in ${timeStr(t)}." )

      case ExceptionReport(e) =>
        /** Theoretically, we may encounter an exceptional message that has
          * not yet been reported via AbortedExceptionally. */
        println( s"Verification aborted exceptionally: ${e.toString}" )
        Option(e.getCause) match {
          case Some(cause) => println( s"  Cause: ${cause.toString}" )
          case None =>
        }
      //println( s"  See log file for more details: ..." )

      case ExternalDependenciesReport(deps) =>
        val s: String = (deps map (dep => {
          s"  ${dep.name} ${dep.version}, located at ${dep.location}."
        })).mkString("\n")
        println( s"The following dependencies are used:\n$s" )

      case WarningsDuringParsing(warnings) =>
        warnings.foreach(println)

      case InvalidArgumentsReport(tool_sig, errors) =>
        errors.foreach(e => println(s"  ${e.readableMessage}"))
        println( s"Run with just --help for usage and options" )

      case ExecutionTraceReport(memberTraces, axioms, functionPostAxioms) =>
        println("Execution trace for the last run:")
        println(s"  Members:")
        memberTraces.foreach(t => println(s"    $t"))
        println(s"  Axioms:")
        axioms.foreach(t => println(s"    $t"))
        println(s"  FunctionPostAxioms:")
        functionPostAxioms.foreach(a => println(s"    $a"))


      case CopyrightReport(text) =>
        println( text )

      case EntitySuccessMessage(_, _, _) => println("MSG TYPE: entity success")   // FIXME Currently, we only print overall verification results to STDOUT.
      case EntityFailureMessage(_, _, _, _) => println("MSG TYPE: entity failure")// FIXME Currently, we only print overall verification results to STDOUT.
      case ConfigurationConfirmation(_) =>     // TODO  use for progress reporting
        println( s"Configuration confirmation:" )
      case InternalWarningMessage(_) =>        // TODO  use for progress reporting
        println( s"Internal warning:" )
      case sm:SimpleMessage =>
        println( sm.text )
      case pm: PongMessage =>
        println(pm.text)
      case _ =>
        println( s"Cannot properly print message of unsupported type: $msg" )
    }
    counter = counter + 1
  }
}

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

  def stream_testing(args: Array[String]): Unit = {
    //CONFIG
    implicit val executionContext = ExecutionContext.global
    val backend_config = SiliconConfig(List("--disableCaching"))
//    val backend_config = CustomConfig(List("--disableCaching"))
    val config = new ViperConfig(args)
    config.verify()

    //FILES
    val file = "src\\test\\resources\\viper\\sum_method.vpr"
//    val file = "src\\test\\resources\\viper\\verification_error.vpr"

    //LOG & REPORT
//    val consoleLogger = ViperStdOutLogger("test_logger", "ALL")
    val consoleLogger = SilentLogger()
    val reporter = TestReporter()
//    val reporter = NoopReporter

    //VERIFICATION
    val core = new ViperCoreServer(config)
    core.start()

    val parser = new AstGenerator(file, consoleLogger)
    val vjh = core.verify(file, backend_config, parser.viper_ast.get)

    core.reportBackendMessages(vjh.id, reporter)
    Thread.sleep(8000)

    core.stop()
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
    stream_testing(args)
//    startHttpServer(args)
  } // method main
}
