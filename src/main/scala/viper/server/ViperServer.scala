package viper.server

import java.nio.file.Paths

import org.rogach.scallop.ScallopOption
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.silicon.SiliconFrontend
import viper.silver.logger.ViperLogger
import viper.silver.reporter.StdIOReporter

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object ViperServerRunner {

  var httpServer: ViperHttpServer = _

  def silasTestCode(args: Array[String]): Unit ={
    implicit val executionContext = ExecutionContext.global

    val programName: String = "wrong.sil"
    val programID: String = "nonExistentFileName.sil"

    val config = new ViperConfig(args)
    config.verify()
    // Prepare program to verify ------------------------------------------------------------------------------
    val otherlogger = ViperLogger("otherViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    val frontend = new SiliconFrontend(new StdIOReporter(), otherlogger.get)

    frontend.setVerifier(frontend.createVerifier(programName))

    frontend.prepare(Seq(programName))

    frontend.init(frontend.verifier)

    frontend.reset(Paths.get(programName))

    frontend.parsing()
    frontend.semanticAnalysis()
    frontend.translation()
    frontend.consistencyCheck()

    val program = frontend.program.get

    println("Generating new ViperCoreServer")
    val core = new ViperCoreServer(config)


    core.start()
    println("\n\nVerifying File:")

    val backendConfig = SiliconConfig(List())
    val reporter = new StdIOReporter()


    println("First verification:")
    val first_handler = core.verify(programID, backendConfig, reporter, program)
    val first_result = core.getFuture(first_handler.id)

    Thread.sleep(5000)

    println("Second verification:")
    val second_handler = core.verify(programID, backendConfig, reporter, program)
    val second_result = core.getFuture(second_handler.id)

    Thread.sleep(2500)
    core.flushCache()

    println("Third verification:")
    val third_handler = core.verify(programID, backendConfig, reporter, program)
    val third_result = core.getFuture(third_handler.id)

    //Thread.sleep(2500)

    println("Fourth verification:")
    val fourth_handler = core.verify(programID, backendConfig, reporter, program)
    val fourth_result = core.getFuture(fourth_handler.id)

    third_result.onComplete({
      case Success(_) =>
        fourth_result.onComplete({
          case Success(_) =>
            println("Completed with success")
            core.stop()
          case Failure(_) =>
            println("Completed with failure")
            core.stop()
        })
      case Failure(_) =>
        println("Third result failed")
        core.stop()
    })


    /*
        // Execute ViperHttpServer

        val config = new ViperConfig(args)
        config.verify()
        httpServer = new ViperHttpServer(config)

        httpServer.start()
    */
  }

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

  def main(args: Array[String]): Unit = {
    implicit val executionContext = ExecutionContext.global

    val config = new ViperConfig(args)
    config.verify()

    // Prepare program to verify ------------------------------------------------------------------------------
    val otherlogger = ViperLogger("otherViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())

    httpServer = new ViperHttpServer(config)
    httpServer.start()
    writeBatchScripts(config.port, Some("sum_method.vpr"))
  } // method main
}
