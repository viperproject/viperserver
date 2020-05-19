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


  /** Creates batch script to run python <a href="https://github.com/viperproject/viper_client">viper_client</a>
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

  def main(args: Array[String]): Unit = {
    implicit val executionContext = ExecutionContext.global

    val config = new ViperConfig(args)
    config.verify()

    httpServer = new ViperHttpServer(config)
    httpServer.start()
    writeBatchScripts(config.port, Some("sum_method.vpr"))
  } // method main
}
