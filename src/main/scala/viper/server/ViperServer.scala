package viper.server

import org.rogach.scallop.ScallopOption

import scala.concurrent.ExecutionContext

object ViperServerRunner {

//  var viperHttpBasic: ViperHttpBasic = _
//  var viperHttpStandard: ViperHttpStandard = _
//  var viperHttpCustom: ViperHttpCustom = _
//  var viperHttpServer: ViperHttpServer = _
  var newViperHttpServer: NewViperHttpServer = _

  /** Creates batch script to run a <a href="https://github.com/viperproject/viper_client">viper_client</a> written in python.
    * */
  private def writeBatchScripts(portOption: ScallopOption[Int], file: Option[String]): Unit ={
    if(!portOption.isDefined){
      println("port was not defined, batch files won't be created.")
      return
    }

    val port = portOption.apply()

    import java.io.{File, FileWriter}

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

//  def exampleBasic(args: Array[String]): Unit ={
//    implicit val executionContext = ExecutionContext.global
//    val config = new ViperConfig(args)
//    config.verify()
//
//    viperHttpBasic = new ViperHttpBasic(config)
//    viperHttpBasic.start(Some(viperHttpBasic.routes()))
//
//    writeBatchScripts(config.port, Some("sum_method.vpr"))
//  }
//
//  def exampleStandard(args: Array[String]): Unit ={
//    implicit val executionContext = ExecutionContext.global
//    val config = new ViperConfig(args)
//    config.verify()
//
//    viperHttpStandard = new ViperHttpStandard(config)
//    viperHttpStandard.start(Some(viperHttpStandard.routes()))
//
//    writeBatchScripts(config.port, Some("sum_method.vpr"))
//  }
//
//  def exampleCustomizable(args: Array[String]): Unit ={
//    implicit val executionContext = ExecutionContext.global
//    val config = new ViperConfig(args)
//    config.verify()
//
//    viperHttpCustom = new ViperHttpCustom(config)
//    viperHttpCustom.start(Some(viperHttpCustom.routes()))
//
//    writeBatchScripts(config.port, Some("sum_method.vpr"))
//  }

  /** Start VCS in HTTP mode.
    * */
  def startHttpServer(args: Array[String]): Unit ={
//    implicit val executionContext = ExecutionContext.global
//    val config = new ViperConfig(args)
//    config.verify()
//
//    viperHttpServer = new ViperHttpServer(config)
//    viperHttpServer.start(Some(viperHttpServer.routes()))
//
//    writeBatchScripts(config.port, Some("sum_method.vpr"))
  }

  /** Start VCS in HTTP mode.
    * */
  def startNewHttpServer(args: Array[String]): Unit ={
    implicit val executionContext = ExecutionContext.global
    val config = new ViperConfig(args)
    config.verify()

    newViperHttpServer = new NewViperHttpServer(config)
    newViperHttpServer.start()

    writeBatchScripts(config.port, Some("sum_method.vpr"))
  }

  def main(args: Array[String]): Unit = {
    startNewHttpServer(args)
  } // method main
}
