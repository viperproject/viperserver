package viper.server

import org.rogach.scallop.ScallopOption

import scala.concurrent.ExecutionContext


// === HTTP SERVER =====================================

object ViperServerRunner {

  var newViperHttpServer: ViperHttpServer = _

  /** Start VCS in HTTP mode.
    * */
  def startNewHttpServer(args: Array[String]): Unit ={
    implicit val executionContext = ExecutionContext.global
    val config = new ViperConfig(args)
    config.verify()

    newViperHttpServer = new ViperHttpServer(config)
    newViperHttpServer.start()
  }

  def main(args: Array[String]): Unit = {
    startNewHttpServer(args)
  } // method main
}
