package viper.server

object ViperServerRunner {

  var httpServer: ViperHttpServer = _

  /** Start VCS in HTTP mode.
    * */
  def startHttpServer(args: Array[String]): Unit = {
    httpServer = new ViperHttpServer(args)
    httpServer.start()
  }

  def main(args: Array[String]): Unit = {
    startHttpServer(args)
  } // method main
}
