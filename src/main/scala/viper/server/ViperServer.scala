package viper.server

import viper.silver.logger.ViperLogger
import viper.silver.reporter._

object ViperServerRunner {

  var httpServer: ViperHttpServer = _
    
    def main(args: Array[String]): Unit = {
      // Execute ViperCoreServer
/*
      import viper.silicon.SiliconFrontend
      import java.nio.file.Paths

      val fileName: String = "wrong.sil"

      val config = new ViperConfig(args)
      config.verify()
      // Prepare program to verify:
      val otherlogger = ViperLogger("otherViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
      val frontend = new SiliconFrontend(new StdIOReporter(), otherlogger.get)

      frontend.setVerifier(frontend.createVerifier(fileName))

      frontend.init(frontend.verifier)
      frontend.reset(Paths.get(fileName))

      frontend.parsing()
      frontend.semanticAnalysis()
      frontend.translation()
      frontend.consistencyCheck()

      val program = frontend.program.get

      println("Generating new ViperCoreServer")
      val core = new ViperCoreServer(config)


      core.start()
      println("\n\nVerifying File:")

      val backendConfig = List("silicon", fileName)
      val reporter = new StdIOReporter()
    

      println("First verification:")
      core.verify(backendConfig, reporter, program)

      Thread.sleep(5000)

      println("Second verification:")
      core.verify(backendConfig, reporter, program)

      println("\n\n")


      /*
       # In the normal usecase stop will not be called immediately after the execution of the verification. (Hence the waiting)
       */

      // wait for 10 seconds
      Thread.sleep(10000)
      core.stop()
*/
      // Execute ViperHttpServer

      val config = new ViperConfig(args)
      config.verify()
      httpServer = new ViperHttpServer(config)

      httpServer.start()
  } // method main
}