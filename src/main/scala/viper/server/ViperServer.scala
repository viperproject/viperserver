package viper.server

import viper.silver.logger.ViperLogger
import viper.silver.reporter._
import scala.concurrent.ExecutionContextExecutor
import akka.actor.ActorSystem

import scala.util.{Failure, Success}

object ViperServerRunner {
  import viper.server.ViperCoreServer

  var httpServer: ViperHttpServer = _
    
    def main(args: Array[String]): Unit = {
      // Execute ViperCoreServer
      implicit val system: ActorSystem = ActorSystem("Main")
      implicit val executionContext: ExecutionContextExecutor = system.dispatcher

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
      val first_handler = core.verify(backendConfig, reporter, program)

      Thread.sleep(5000)

      println("Second verification:")
      val second_handler = core.verify(backendConfig, reporter, program)

      Thread.sleep(2500)
      core.flushCache()
      
      println("Third verification:")
      val third_handler = core.verify(backendConfig, reporter, program)

      val resFuture = core.getFuture(third_handler.id)


      resFuture.onComplete({res => res match {
        case Success(_) =>
          println("Completed with success")
          core.stop()
        case Failure(_) =>
          println("Completed with failure")
          core.stop()
      }})




/*
      // Execute ViperHttpServer

      val config = new ViperConfig(args)
      config.verify()
      httpServer = new ViperHttpServer(config)

      httpServer.start()
*/


  } // method main
}