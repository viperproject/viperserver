package viper.server

import viper.silver.logger.ViperLogger
import viper.silver.reporter._
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.ExecutionContext
import akka.actor.ActorSystem

import scala.util.{Failure, Success}

import viper.server.ViperBackendConfigs._

object ViperServerRunner {
  import viper.server.ViperCoreServer

  var httpServer: ViperHttpServer = _
    
    def main(args: Array[String]): Unit = {
      implicit val executionContext = ExecutionContext.global

      import viper.silicon.SiliconFrontend
      import java.nio.file.Paths

      
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
      // Finished preparing program -----------------------------------------------------------------------------


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


  } // method main
}