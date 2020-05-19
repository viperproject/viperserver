import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.server.{AstGenerator, ViperConfig, ViperCoreServer}
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.StdIOReporter

import scala.util.{Failure, Success}

class CoreServerTest extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  private val verifiableFile = "src\\test\\resources\\viper\\let.vpr"
  private val emptyFile ="src\\test\\resources\\viper\\empty.vpr"
  private val sumFile = "src\\test\\resources\\viper\\sum_method.vpr"
  private val nonExistingFile = "src\\test\\resources\\viper\\bla.vpr"
  private val verificationErrorFile = "src\\test\\resources\\viper\\verification_error.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")
  private val console_reporter = new StdIOReporter()

  private def verification(file: String, caching: Boolean): Unit ={
    val astGen = new AstGenerator(file, console_logger)
    val config = new ViperConfig(List())
    config.verify()

    val core = new ViperCoreServer(config)
    core.start()

    val backendConfig =  if(!caching){
      SiliconConfig(List("--disableCaching"))
    }else {
      SiliconConfig(List())
    }

    console_logger.get.info("verifying file ...")

    astGen.viper_ast match {
      case Some(prog) =>

        val handler = core.verify(file, backendConfig, console_reporter, prog)
        val result = core.getFuture(handler.id)

        console_logger.get.info("Verification not yet done, test thread sent to sleep ...")
        while(!result.isCompleted){
          Thread.sleep(1000)
        }
        console_logger.get.info("... verification done, test thread waking up")

        result.onComplete({
          case Success(_) =>
            console_logger.get.info("Test completed with success")
          case Failure(_) =>
            console_logger.get.info("Test completed with failure")
        })
      case None =>
        console_logger.get.error("Test failed because of parsing!")
    }
    core.stop()
  }

  "CoreServerTest" should {
    s"verify the AST of the  Viper program 'sum_method' with caching enabled using VCS." in {
      verification(sumFile, true)
    }
    s"verify the AST of the  Viper program 'sum_method' with caching disabled using VCS." in {
      verification(sumFile, false)
    }
    s"verify the AST of the  Viper program 'let' with caching disabled using VCS." in {
      verification(verifiableFile, false)
    }
    s"verify the AST of the  Viper program 'let' with caching enabled using VCS." in {
      verification(verifiableFile, true)
    }
    s"verify the AST of the  Viper program 'verification_error' with caching disabled using VCS." in {
      verification(verificationErrorFile, false)
    }
    s"verify the AST of the  Viper program 'verification_error' with caching enabled using VCS." in {
      verification(verificationErrorFile, true)
    }
  }
}
