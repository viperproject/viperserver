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

  private val verifiableFile_id = "let"
  private val emptyFile_id ="empty"
  private val sumFile_id = "sum_method"
  private val nonExistingFile_id = "bla"
  private val verificationErrorFile_id = "verification_error"

  private val testSimpleViperCode_args = Array("--disableCaching", verifiableFile)
  private val testEmptyFile_args = Array("--disableCaching", emptyFile)
  private val testNonExistingFile_args = Array("--disableCaching", nonExistingFile)
  private val testSumFile_args = Array(sumFile)
  private val testVerificationErrorFile_args = Array("--disableCaching", verificationErrorFile)

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")
  private val console_reporter = new StdIOReporter()

  "CoreServerTest" should {
    s"verify the AST of the  Viper program 'sum_method' using VCS." in {
      val astGen = new AstGenerator(sumFile, console_logger)
      val config = new ViperConfig(List())
      config.verify()

      val core = new ViperCoreServer(config)
      core.start()

      val backendConfig = SiliconConfig(List())

      console_logger.get.info("verifying file ...")

      astGen.translated_ast match {
        case Some(prog) =>
//          console_logger.get.info("sleeping ...")
//          Thread.sleep(15000)
//          console_logger.get.info("... waking up")

          val handler = core.verify(sumFile_id, backendConfig, console_reporter, prog)
          val result = core.getFuture(handler.id)

          while(!result.isCompleted){
            print(".")
          }

          result.onComplete({
            case Success(_) =>
              console_logger.get.info("Completed with success")
            case Failure(_) =>
              console_logger.get.info("Completed with failure")
          })
        case None =>
          console_logger.get.error("Parsing failed!")
      }
    }
  }
}
