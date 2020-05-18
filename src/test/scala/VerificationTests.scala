import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.AstGenerator
import viper.silver.logger.{SilentLogger, ViperStdOutLogger}

class VerificationTests extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  private val verifiableFile = "src\\test\\resources\\viper\\let.vpr"
  private val emptyFile ="src\\test\\resources\\viper\\empty.vpr"
  private val sumFile = "src\\test\\resources\\viper\\sum_method.vpr"
  private val nonExistingFile = "src\\test\\resources\\viper\\bla.vpr"
  private val verificationErrorFile = "src\\test\\resources\\viper\\verification_error.vpr"

  private val testSimpleViperCode_args = Array("--disableCaching", verifiableFile)
  private val testEmptyFile_args = Array("--disableCaching", emptyFile)
  private val testNonExistingFile_args = Array("--disableCaching", nonExistingFile)
  private val testSumFile_args = Array("--disableCaching", sumFile)
  private val testVerificationErrorFile_args = Array("--disableCaching", verificationErrorFile)

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")

  "VerificationTest" should {
    s"verify the AST of the parsed and translated Viper program 'sum_method'." in {
      val astGen = new AstGenerator(sumFile, SilentLogger())
      val astVer = new AstVerifier(testSumFile_args, astGen.viper_ast, console_logger)
    }
    s"verify the AST of a parsed and translated Viper program that does not verify." in {
      val astGen : AstGenerator = new AstGenerator(verificationErrorFile, SilentLogger())
      val astVer = new AstVerifier(testVerificationErrorFile_args, astGen.viper_ast, console_logger)
    }
    s"verify the AST of the parsed and translated Viper program 'let'." in {
      val astGen : AstGenerator = new AstGenerator(verifiableFile, SilentLogger())
      val astVer = new AstVerifier(testSimpleViperCode_args, astGen.viper_ast, console_logger)
    }
    s"verify the empty AST of the empty Viper programs." in {
      val astGen : AstGenerator = new AstGenerator(emptyFile, SilentLogger())
      val astVer = new AstVerifier(testEmptyFile_args, astGen.viper_ast, console_logger)
    }
  }
}
