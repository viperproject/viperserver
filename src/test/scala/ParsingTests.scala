import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.AstGenerator
import viper.silver.logger.ViperStdOutLogger

class ParsingTests extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  private val verifiableFile = "src\\test\\resources\\viper\\let.vpr"
  private val emptyFile ="src\\test\\resources\\viper\\empty.vpr"
  private val sumFile = "src\\test\\resources\\viper\\sum_method.vpr"
  private val nonExistingFile = "src\\test\\resources\\viper\\bla.vpr"
  private val typeErrorFile = "src\\test\\resources\\viper\\type_error.vpr"
  private val parseErrorFile = "src\\test\\resources\\viper\\parse_error.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")

  "AstGenerator" should {
    s"parse and translate the viper file sum_method" in {
      val ast_gen = new AstGenerator(sumFile, console_logger)
    }
    s"parse and translate the viper file verifiable_file" in {
      val ast_gen = new AstGenerator(verifiableFile, console_logger)
    }
    s"parse and translate an empty file" in {
      val ast_gen = new AstGenerator(emptyFile, console_logger)
    }
    s"parse file with type error but fail when translating it." in {
      val ast_gen = new AstGenerator(typeErrorFile, console_logger)
    }
    s"fail when parsing the file." in {
      val ast_gen = new AstGenerator(parseErrorFile, console_logger)
    }
    s"fail when parsing non-existing file." in {
      try {
        val ast_gen = new AstGenerator(nonExistingFile, console_logger)
      } catch {
        case e: java.nio.file.NoSuchFileException => console_logger.get.error(s"No file at: ${nonExistingFile}")
      }
    }
  }
}
