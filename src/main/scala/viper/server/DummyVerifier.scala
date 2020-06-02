
import ch.qos.logback.classic.{Level, Logger}
import com.sun.tools.javac.code.TypeTag
import org.rogach.scallop.{ArgType, ValueConverter}
import viper.silicon.Config
import viper.silver.ast.Program
import viper.silver.frontend.{DefaultStates, SilFrontend, SilFrontendConfig}
import viper.silver.reporter._
import viper.silver.logger.ViperStdOutLogger
import viper.silver.verifier.{AbstractError => DummyError, Dependency, VerificationResult, Verifier, Failure => DummyFailure, Success => DummySuccess}


object Dummy {

  val dummyInputFilename = "dummy-file-to-prevent-cli-parser-from-complaining-about-missing-file-name.silver"

  def fromPartialCommandLineArguments(args: Seq[String],
                                      reporter: Reporter,
                                      debugInfo: Seq[(String, Any)] = Nil) : Dummy = {

    val dummy = new Dummy(reporter)
    dummy.parseCommandLine(args :+ dummyInputFilename)
    dummy
  }
}

class DummyFrontend(override val reporter: Reporter,
                      override implicit val logger: Logger = ViperStdOutLogger("SiliconFrontend", "INFO").get) extends SilFrontend {

  protected var dummyInstance: Dummy = new Dummy(reporter)

  def configureVerifier(args: Seq[String]) = {
    dummyInstance.parseCommandLine(args)
    dummyInstance.start()
    dummyInstance.config
  }

  //attach a silicon verifier to SiliconFrontend
  override def init(verifier: Verifier): Unit = {
    verifier match {
      case dummy: Dummy =>
        dummyInstance = dummy
      case _ =>
        sys.error( "Expected verifier to be an instance of Silicon but got an instance " +
          s"of ${verifier.getClass}")
    }
    super.init(verifier)
    _config = dummyInstance.config
  }

  /**
    * Create the verifier. The full command is parsed for debugging purposes only,
    * since the command line arguments will be passed later on via
    * [[viper.silver.verifier.Verifier.parseCommandLine]].
    */
  override def createVerifier(fullCmd: String): Verifier = {
    dummyInstance = new Dummy(reporter)
    dummyInstance
  }
}

class Dummy(val reporter: Reporter) extends Verifier {

  var config: Config = _

  override def name: String = "DummyVerifier"
  override def version: String = ""
  override def buildVersion: String = ""
  override def copyright: String = ""
  override def debugInfo(info: Seq[(String, Any)]): Unit = {}
  override def dependencies: Seq[Dependency] = Seq()

  override def parseCommandLine(args: Seq[String]): Unit = {
    config = new Config(args)
  }

  override def start(): Unit = {}

  override def verify(program: Program): VerificationResult = {
    println("DummyVerifier.verify()")
    DummySuccess
  }

  /** Stops the verifier. The behaviour of subsequent calls to `start` or `verify`
    * is unspecified.
    */
  override def stop(): Unit = {}
}


