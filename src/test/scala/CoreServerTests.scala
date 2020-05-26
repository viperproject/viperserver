import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.server.{AstGenerator, VerificationJobHandler, ViperConfig, ViperCoreServer}
import viper.silver.logger.{SilentLogger, ViperStdOutLogger}
import viper.silver.reporter.{NoopReporter, StdIOReporter}
import viper.silver.verifier.{AbstractError, VerificationError, VerificationResult, Failure => VerificationFailure, Success => VerificationSuccess}


import scala.concurrent.Future
import scala.util.{Failure, Success}

class CoreServerTest extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  private val verifiableFile = "src\\test\\resources\\viper\\let.vpr"
  private val emptyFile ="src\\test\\resources\\viper\\empty.vpr"
  private val sumFile = "src\\test\\resources\\viper\\sum_method.vpr"
  private val verificationErrorFile = "src\\test\\resources\\viper\\verification_error.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")
  private val silent_logger = SilentLogger()
  private val console_reporter = new StdIOReporter()
  private val silent_reporter = NoopReporter

  "An instance of ViperCoreServer" when {
    "verifying a single file with caching disabled" should {
      val file = verificationErrorFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      "execute 'start()' without exceptions" in {
        core.start()
      }

      var handler: VerificationJobHandler = null
      "execute 'verify()' without exceptions" in {
        handler = core.verify(file, SiliconConfig(List("--disableCaching")), silent_reporter, astGen.viper_ast.get)
        assert(handler != null)
      }

      "have 'verifiy()' return a JobHandler with  positive id when executed" in {
        assert(handler.id >= 0)
      }

      var result_future: Future[VerificationResult] = null
      "have 'getFuture()' return a future a of the VerificationResult" in {
        result_future = core.getFuture(handler.id)
        assert(result_future != null)
      }

      "eventually have a successfully completed future with the expected verification result" in {
        while (!result_future.isCompleted) {
          Thread.sleep(500)
        }
        result_future.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file == verificationErrorFile)
              case VerificationSuccess => assert(file != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
      }

      "execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying a single file with caching enabled" should {
      val file = sumFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      "execute 'start()' without exceptions" in {
        core.start()
      }

      var handler: VerificationJobHandler = null
      "execute 'verify()' without exceptions" in {
        handler = core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)
        assert(handler != null)
      }

      "have 'verifiy()' return a JobHandler with  positive id when executed" in {
        assert(handler.id >= 0)
      }

      var result_future: Future[VerificationResult] = null
      "have 'getFuture()' return a future a of the VerificationResult" in {
        result_future = core.getFuture(handler.id)
        assert(result_future != null)
      }

      "eventually have a successfully completed future with the expected verification result" in {
        while (!result_future.isCompleted) {
          Thread.sleep(500)
        }
        result_future.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file == verificationErrorFile)
              case VerificationSuccess => assert(file != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
      }

      "execute stop without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "verifying three files with caching disabled" should {
      val file1 = sumFile
      val file2 = verifiableFile
      val astGen1 = new AstGenerator(file1, silent_logger)
      val astGen2 = new AstGenerator(file2, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      core.start()

      var handler1: VerificationJobHandler = null
      var handler2: VerificationJobHandler = null
      var handler3: VerificationJobHandler = null
      "execute 'verify()' repeatedly without exceptions" in {
        handler1 = core.verify(file1, SiliconConfig(List("--disableCaching")), silent_reporter, astGen1.viper_ast.get)
        handler2 = core.verify(file2, SiliconConfig(List("--disableCaching")), silent_reporter, astGen2.viper_ast.get)
        handler3 = core.verify(file2, SiliconConfig(List("--disableCaching")), silent_reporter, astGen2.viper_ast.get)
        assert(handler1 != null)
        assert(handler2 != null)
        assert(handler3 != null)
      }

      "have 'verifiy()' return JobHandlers with unique positive ids when executed " in {
        assert(handler1.id == 0)
        assert(handler2.id == 1)
        assert(handler3.id == 2)
      }

      var result_future1: Future[VerificationResult] = null
      var result_future2: Future[VerificationResult] = null
      var result_future3: Future[VerificationResult] = null
      "have 'getFuture()' return a future a of the VerificationResult" in {
        result_future1 = core.getFuture(handler1.id)
        result_future2 = core.getFuture(handler2.id)
        result_future3 = core.getFuture(handler3.id)
        assert(result_future1 != null)
        assert(result_future2 != null)
        assert(result_future3 != null)
      }

      "eventually have a successfully completed future with the expected verification result" in {
        while (!result_future1.isCompleted && !result_future2.isCompleted && !result_future3.isCompleted) {
          Thread.sleep(500)
        }
        result_future1.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file1 == verificationErrorFile)
              case VerificationSuccess => assert(file1 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
        result_future2.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file2 == verificationErrorFile)
              case VerificationSuccess => assert(file2 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
        result_future3.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file2 == verificationErrorFile)
              case VerificationSuccess => assert(file2 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
      }

      "execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying three files with caching enabled" should {
      val file1 = sumFile
      val file2 = verifiableFile
      val astGen1 = new AstGenerator(file1, silent_logger)
      val astGen2 = new AstGenerator(file2, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      var handler1: VerificationJobHandler = null
      var handler2: VerificationJobHandler = null
      var handler3: VerificationJobHandler = null
      "execute 'verify()' repeatedly without exceptions" in {
        handler1 = core.verify(file1, SiliconConfig(List()), silent_reporter, astGen1.viper_ast.get)
        handler2 = core.verify(file2, SiliconConfig(List()), silent_reporter, astGen2.viper_ast.get)
        handler3 = core.verify(file2, SiliconConfig(List()), silent_reporter, astGen2.viper_ast.get)
        assert(handler1 != null)
        assert(handler2 != null)
        assert(handler3 != null)
      }

      "have 'verifiy()' return JobHandlers with unique positive ids when executed " in {
        assert(handler1.id == 0)
        assert(handler2.id == 1)
        assert(handler3.id == 2)
      }

      var result_future1: Future[VerificationResult] = null
      var result_future2: Future[VerificationResult] = null
      var result_future3: Future[VerificationResult] = null
      "have 'getFuture()' return a future a of the VerificationResult" in {
        result_future1 = core.getFuture(handler1.id)
        result_future2 = core.getFuture(handler2.id)
        result_future3 = core.getFuture(handler3.id)
        assert(result_future1 != null)
        assert(result_future2 != null)
        assert(result_future3 != null)
      }

      "eventually have a successfully completed future with the expected verification result" in {
        while (!result_future1.isCompleted && !result_future2.isCompleted && !result_future3.isCompleted) {
          Thread.sleep(500)
        }
        result_future1.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file1 == verificationErrorFile)
              case VerificationSuccess => assert(file1 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
        result_future2.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file2 == verificationErrorFile)
              case VerificationSuccess => assert(file2 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
        result_future3.onComplete({
          case Success(verRes) =>
            verRes match {
              case VerificationFailure(_) => assert(file2 == verificationErrorFile)
              case VerificationSuccess => assert(file2 != verificationErrorFile)
            }
          case Failure(_) => fail()
        })
      }

      "execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "verifying a multiple file with caching enabled" should {
      val file1 = sumFile
      val file2 = verifiableFile
      val astGen1 = new AstGenerator(file1, silent_logger)
      val astGen2 = new AstGenerator(file2, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      var handler1: VerificationJobHandler = null
      var handler2: VerificationJobHandler = null
      var handler3: VerificationJobHandler = null

      "execute 'flushCache()' without exceptions after verifying files" in {
        handler1 = core.verify(file1, SiliconConfig(List()), silent_reporter, astGen1.viper_ast.get)
        core.flushCache()
        handler2 = core.verify(file2, SiliconConfig(List()), silent_reporter, astGen2.viper_ast.get)
        core.flushCache()
        handler3 = core.verify(file2, SiliconConfig(List()), silent_reporter, astGen2.viper_ast.get)
        core.flushCache()
        assert(handler1 != null)
        assert(handler2 != null)
        assert(handler3 != null)
      }

      "execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "maximum capacity is exceeded" should {
      val file = sumFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val handler = core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)
      core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)
      core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)

      "have 'verify()' return a VerificationJobHandler with negative id." in {
        val spillHandler = core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)
        assert(spillHandler.id < 0)
      }

      "until a job handle has been freed." in {
        val result_future = core.getFuture(handler.id)
        while (!result_future.isCompleted) {
          Thread.sleep(500)
        }
        val newHandler = core.verify(file, SiliconConfig(List()), silent_reporter, astGen.viper_ast.get)
        assert(newHandler.id > 0)
      }

      "execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
}