import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.server.{AstGenerator, VerificationJobHandler, ViperConfig, ViperCoreServer}
import viper.silver.logger.{SilentLogger, ViperStdOutLogger}
import viper.silver.reporter._

import scala.concurrent.Future
import scala.util.{Failure, Success}



class CoreServerTest extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  implicit var actor_system: ActorSystem = ActorSystem("Test")
  val client_actor = actor_system.actorOf(ClientActor.props())

  object ClientActor {
    case object Terminate
    def props(): Props = Props(new  ClientActor())
  }

  class ClientActor() extends Actor{

    override def receive: PartialFunction[Any, Unit] = {
      case _: OverallSuccessMessage =>
        actor_system.terminate()
      case _: OverallFailureMessage =>
        actor_system.terminate()
    }
  }

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
      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var handler: VerificationJobHandler = null
      "be able to execute 'verify()' without exceptions" in {
        handler = core.verify(file, SiliconConfig(List("--disableCaching")), astGen.viper_ast.get)
        assert(handler != null)
      }

      "be able to have 'verifiy()' return a JobHandler with positive id when executed" in {
        assert(handler.id >= 0)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future = core.getMessagesFuture(handler.id)
        assert(messages_future != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed sucessfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(500)
        }
        messages_future.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying a single file with caching enabled" should {
      val file = sumFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var handler: VerificationJobHandler = null
      "be able to execute 'verify()' without exceptions" in {
        handler = core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)
        assert(handler != null)
      }

      "be able to have 'verifiy()' return a JobHandler with  positive id when executed" in {
        assert(handler.id >= 0)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future = core.getMessagesFuture(handler.id)
        assert(messages_future != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed sucessfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(500)
        }
        messages_future.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "execute stop without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "verifying multiple files with caching disabled" should {
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
      "be able to execute 'verify()' repeatedly without exceptions" in {
        handler1 = core.verify(file1, SiliconConfig(List("--disableCaching")), astGen1.viper_ast.get)
        handler2 = core.verify(file2, SiliconConfig(List("--disableCaching")), astGen2.viper_ast.get)
        handler3 = core.verify(file2, SiliconConfig(List("--disableCaching")), astGen2.viper_ast.get)
        assert(handler1 != null)
        assert(handler2 != null)
        assert(handler3 != null)
      }

      "be able to have 'verifiy()' return JobHandlers with unique positive ids when executed " in {
        assert(handler1.id == 0)
        assert(handler2.id == 1)
        assert(handler3.id == 2)
      }

      var messages_future1: Future[Seq[Message]] = null
      var messages_future2: Future[Seq[Message]] = null
      var messages_future3: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future1 = core.getMessagesFuture(handler1.id)
        messages_future2 = core.getMessagesFuture(handler2.id)
        messages_future3 = core.getMessagesFuture(handler3.id)
        assert(messages_future1 != null)
        assert(messages_future2 != null)
        assert(messages_future3 != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed sucessfully" in {
        while (!messages_future1.isCompleted && !messages_future2.isCompleted && !messages_future3.isCompleted) {
          Thread.sleep(500)
        }
        messages_future1.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
        messages_future2.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
        messages_future3.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying multiple files with caching enabled" should {
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
      "be able to execute 'verify()' repeatedly without exceptions" in {
        handler1 = core.verify(file1, SiliconConfig(List()), astGen1.viper_ast.get)
        handler2 = core.verify(file2, SiliconConfig(List()), astGen2.viper_ast.get)
        handler3 = core.verify(file2, SiliconConfig(List()), astGen2.viper_ast.get)
        assert(handler1 != null)
        assert(handler2 != null)
        assert(handler3 != null)
      }

      "have 'verifiy()' return JobHandlers with unique positive ids when executed " in {
        assert(handler1.id == 0)
        assert(handler2.id == 1)
        assert(handler3.id == 2)
      }

      var messages_future1: Future[Seq[Message]] = null
      var messages_future2: Future[Seq[Message]] = null
      var messages_future3: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future1 = core.getMessagesFuture(handler1.id)
        messages_future2 = core.getMessagesFuture(handler2.id)
        messages_future3 = core.getMessagesFuture(handler3.id)
        assert(messages_future1 != null)
        assert(messages_future2 != null)
        assert(messages_future3 != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed sucessfully" in {
        while (!messages_future1.isCompleted && !messages_future2.isCompleted && !messages_future3.isCompleted) {
          Thread.sleep(500)
        }
        messages_future1.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
        messages_future2.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
        messages_future3.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "verifying files with caching enabled" should {
      val file1 = sumFile
      val file2 = verifiableFile
      val astGen1 = new AstGenerator(file1, silent_logger)
      val astGen2 = new AstGenerator(file2, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      "be able to execute 'flushCache()' without exceptions after verifying files" in {
        val handler1 = core.verify(file1, SiliconConfig(List()), astGen1.viper_ast.get)
        core.flushCache()
        val handler2 = core.verify(file2, SiliconConfig(List()), astGen2.viper_ast.get)
        core.flushCache()
        val handler3 = core.verify(file2, SiliconConfig(List()), astGen2.viper_ast.get)
        core.flushCache()
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "maximum capacity of verification jobs is exceeded" should {
      val file = sumFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val handler = core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)
      core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)
      core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)

      "have 'verify()' return a VerificationJobHandler with negative id." in {
        val spillHandler = core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)
        assert(spillHandler.id < 0)
      }

      "until a job handle has been freed." in {
        val result_future = core.getMessagesFuture(handler.id)
        while (!result_future.isCompleted) {
          Thread.sleep(500)
        }
        val newHandler = core.verify(file, SiliconConfig(List()), astGen.viper_ast.get)
        assert(newHandler.id > 0)
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
  "An instance of ViperCoreServer" when {
    "verifying a file with caching disabled" should {
      val file = sumFile
      val astGen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      core.start()

      var handler: VerificationJobHandler = null
      handler = core.verify(file, SiliconConfig(List("--disableCaching")), astGen.viper_ast.get)

      "be able to have 'streamMessages()' stream Viper messages to a client actor." in {
        core.streamMessages(handler.id, client_actor)
      }

      Thread.sleep(10000)

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
}