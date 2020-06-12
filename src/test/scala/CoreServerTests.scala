import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.ViperBackendConfigs.SiliconConfig
import viper.server.{AstGenerator, VerificationJobHandler, ViperConfig, ViperCoreServer}
import viper.silver.ast.Program
import viper.silver.logger.SilentLogger
import viper.silver.reporter._

import scala.concurrent.Future
import scala.util.{Failure, Success}



class CoreServerTest extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  implicit var actor_system: ActorSystem = ActorSystem("Test")
  val client_actor_0 = actor_system.actorOf(ClientActor.props(0))
  val client_actor_1 = actor_system.actorOf(ClientActor.props(1))
  val client_actor_2 = actor_system.actorOf(ClientActor.props(2))
  val actorTestRestults: Array[Option[Boolean]] = Array(None, None, None)


  object ClientActor {
    case object Terminate
    def props(shouldSucceed: Int): Props = Props(new ClientActor(shouldSucceed))
  }

  class ClientActor(private val test_no: Int) extends Actor{

    override def receive: PartialFunction[Any, Unit] = {
      case m: Message =>
        m match {
          case osm: OverallSuccessMessage =>
            println("osm")
          case ofm: OverallFailureMessage =>
            actorTestRestults(test_no) = Some(false)
          case _ =>
        }
      case ClientActor.Terminate =>
        println("terminating external actor system")
        system.terminate()
    }
  }

  private val emptyFile ="src\\test\\resources\\viper\\empty.vpr"
  private val sumFile = "src\\test\\resources\\viper\\sum_method.vpr"
  private val verificationErrorFile = "src\\test\\resources\\viper\\verification_error.vpr"

//  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")
  private val silent_logger = SilentLogger()

  val noCacheBackend = SiliconConfig(List("--disableCaching"))
  val cacheBackend = SiliconConfig(List())

  "An instance of ViperCoreServer" when {
    "verifying a single file with caching disabled" should {
      val file = verificationErrorFile
      val ast_gen = new AstGenerator(file, silent_logger)
      val ast = ast_gen.viper_ast.get

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      "be able to execute 'start()' without exceptions" in {
        core.start()
      }
      "not be able to execute 'start()' more than once without exceptions" in {
        assertThrows[Throwable]{
          core.start()
        }
      }

      var handler: VerificationJobHandler = null
      "be able to execute 'verify()' without exceptions" in {
        handler = core.verify(file, noCacheBackend, ast)
        assert(handler != null)
      }

      "be able to have 'verify()' return a JobHandler with non-negative id when executed" in {
        assert(handler.id >= 0)
        handler = core.verify(file, noCacheBackend, ast)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future = core.getMessagesFuture(handler.id)
        assert(messages_future != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed successfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[Throwable] {
          core.verify(file, noCacheBackend, ast)
        }
      }
    }
    "verifying a single file with caching enabled" should {
      val file = sumFile
      val ast_gen = new AstGenerator(file, silent_logger)

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var handler: VerificationJobHandler = null
      "be able to execute 'verify()' without exceptions" in {
        handler = core.verify(file, cacheBackend, ast_gen.viper_ast.get)
        assert(handler != null)
      }

      "be able to have 'verify()' return a JobHandler with  non-negative id when executed" in {
        assert(handler.id >= 0)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages" in {
        messages_future = core.getMessagesFuture(handler.id)
        assert(messages_future != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed successfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "unsuccessfully complete the future returned by 'getMessagesFuture()' for an inexistent job" in {
        val wrongJid = 42
        messages_future = core.getMessagesFuture(wrongJid)
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case Success(verRes) => fail()
          case Failure(_) => succeed
        })
      }

      "execute stop without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[Throwable] {
          core.verify(file, noCacheBackend, ast_gen.viper_ast.get)
        }
      }
    }
    "verifying multiple files with caching disabled and retrieving results via 'getMessagesFuture()'" should {
      val files: List[String] = List(emptyFile, sumFile, verificationErrorFile)
      val programs: List[Program] = files.map(f => {
        val ast_gen = new AstGenerator(f, silent_logger)
        ast_gen.viper_ast.get
      })

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      core.start()

      val filesAndProgs: List[(String, Program)] = files.zip(programs)
      var handlers: List[VerificationJobHandler] = null
      "be able to have 'verify()' repeatedly without exceptions" in {
        handlers = filesAndProgs map { case (f, p) => core.verify(f, noCacheBackend, p) }
      }

      "be able to have 'verify()' return JobHandlers with unique non-negative ids when executed " in {
        assert(handlers(0).id == 0)
        assert(handlers(1).id == 1)
        assert(handlers(2).id == 2)
      }

      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages containing the expected verification result" in {
        val messages_futures: List[Future[Seq[Message]]] = handlers.map(h => {
          core.getMessagesFuture(h.id)
        })
        val filesAndFutures = files.zip(messages_futures)
        filesAndFutures.foreach({ case (f, mf) =>
          while (!mf.isCompleted) {
            Thread.sleep(100)
          }
          mf.onComplete({
            case Success(msgs) =>
              msgs.last match {
                case m: OverallSuccessMessage =>
                  assert(f != verificationErrorFile)
                case m: OverallFailureMessage =>
                  assert(f == verificationErrorFile)
                case _ => fail()
              }
            case Failure(e) => fail()
          })
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying multiple files with caching enabled and retrieving results via 'getMessagesFuture()'" should {
      val files: List[String] = List(emptyFile, sumFile, verificationErrorFile)
      val programs: List[Program] = files.map(f => {
        val ast_gen = new AstGenerator(f, silent_logger)
        ast_gen.viper_ast.get
      })

      val config = new ViperConfig(List())
      config.verify()
      var core = new ViperCoreServer(config)
      core.start()

      val filesAndProgs: List[(String, Program)] = files.zip(programs)
      var handlers: List[VerificationJobHandler] = null
      "be able to have 'verify()' repeatedly without exceptions" in {
        handlers = filesAndProgs map { case (f, p) => core.verify(f, noCacheBackend, p) }
      }

      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages containing the expected verification result" in {
        val messages_futures: List[Future[Seq[Message]]] = handlers.map(h => {
          core.getMessagesFuture(h.id)
        })
        val filesAndFutures = files.zip(messages_futures)
        filesAndFutures.foreach({ case (f, mf) =>
          while (!mf.isCompleted) {
            Thread.sleep(100)
          }
          mf.onComplete({
            case Success(msgs) =>
              msgs.last match {
                case m: OverallSuccessMessage =>
                  assert(f != verificationErrorFile)
                case m: OverallFailureMessage =>
                  assert(f == verificationErrorFile)
                case _ => fail()
              }
            case Failure(e) => fail()
          })
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying multiple files with caching disabled and retrieving results via 'streamMessages()" should {
      val file1 = emptyFile
      val file2 = sumFile
      val file3 = verificationErrorFile

      val ast_gen1 = new AstGenerator(file1, silent_logger)
      val ast_gen2 = new AstGenerator(file2, silent_logger)
      val ast_gen3 = new AstGenerator(file3, silent_logger)
      val ast1 = ast_gen1.viper_ast.get
      val ast2 = ast_gen2.viper_ast.get
      val ast3 = ast_gen3.viper_ast.get

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val vjh1 = core.verify(file1, noCacheBackend, ast1)
      val vjh2 = core.verify(file2, noCacheBackend, ast2)
      val vjh3 = core.verify(file3, noCacheBackend, ast3)

      "be able to have 'streamMessages()' stream a sequence of Viper messages without errors" in {
        core.streamMessages(vjh1.id, client_actor_0)
        core.streamMessages(vjh2.id, client_actor_1)
        core.streamMessages(vjh3.id, client_actor_2)
//        while(actors_finished <= 2){
//          Thread.sleep(100)
//        }
      }


      "have the stream of messages contain the expected verification result" in {
        Thread.sleep(15000)
        assert(actorTestRestults(0) == Some(true))
        assert(actorTestRestults(1) == Some(true))
        assert(actorTestRestults(2) == Some(false))
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying an incorrect viper program several times with caching enabled" should {
      val file = verificationErrorFile
      val ast_gen = new AstGenerator(file, silent_logger)
      val ast = ast_gen.viper_ast.get

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      "produce an OverallFailure Message with a non-empty error list upon first verification." in {
        val vjh_original = core.verify(file, cacheBackend, ast)
        val messages_future_original = core.getMessagesFuture(vjh_original.id)
          while (!messages_future_original.isCompleted) {
            Thread.sleep(500)
          }
          messages_future_original.onComplete({
            case Success(msgs) =>
              msgs.last match {
                case ofm: OverallFailureMessage =>
                  assert(ofm.result.errors.nonEmpty)
                case _ => fail()
              }
            case Failure(e) => fail()
          })
        }

      "produce an OverallFailure Message with an empty error list when re- verified." in {
        val vjh_cached = core.verify(file, cacheBackend, ast)
        val messages_future_cached = core.getMessagesFuture(vjh_cached.id)
        while (!messages_future_cached.isCompleted) {
          Thread.sleep(100)
        }
        messages_future_cached.onComplete({
          case Success(msgs) =>
            msgs.last match {
              case ofm: OverallFailureMessage =>
                assert(ofm.result.errors.isEmpty)
              case _ => fail()
            }
          case Failure(e) => fail()
        })
      }

      "be able to execute 'flushCache()' without exceptions after several verifications" in {
        core.flushCache()
      }

      "produce an OverallFailure Message with an empty error list when re- verified after flushing the cache." in {
        val vjh_flushed = core.verify(file, cacheBackend, ast)
        val messages_future_flushed = core.getMessagesFuture(vjh_flushed.id)
        while (!messages_future_flushed.isCompleted) {
          Thread.sleep(100)
        }
        messages_future_flushed.onComplete({
          case Success(msgs) =>
            msgs.last match {
              case ofm: OverallFailureMessage =>
                assert(ofm.result.errors.nonEmpty)
              case _ => fail()
            }
          case Failure(e) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "maximum capacity of verification jobs is exceeded" should {
      val file = sumFile
      val ast_gen = new AstGenerator(file, silent_logger)
      val ast = ast_gen.viper_ast.get

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val handler = core.verify(file, SiliconConfig(List()), ast)
      core.verify(file, noCacheBackend, ast)
      core.verify(file, noCacheBackend, ast)

      "have 'verify()' return a VerificationJobHandler with negative id" in {
        val spillHandler = core.verify(file, noCacheBackend, ast)
        assert(spillHandler.id < 0)
      }

      "until a job handle has been freed" in {
        val result_future = core.getMessagesFuture(handler.id)
        while (!result_future.isCompleted) {
          Thread.sleep(100)
        }
        val newHandler = core.verify(file, noCacheBackend, ast)
        assert(newHandler.id > 0)
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
}