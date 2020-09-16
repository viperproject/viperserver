import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.core.ViperBackendConfigs.SiliconConfig
import viper.server.core.{ViperCoreServer, ViperCoreServerUtils}
import viper.server.vsi._
import viper.server.utility.AstGenerator
import viper.server.ViperConfig
import viper.silver.ast.Program
import viper.silver.logger.SilentLogger
import viper.silver.reporter._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class CoreServerTest extends WordSpec with Matchers with ScalatestRouteTest {
  import scala.language.postfixOps

  implicit var actor_system: ActorSystem = ActorSystem("Test")
  val test_actor_0 = actor_system.actorOf(ClientActor.props(0))
  val test_actor_1 = actor_system.actorOf(ClientActor.props(1))
  val test_actor_2 = actor_system.actorOf(ClientActor.props(2))
  val actor_tests_results: Array[Option[Boolean]] = Array(None, None, None)

  object ClientActor {
    case object Terminate
    def props(test_no: Int): Props = Props(new ClientActor(test_no))
  }

  class ClientActor(private val test_no: Int) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case m: Message =>
        m match {
          case _: OverallSuccessMessage =>
            actor_tests_results(test_no) = Some(true)
          case _: OverallFailureMessage =>
            actor_tests_results(test_no) = Some(false)
          case _ =>
        }
      case ClientActor.Terminate =>
        println("terminating external actor system")
        system.terminate()
    }
  }

  private val silent_logger = SilentLogger()

  private val ast_gen = new AstGenerator(silent_logger)
  private val empty_file = "src/test/resources/viper/empty.vpr"
  private val sum_file = "src/test/resources/viper/sum_method.vpr"
  private val verificationError_file = "src/test/resources/viper/verification_error.vpr"

  private val empty_ast = ast_gen.generateViperAst(empty_file).get
  private val sum_ast = ast_gen.generateViperAst(sum_file).get
  private val verificationError_ast = ast_gen.generateViperAst(verificationError_file).get

  private val noCache_backend = SiliconConfig(List("--disableCaching"))
  private val cache_backend = SiliconConfig(List())

  "An instance of ViperCoreServer" when {
    "verifying a single file with caching disabled" should {
      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)

      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var jid: JobID = null
      "be able to execute 'verify()' without exceptions" in {
        jid = core.verify(verificationError_file, noCache_backend, verificationError_ast)
        assert(jid != null)
      }

      "be able to have 'verify()' return a JobHandler with non-negative id" in {
        assert(jid.id >= 0)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages." in {
        messages_future = ViperCoreServerUtils.getMessagesFuture(core, jid)
        assert(messages_future != null)
      }

      "eventually see the future returned from 'getMessagesFuture()' completed successfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(12100)
        }
        messages_future.onComplete({
          case Success(_) => succeed
          case Failure(_) => fail()
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[IllegalStateException] {
          core.verify(verificationError_file, noCache_backend, verificationError_ast)
        }
      }
    }
    "verifying a single file with caching enabled" should {
      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var jid: JobID = null
      "be able to execute 'verify()' without exceptions" in {
        jid = core.verify(sum_file, cache_backend, sum_ast)
        assert(jid != null)
      }

      "be able to have 'verify()' return a JobHandler with  non-negative id" in {
        assert(jid.id >= 0)
      }

      var messages_future: Future[Seq[Message]] = null
      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages" in {
        messages_future = ViperCoreServerUtils.getMessagesFuture(core, jid)
        assert(messages_future != null)
      }

      "see the future returned from 'getMessagesFuture()' eventually completed successfully" in {
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case Success(verRes) => succeed
          case Failure(_) => fail()
        })
      }

      "see the future returned by 'getMessagesFuture()' eventually complete unsuccessfully for an inexistent job" in {
        val wrong_jid = JobID(42)
        messages_future = ViperCoreServerUtils.getMessagesFuture(core, wrong_jid)
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case Success(_) => fail()
          case Failure(_) => succeed
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[IllegalStateException] {
          core.verify(sum_file, noCache_backend, sum_ast)
        }
      }
    }
    "verifying multiple files with caching disabled and retrieving results via 'getMessagesFuture()'" should {
      val files: List[String] = List(empty_file, sum_file, verificationError_file)
      val programs: List[Program] = List(empty_ast, sum_ast, verificationError_ast)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val filesAndProgs: List[(String, Program)] = files.zip(programs)
      var handlers: List[JobID] = null
      "be able to have 'verify()' executed repeatedly without exceptions" in {
        handlers = filesAndProgs map { case (f, p) => core.verify(f, noCache_backend, p) }
      }

      "be able to have 'verify()' return JobHandlers with unique non-negative ids" in {
        assert(handlers(0).id == 0)
        assert(handlers(1).id == 1)
        assert(handlers(2).id == 2)
      }

      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages containing the expected verification result" in {
        val messages_futures: List[Future[Seq[Message]]] = handlers.map(h => {
          ViperCoreServerUtils.getMessagesFuture(core, h)
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
                  assert(f != verificationError_file)
                case m: OverallFailureMessage =>
                  assert(f == verificationError_file)
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
      val files: List[String] = List(empty_file, sum_file, verificationError_file)
      val programs: List[Program] = List(empty_ast, sum_ast, verificationError_ast)

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val filesAndProgs: List[(String, Program)] = files.zip(programs)
      var handlers: List[JobID] = null
      "be able to have 'verify()' executed repeatedly without exceptions" in {
        handlers = filesAndProgs map { case (f, p) => core.verify(f, noCache_backend, p) }
      }

      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages containing the expected verification result" in {
        val messages_futures: List[Future[Seq[Message]]] = handlers.map(h => {
          ViperCoreServerUtils.getMessagesFuture(core, h)
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
                  assert(f != verificationError_file)
                case m: OverallFailureMessage =>
                  assert(f == verificationError_file)
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
      val file1 = empty_file
      val file2 = sum_file
      val file3 = verificationError_file

      val ast1 = empty_ast
      val ast2 = sum_ast
      val ast3 = verificationError_ast

      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val jid1 = core.verify(file1, noCache_backend, ast1)
      val jid2 = core.verify(file2, noCache_backend, ast2)
      val jid3 = core.verify(file3, noCache_backend, ast3)

      "be able to have 'streamMessages()' stream a sequence of Viper messages without errors" in {
        core.streamMessages(jid1, test_actor_0)
        core.streamMessages(jid2, test_actor_1)
        core.streamMessages(jid3, test_actor_2)
      }

      "have the stream of messages contain the expected verification result" in {
        Thread.sleep(20000)
        assert(actor_tests_results(0) == Some(true))
        assert(actor_tests_results(1) == Some(true))
        assert(actor_tests_results(2) == Some(false))
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
    "verifying an incorrect viper program several times with caching enabled" should {
      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      "produce an OverallFailure Message with a non-empty error list upon first verification." in {
        val jid_original = core.verify(verificationError_file, cache_backend, verificationError_ast)
        val messages_future_original = ViperCoreServerUtils.getMessagesFuture(core, jid_original)
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
        val jid_cached = core.verify(verificationError_file, cache_backend, verificationError_ast)
        val messages_future_cached = ViperCoreServerUtils.getMessagesFuture(core, jid_cached)
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
        val jid_flushed = core.verify(verificationError_file, cache_backend, verificationError_ast)
        val messages_future_flushed = ViperCoreServerUtils.getMessagesFuture(core, jid_flushed)
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
      val config = new ViperConfig(List())
      config.verify()
      val core = new ViperCoreServer(config)
      core.start()

      val jid = core.verify(sum_file, noCache_backend, sum_ast)
      core.verify(sum_file, noCache_backend, sum_ast)
      core.verify(sum_file, noCache_backend, sum_ast)

      "have 'verify()' return a VerificationJobHandler with negative id" in {
        val spillHandler = core.verify(sum_file, noCache_backend, sum_ast)
        assert(spillHandler.id < 0)
      }

      "have 'verify()' return a non-negative id upon freeing up a verification request by calling 'getMessagesFuture()'" in {
        val result_future = ViperCoreServerUtils.getMessagesFuture(core, jid)
        while (!result_future.isCompleted) {
          Thread.sleep(100)
        }
        val newHandler = core.verify(sum_file, noCache_backend, sum_ast)
        assert(newHandler.id > 0)
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
}