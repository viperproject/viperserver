// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import viper.server.utility.AstGenerator
import viper.server.vsi.VerJobId
import viper.silver.ast.Program
import viper.silver.logger.SilentLogger
import viper.silver.reporter._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps


/**
  * TODO rewrite all tests in terms of [[AsyncCoreServerSpec]]
  *      which offers a the (reactive) specification for ViperCoreServer.
  */
class CoreServerTest extends AnyWordSpec with Matchers with ScalatestRouteTest {

  implicit var actor_system: ActorSystem = ActorSystem("Test")
  private val test_actors = 0 to 2 map ((i: Int) => actor_system.actorOf(ClientActor.props(i)))
  private val expected_results: Array[Option[Boolean]] = Array(Some(true), Some(true), Some(false))

  object ClientActor {
    case object Terminate
    case object ReportOutcome
    def props(test_no: Int): Props = Props(new ClientActor(test_no))
  }

  class ClientActor(private val test_no: Int) extends Actor {

    private var outcome: Option[Boolean] = None

    override def receive: PartialFunction[Any, Unit] = {
      case m: Message =>
        m match {
          case _: OverallSuccessMessage =>
            outcome = Some(true)
          case _: OverallFailureMessage =>
            outcome = Some(false)
          case m =>
        }
      case ClientActor.ReportOutcome =>
        sender() ! outcome
      case ClientActor.Terminate =>
        system.terminate()
    }
  }

  private val silent_logger = SilentLogger()

  private val ast_gen = new AstGenerator(silent_logger.get)

  private val ver_error_file = "src/test/resources/viper/verification_error.vpr"
  private val empty_viper_file = "src/test/resources/viper/empty.vpr"
  private val correct_viper_file = "src/test/resources/viper/sum_method.vpr"

  private val files = List(empty_viper_file, correct_viper_file, ver_error_file)

  private val asts = files.map(ast_gen.generateViperAst(_).get)

  private def getAstByFileName(file: String): Program =
    (files zip asts collect {
      case (f, ast) if f==file => ast
    }).last

  private val noCache_backend = SiliconConfig(List("--disableCaching"))
  private val cache_backend = SiliconConfig(List())

  private val empty_args: Array[String] = Array()

  "An instance of ViperCoreServer" when {
    "verifying a single program with caching disabled" should {
      val core = new ViperCoreServer(empty_args)

      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var jid: VerJobId = null
      "be able to execute 'verify()' without exceptions" in {
        jid = core.verify(ver_error_file, noCache_backend, getAstByFileName(ver_error_file))
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
          Thread.sleep(100)
        }
        messages_future.onComplete({
          case scala.util.Success(_) => succeed
          case scala.util.Failure(e) => fail(e)
        })
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[IllegalStateException] {
          core.verify(ver_error_file, noCache_backend, getAstByFileName(ver_error_file))
        }
      }
    }

    "verifying a single program with caching enabled" should {
      val core = new ViperCoreServer(empty_args)

      "be able to execute 'start()' without exceptions" in {
        core.start()
      }

      var jid: VerJobId = null
      "be able to execute 'verify()' without exceptions" in {
        jid = core.verify(correct_viper_file, cache_backend, getAstByFileName(correct_viper_file))
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
        messages_future.onComplete {
          case scala.util.Success(_) => succeed
          case scala.util.Failure(e) => fail(e)
        }
      }

      "see the future returned by 'getMessagesFuture()' eventually complete unsuccessfully for an inexistent job" in {
        val wrong_jid = VerJobId(42)
        messages_future = ViperCoreServerUtils.getMessagesFuture(core, wrong_jid)
        while (!messages_future.isCompleted) {
          Thread.sleep(100)
        }
        messages_future.onComplete {
          case scala.util.Success(_) => fail()
          case scala.util.Failure(_) => succeed
        }
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }

      "not be able to execute 'verify()' after 'stop()' without exceptions" in {
        assertThrows[IllegalStateException] {
          core.verify(correct_viper_file, noCache_backend, getAstByFileName(correct_viper_file))
        }
      }
    }

    "verifying multiple programs with caching disabled and retrieving results via 'getMessagesFuture()'" should {

      val core = new ViperCoreServer(empty_args)
      core.start()

      var handlers: List[VerJobId] = null
      "be able to have 'verify()' executed repeatedly without exceptions" in {
        handlers = (files zip asts) map { case (f, p) => core.verify(f, noCache_backend, p) }
      }

      "be able to have 'verify()' return JobHandlers with unique non-negative ids" in {
        assert(handlers(0).id == 0)
        assert(handlers(1).id == 1)
        assert(handlers(2).id == 2)
      }

      /** Does not terminate. Rewritten in [[AsyncCoreServerSpec]]. */
//      "be able to have 'getMessagesFuture()' return a future of a sequence of Viper messages containing the expected verification result" in {
//        val messages_futures: List[Future[Seq[Message]]] = handlers.map(h => {
//          ViperCoreServerUtils.getMessagesFuture(core, h)
//        })
//        files.zip(messages_futures).foreach({ case (f, mf) =>
//          while (!mf.isCompleted) {
//            Thread.sleep(100)
//          }
//          mf.onComplete({
//            case scala.util.Success(messages) =>
//              messages.last match {
//                case _: OverallSuccessMessage =>
//                  assert(f != ver_error_file)
//                case _: OverallFailureMessage =>
//                  assert(f == ver_error_file)
//                case _ => fail()
//              }
//            case scala.util.Failure(e) => fail(e)
//          })
//        })
//      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }

    "verifying an incorrect Viper program several times with caching enabled" should {
      val core = new ViperCoreServer(empty_args)
      core.start()

      "produce an OverallFailure message with a non-empty error list upon first verification." in {
        val jid_original = core.verify(ver_error_file, cache_backend, getAstByFileName(ver_error_file))

        val messages_fut = ViperCoreServerUtils.getMessagesFuture(core, jid_original)

        messages_fut.onComplete {
          case scala.util.Success(messages) =>
            messages.last match {
              case ofm: OverallFailureMessage =>
                assert(ofm.result.errors.nonEmpty)
              case _ =>
                fail("last message in stream must be of type OverallFailureMessage")
            }
          case scala.util.Failure(e) =>
            fail(e)
        }

        Await.ready(messages_fut, 5 seconds)
      }

      /** Rewritten in [[AsyncCoreServerSpec]] (but currently fails due to Silver issue#489) */
//      "produce an EntityFailure message with a set cached flag when re-verified." in {
//
//        val jid_cached = files zip asts collect {
//          case (file, ast) if file == ver_error_file =>
//            core.verify(ver_error_file, cache_backend, ast)
//        } last
//
//        val messages_fut = ViperCoreServerUtils.getMessagesFuture(core, jid_cached)
//
//        messages_fut.onComplete {
//          case scala.util.Success(messages) =>
//            val has_cached_msg = messages.exists {
//              case EntityFailureMessage(_, _, _, _, true) => true
//              case _ => false
//            }
//            has_cached_msg should be (true)
//          case scala.util.Failure(e) => fail(e)
//        }
//
//        Await.ready(messages_fut, 3 seconds)
//      }

      "be able to execute 'flushCache()' without exceptions after several verifications" in {
        core.flushCache()
      }

      "produce an EntityFailure message with cleared cached flag and an OverallFailure message with an non-empty error list when reverified after flushing the cache." in {

        val jid_flushed = files zip asts collect {
          case (file, ast) if file == ver_error_file =>
            core.verify(ver_error_file, cache_backend, ast)
        } last

        val messages_fut = ViperCoreServerUtils.getMessagesFuture(core, jid_flushed)

        Await.result(messages_fut, 5 seconds) match {
          case messages: List[Message] =>
            val has_cached_msg = messages.exists {
              case EntityFailureMessage(_, _, _, _, true) =>
                true
              case _ =>
                false
            }
            val has_overall_failure = messages.last match {
              case _: OverallFailureMessage => true
              case _ => false
            }
            has_cached_msg should be (false)
            has_overall_failure should be (true)
        }
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }

    "maximum capacity of verification jobs is exceeded" should {
      val core = new ViperCoreServer(empty_args)
      core.start()

      val (test_file, test_ast) = files zip asts collect  {
        case (file, ast) if file == ver_error_file => (file, ast)
      } last

      val jid = 1 to core.config.maximumActiveJobs() map {
        _ => core.verify(test_file, noCache_backend, test_ast)
      } last

      "have 'verify()' return a VerificationJobHandler with negative id" in {
        val spillHandler = core.verify(test_file, noCache_backend, test_ast)
        assert(spillHandler.id < 0)
      }

      "have 'verify()' return a non-negative id upon freeing up a verification request by calling 'getMessagesFuture()'" in {
        val result_fut = ViperCoreServerUtils.getMessagesFuture(core, jid)
        Await.ready(result_fut, 5 seconds)

        val newHandler = core.verify(test_file, noCache_backend, test_ast)
        assert(newHandler.id > 0)
      }

      "be able to execute 'stop()' without exceptions" in {
        core.stop()
      }
    }
  }
}