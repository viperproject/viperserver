// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.server.core

import java.nio.file.Paths

import akka.actor.{Actor, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Outcome, Succeeded}
import org.scalatest.wordspec.AnyWordSpec
import viper.server.utility.AstGenerator
import viper.server.vsi.{JobNotFoundException, VerJobId}
import viper.silver.ast.{HasLineColumn, Program}
import viper.silver.logger.SilentLogger
import viper.silver.reporter.{EntityFailureMessage, Message, OverallFailureMessage, OverallSuccessMessage}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.language.postfixOps

/**
  * Although the withServer fixture deals with futures, AnyWordSpec as been purposely chosen over AsyncFlatSpec:
  * AsyncFlatSpec exposes the execution context used by the test cases. This is undesirable for server unit tests
  * because (1) we need a VerificationExecutionContext instead of a plain ExecutionContext and (2) each test case should
  * create and terminate its own execution context such that these interactions get tested as well. Overriding the
  * AsyncFlatSpec's execution context is hard as we want a separate one per unit test. Furthermore, terminating the
  * execution context at the end of a unit test breaks the functionality of AsyncFlatSpec since it depends on the
  * execution context to process the assertion in the returned future.
  * Having two execution context, (1) AsyncFlatSpec's execution context for all test cases and (2) a
  * VerificationExecutionContext per unit test, is dangerous: One has to make sure to always choose the right one,
  * depening on the use case (dealing with futures while ViperServer runs vs. after ViperServer has stopped and
  * VerificationExecutionContext has been terminated). Making things worse, AsyncFlatSpec's execution context is
  * implicit meaning that it can easily be used by accident.
  *
  * Therefore, CoreServerSpec uses AnyWordSpec and the withServer fixture handles futures by awaiting them instead of
  * offloading this work to a scalatest class.
  */
class CoreServerSpec extends AnyWordSpec with Matchers {
  private val empty_viper_file = "src/test/resources/viper/empty.vpr"
  private val correct_viper_file = "src/test/resources/viper/sum_method.vpr"
  private val ver_error_file = "src/test/resources/viper/verification_error.vpr"

  private val files = List(empty_viper_file, correct_viper_file, ver_error_file)

  private val ast_gen = new AstGenerator(SilentLogger().get)

  // lazy collection of ASTs that have been parsed so far
  private var asts: Map[String, Program] = Map.empty
  private def getAstByFileName(file: String): Program = {
    def genAst(f: String): Program = {
      val prog = ast_gen.generateViperAst(f).get
      asts += f -> prog
      prog
    }
    asts.getOrElse(file, genAst(file))
  }

  def verifySiliconWithoutCaching(server: ViperCoreServer, vprFile: String): VerJobId = {
    val silicon_without_caching: SiliconConfig = SiliconConfig(List("--disableCaching"))
    server.verify(vprFile, silicon_without_caching, getAstByFileName(vprFile))
  }
  def verifySiliconWithCaching(server: ViperCoreServer, vprFile: String): VerJobId = {
    val silicon_with_caching: SiliconConfig = SiliconConfig(List())
    server.verify(vprFile, silicon_with_caching, getAstByFileName(vprFile))
  }

  var currentTestName: Option[String] = None
  override def withFixture(test: NoArgTest): Outcome = {
    currentTestName = Some(test.name)
    val res = super.withFixture(test)
    currentTestName = None
    res
  }

  /** loan-fixture taking care of starting and stopping a core server */
  def withServer(testCode: (ViperCoreServer, VerificationExecutionContext) => Future[Assertion],
                 afterStop: (ViperCoreServer, VerificationExecutionContext) => Future[Assertion] = (_, _) => Future.successful(Succeeded)): Assertion = {
    // create a new execution context for each ViperCoreServer instance which keeps the tests independent since
    val verificationContext = new DefaultVerificationExecutionContext()
    val logFile = Paths.get("logs", s"viperserver_journal_${System.currentTimeMillis()}.log").toFile
    logFile.getParentFile.mkdirs
    logFile.createNewFile()
    val server_args: Array[String] = Array("--logLevel", "TRACE", "--logFile", logFile.getAbsolutePath)
    val core = new ViperCoreServer(server_args)(verificationContext)
    core.start()

    val testName = currentTestName match {
      case Some(name) =>
        core.logger.get.debug(s"server started for test case '$name'")
        name
      case None => throw new Exception("no test name")
    }

    // execute testCode
    val testFuture = testCode(core, verificationContext)
    // if successful, try to stop core server
    // if unsuccessful, stop core server but return error of testFuture
    val testWithShutdownFuture = testFuture.transformWith(testRes => {
      core.logger.get.debug("stopping server")
      val coreStopFuture = core.stop()
      testRes match {
        case Success(_) => coreStopFuture
        case f: Failure[Assertion] => coreStopFuture.transform(_ => f)(verificationContext)
      }
    })(verificationContext)
    // run afterStop if testWithShutdownFuture was successful:
    val finalFut = testWithShutdownFuture
      .flatMap(_ => {
        core.logger.get.debug(s"server has been stopped")
        afterStop(core, verificationContext)
      })(verificationContext)
      .transform(res => {
        core.logger.get.debug(s"test case '$testName' is done")
        res
      })(verificationContext)
    val res = Await.result(finalFut, Duration.Inf)
    val startTime = System.currentTimeMillis()
    verificationContext.terminate(60 * 1000) // 1min - rather high such that a potential timeout becomes clearly visible
    val terminateDuration = System.currentTimeMillis() - startTime
    core.logger.get.debug(s"terminating VerificationExecutionContext took ${terminateDuration}ms")
    res
  }

  /* vvvvvvvvvvvvvvvvvvvvvvv */

  "ViperCoreServer" should {

    s"be able to verify a single program with caching disabled" in withServer({ (core, context) =>
      val jid = verifySiliconWithoutCaching(core, ver_error_file)
      assert(jid != null)
      assert(jid.id >= 0)
      val messages_future = ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map { _ => Succeeded }(context)
      Future.successful(assert(messages_future != null))
    }, (core, _) => {
      // verify after calling `stop()` should fail:
      Future.successful(assertThrows[IllegalStateException] {
        verifySiliconWithoutCaching(core, ver_error_file)
      })
    })

    s"be able to verify a single program with caching enabled" in withServer({ (core, context) =>
      val jid = verifySiliconWithCaching(core, correct_viper_file)
      assert(jid != null)
      assert(jid.id >= 0)
      val messages_future = ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map { _ => Succeeded }(context)
      assert(messages_future != null)
      messages_future
    }, (core, _) => {
      // verify after calling `stop()` should fail:
      Future.successful(assertThrows[IllegalStateException] {
        verifySiliconWithCaching(core, correct_viper_file)
      })
    })

    s"not be able to call `getMessagesFuture` with an non-existent JobId" in withServer({ (core, context) =>
      val wrong_jid = VerJobId(42)
      ViperCoreServerUtils.getMessagesFuture(core, wrong_jid)(context).failed.transform({
        case JobNotFoundException => Succeeded
        case ex => throw new TestFailedException(s"unexpected exception occurred ($ex)", 0)
      }, ex => throw new TestFailedException(s"expected an exception but none occurred ($ex)", 0))(context)
    })

    s"be able to eventually produce an OverallFailureMessage @$ver_error_file and retrieve the cached results upon requesting to verify the same AST" in withServer({ (core, context) =>
      val jid1 = verifySiliconWithCaching(core, ver_error_file)
      val firstVerification = ViperCoreServerUtils.getMessagesFuture(core, jid1)(context).map {
        messages: List[Message] =>
          val ofms = messages collect {
            case ofm: OverallFailureMessage => ofm
          }
          val efms = messages collect {
            case efm: EntityFailureMessage => efm
          }
          // first verification thus cached flag should not be set:
          assert(efms.length === 1 && !efms.last.cached)
          assert(ofms.length === 1)
          // list of errors should not be empty:
          assert(ofms.head.result.errors.nonEmpty)
      }(context)
      // verify same file again and check whether result comes from cache:
      firstVerification.flatMap (_ => {
        val jid2 = verifySiliconWithCaching(core, ver_error_file)
        ViperCoreServerUtils.getMessagesFuture(core, jid2)(context).map {
          messages: List[Message] =>
            val efms: List[EntityFailureMessage] = messages collect {
              case efm: EntityFailureMessage => efm
            }
            assert(efms.length === 1 && efms.last.cached)
        }(context)
      })(context)
    })

    s"report the same file location if the error is cached as when it's first verified - Issue #23" in withServer({ (core, context) =>
      val file = "src/test/resources/viper/issues/00023.vpr"
      val lineNrOfExpectedVerificationError = 9
      val jid1 = verifySiliconWithCaching(core, file)
      val firstVerification = ViperCoreServerUtils.getMessagesFuture(core, jid1)(context).map {
        messages: List[Message] =>
          val ofms = messages collect {
            case ofm: OverallFailureMessage => ofm
          }
          val efms = messages collect {
            case efm: EntityFailureMessage => efm
          }
          // first verification thus cached flag should not be set:
          assert(efms.length === 1 && !efms.last.cached)
          assert(efms.head.result.errors.length === 1)
          val lineNr = efms.head.result.errors.head.pos match {
            case lc: HasLineColumn => lc.line
            case _ => fail("error should have positional information")
          }
          assert(lineNr == lineNrOfExpectedVerificationError)
          assert(ofms.length === 1)
          // list of errors should not be empty:
          assert(ofms.head.result.errors.nonEmpty)
      }(context)
      // verify same file again and check whether result comes from cache and the same line is reported:
      firstVerification.flatMap (_ => {
        val jid2 = verifySiliconWithCaching(core, file)
        ViperCoreServerUtils.getMessagesFuture(core, jid2)(context).map {
          messages: List[Message] =>
            val efms: List[EntityFailureMessage] = messages collect {
              case efm: EntityFailureMessage => efm
            }
            assert(efms.length === 1 && efms.last.cached)
            assert(efms.head.result.errors.length === 1)
            val lineNr = efms.head.result.errors.head.pos match {
              case lc: HasLineColumn => lc.line
              case _ => fail("error should have positional information")
            }
            assert(lineNr == lineNrOfExpectedVerificationError)
        }(context)
      })(context)
    })

    s"be able to call flushCache and get an EntityFailure message with cleared cached flag" in withServer({ (core, context) =>
      val jid1 = verifySiliconWithCaching(core, ver_error_file)
      val firstVerification = ViperCoreServerUtils.getMessagesFuture(core, jid1)(context).map {
        messages: List[Message] =>
          val ofms = messages collect {
            case ofm: OverallFailureMessage => ofm
          }
          val efms = messages collect {
            case efm: EntityFailureMessage => efm
          }
          // first verification thus cached flag should not be set:
          assert(efms.length === 1 && !efms.last.cached)
          assert(ofms.length === 1)
          // list of errors should not be empty:
          assert(ofms.head.result.errors.nonEmpty)
      }(context)
      // flush cache
      val cacheFlushFuture = firstVerification.map(_ => core.flushCache())(context)
      // verify same file again and check that result does not come from cache:
      cacheFlushFuture.flatMap (_ => {
        val jid2 = verifySiliconWithCaching(core, ver_error_file)
        ViperCoreServerUtils.getMessagesFuture(core, jid2)(context).map {
          messages: List[Message] =>
            val ofms = messages collect {
              case ofm: OverallFailureMessage => ofm
            }
            val efms: List[EntityFailureMessage] = messages collect {
              case efm: EntityFailureMessage => efm
            }
            assert(ofms.length == 1)
            assert(efms.length === 1 && !efms.last.cached)
        }(context)
      })(context)
    })

    s"run getMessagesFuture() to get Seq[Message] containing the expected verification result" in withServer({ (core, context) =>
      val jid = verifySiliconWithoutCaching(core, ver_error_file)
      ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map { msgs =>
        msgs.last match {
          case _: OverallFailureMessage => Succeeded
          case m => fail(s"expected failure message but got $m")
        }
      }(context)
    })

    s"be able to verify multiple programs with caching disabled and retrieve results" in withServer({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val jobIds = files.map(file => (file, verifySiliconWithoutCaching(core, file)))
      val filesAndMessages = jobIds map { case (f, id) => (f, ViperCoreServerUtils.getMessagesFuture(core, id)) }
      val resultFutures = filesAndMessages map { case (f, fut) => fut.map(msgs => {
        core.logger.get.debug(s"messages for $f: ${msgs.mkString(",")}")
        msgs.last match {
          case _: OverallSuccessMessage => assert(f != ver_error_file)
          case _: OverallFailureMessage => assert(f == ver_error_file)
          case msg => fail(s"unexpected message: $msg")
        }
      })}
      // map resultFuture to a single assertion:
      Future.sequence(resultFutures).map(_ => Succeeded)
    })

    s"be able to verify multiple programs with caching enabled and retrieve results" in withServer({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val jobIds = files.map(file => (file, verifySiliconWithCaching(core, file)))
      val filesAndMessages = jobIds map { case (f, id) => (f, ViperCoreServerUtils.getMessagesFuture(core, id)(context)) }
      val resultFutures = filesAndMessages map { case (f, fut) => fut.map(msgs => {
        core.logger.get.debug(s"messages for $f: ${msgs.mkString(",")}")
        msgs.last match {
          case _: OverallSuccessMessage => assert(f != ver_error_file)
          case _: OverallFailureMessage => assert(f == ver_error_file)
          case msg => fail(s"unexpected message: $msg")
        }
      })}
      // map resultFuture to a single assertion:
      Future.sequence(resultFutures).map(_ => Succeeded)
    })

    object ClientActor {
      case object Terminate
      case object ReportOutcome
      def props(test_no: Int, executionContext: VerificationExecutionContext): Props = Props(new ClientActor(test_no)(executionContext))
    }

    class ClientActor(private val test_no: Int)(executionContext: VerificationExecutionContext) extends Actor {

      private var outcome: Option[Boolean] = None

      override def receive: PartialFunction[Any, Unit] = {
        case m: Message =>
          m match {
            case _: OverallSuccessMessage =>
              outcome = Some(true)
            case _: OverallFailureMessage =>
              outcome = Some(false)
            case _ =>
          }
        case ClientActor.ReportOutcome =>
          sender() ! outcome
        case ClientActor.Terminate =>
          executionContext.actorSystem.terminate()
        case Success =>
        // Success is sent when the stream is completed
      }
    }

    s"be able to verify multiple programs with caching disabled and retrieve results via `streamMessages()`" in withServer({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val test_actors = 0 to 2 map ((i: Int) => context.actorSystem.actorOf(ClientActor.props(i, context)))
      val jids = files.map(file => verifySiliconWithoutCaching(core, file))
      // stream messages to actors
      val jidsWithActors = jids zip test_actors
      val streamOptionsWithActors = jidsWithActors map { case (jid, actor) => (core.streamMessages(jid, actor), actor) }
      // stream options should be defined
      val streamDones = streamOptionsWithActors map { case (streamOption, actor) =>
        assert(streamOption.isDefined)
        val streamDoneFuture = streamOption.get
        // as soon as stream completes, complete it with the actor
        streamDoneFuture.map(_ => actor)
      }
      // streamState futures should eventually be resolved
      val allVerificationsFuture = Future.sequence(streamDones)
      val outcomesFuture = allVerificationsFuture.flatMap(actors => {
        val outcomeFutures = actors.map(actor => {
          implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
          (actor ? ClientActor.ReportOutcome).mapTo[Option[Boolean]]
        })
        Future.sequence(outcomeFutures)
      })
      val assertionsFuture = outcomesFuture.map(_.zip(files) map { case (outcome, file) =>
        assert(outcome.contains(file != ver_error_file))
      })
      // map assertionsFuture to a single assertion:
      assertionsFuture.map(_ => Succeeded)
    })

    s"be able to start a new verification after maximum capacity was exceeded but earlier verifications have ended" in withServer({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      // start 3 jobs:
      val jids = files.map(file => verifySiliconWithoutCaching(core, file))
      // IDs should all be positive:
      assert(jids.forall(jid => jid.id >= 0))
      // 4th job will fail since capacity is exceeded (ID will be negative)
      val spillJid = verifySiliconWithoutCaching(core, correct_viper_file)
      assert(spillJid.id < 0)
      // wait for complete of first verification job and try again:
      val firstJobCompleted = ViperCoreServerUtils.getMessagesFuture(core, jids.head)(context)
      firstJobCompleted.flatMap(_ => {
        val newJid = verifySiliconWithoutCaching(core, correct_viper_file)
        assert(newJid.id >= 0)
        ViperCoreServerUtils.getMessagesFuture(core, newJid)(context)
      }.map(msgs => {
        msgs.last match {
          case _: OverallSuccessMessage => succeed
          case _: OverallFailureMessage => fail(s"unexpected failure")
          case msg => fail(s"unexpected message: $msg")
        }
      })).flatMap(_ => {
        // wait for completion of remaining two verification:
        val otherFutures = jids.tail.map(jid => ViperCoreServerUtils.getMessagesFuture(core, jid)(context))
        Future.sequence(otherFutures).map(_ => Succeeded)
      })
    })
  }
}
