// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.server.core

import java.nio.file.Paths
import akka.actor.{Actor, Props, Status}
import akka.pattern.ask
import akka.util.Timeout
import org.eclipse.lsp4j.{MessageActionItem, MessageParams, Position, PublishDiagnosticsParams, ShowMessageRequestParams}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Outcome, Succeeded}
import org.scalatest.wordspec.AnyWordSpec
import viper.server.ViperConfig
import viper.server.frontends.lsp.{ClientCoordinator, FileManager, GetIdentifierResponse, GetViperFileEndingsResponse, HintMessage, IdeLanguageClient, LogParams, StateChangeParams, UnhandledViperServerMessageTypeParams, VerificationNotStartedParams, ViperServerService}
import viper.server.utility.AstGenerator
import viper.server.vsi.{JobNotFoundException, VerJobId}
import viper.silver.ast
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn, Program}
import viper.silver.logger.SilentLogger
import viper.silver.reporter.{EntityFailureMessage, EntitySuccessMessage, Message, OverallFailureMessage, OverallSuccessMessage}
import viper.silver.verifier.{AbstractError, VerificationResult, Failure => VerifierFailure, Success => VerifierSuccess}

import java.util.concurrent.CompletableFuture
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
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
  private val execution_context_terminate_timeout_ms = 1000 // 1 sec

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
    verifyAstSiliconWithCaching(server, vprFile, getAstByFileName(vprFile))
  }
  def verifyAstSiliconWithCaching(server: ViperCoreServer, programId: String, ast: Program): VerJobId = {
    val silicon_with_caching: SiliconConfig = SiliconConfig(List())
    server.verify(programId, silicon_with_caching, ast)
  }
  def verifyCarbonWithCaching(server: ViperCoreServer, vprFile: String): VerJobId = {
    val carbon_without_caching: CarbonConfig = CarbonConfig(List())
    server.verify(vprFile, carbon_without_caching, getAstByFileName(vprFile))
  }

  var currentTestName: Option[String] = None
  override def withFixture(test: NoArgTest): Outcome = {
    currentTestName = Some(test.name)
    val res = super.withFixture(test)
    currentTestName = None
    res
  }

  implicit val viperCoreServerFactory: (ViperConfig, VerificationExecutionContext) => ViperCoreServer =
    (config, context) => new ViperCoreServer(config)(context)
  val viperServerServiceFactory: (ViperConfig, VerificationExecutionContext) => ViperServerService =
    (config, context) => new ViperServerService(config)(context)

  /** loan-fixture taking care of starting and stopping a core server */
  def withServer[S <: ViperCoreServer](testCode: (S, VerificationExecutionContext) => Future[Assertion],
                                       afterStop: (S, VerificationExecutionContext) => Future[Assertion] = (_: S, _: VerificationExecutionContext) => Future.successful(Succeeded))
                                      (implicit serverFactory: (ViperConfig, VerificationExecutionContext) => S): Assertion = {
    // create a new execution context for each ViperCoreServer instance which keeps the tests independent since
    val verificationContext = new DefaultVerificationExecutionContext()
    // note that using a single log file per unit test seems to work, however there is an overlap that not only the
    // output of the current but also the next test case ends up in the following file:
    val logFile = Paths.get("logs", s"viperserver_journal_${System.currentTimeMillis()}.log").toFile
    logFile.getParentFile.mkdirs
    logFile.createNewFile()
    val config = new ViperConfig(Seq("--logLevel", "TRACE", "--logFile", logFile.getAbsolutePath))
    val testName = currentTestName match {
      case Some(name) => name
      case None => throw new Exception("no test name")
    }
    val core = serverFactory(config, verificationContext)
    val started = core.start().map({ _ =>
      core.globalLogger.debug(s"server started for test case '$testName'")
    })(verificationContext)

    // execute testCode
    val testFuture = started.flatMap({_ => testCode(core, verificationContext)})(verificationContext)
    // if successful, try to stop core server
    // if unsuccessful, stop core server but return error of testFuture
    val testWithShutdownFuture = testFuture.transformWith(testRes => {
      core.globalLogger.debug("stopping server")
      val coreStopFuture = core.stop()
      testRes match {
        case Success(_) => coreStopFuture
        case f: Failure[Assertion] => coreStopFuture.transform(_ => f)(verificationContext)
      }
    })(verificationContext)
    // run afterStop if testWithShutdownFuture was successful:
    val finalFut = testWithShutdownFuture
      .flatMap(_ => {
        core.globalLogger.debug(s"server has been stopped")
        afterStop(core, verificationContext)
      })(verificationContext)
      .transform(res => {
        core.globalLogger.debug(s"test case '$testName' is done")
        res
      })(verificationContext)
    val res = Await.result(finalFut, Duration.Inf)
    val startTime = System.currentTimeMillis()
    // terminate context with a larger timeout such that we can distinguish a timeout from terminate taking quite long
    verificationContext.terminate(10 * execution_context_terminate_timeout_ms)
    val terminateDurationMs = System.currentTimeMillis() - startTime
    core.globalLogger.debug(s"terminating VerificationExecutionContext took ${terminateDurationMs}ms")
    res match {
      case Succeeded => {
        // check whether timeout has been exceeded and fail test accordingly:
        assert(terminateDurationMs < execution_context_terminate_timeout_ms)
      }
      case _ => res // forward failed assertion
    }
  }

  /* vvvvvvvvvvvvvvvvvvvvvvv */

  "ViperCoreServer" should {

    s"be able to verify a single program with caching disabled" in withServer[ViperCoreServer]({ (core, context) =>
      val jid = verifySiliconWithoutCaching(core, ver_error_file)
      assert(jid != null)
      assert(jid.id >= 0)
      val messages_future = ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map { _ => Succeeded }(context)
      assert(messages_future != null)
      messages_future
    }, (core, _) => {
      // verify after calling `stop()` should fail:
      Future.successful(assertThrows[IllegalStateException] {
        verifySiliconWithoutCaching(core, ver_error_file)
      })
    })

    s"be able to verify a single program with caching enabled" in withServer[ViperCoreServer]({ (core, context) =>
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

    s"be able to verify a single program with weird program ID" in withServer[ViperCoreServer]({ (core, context) =>
      // this test case checks that Silicon does not try to interpret programID and then fails because of an
      // unexpected `:` character.
      // note that this used to fail only on Windows.
      val programID = """_programID_d:\a\gobra-ide\gobra-ide\gobra-ide\client\src\test\data\failing_post.go"""
      val silicon_without_caching: SiliconConfig = SiliconConfig(List("--disableCaching"))
      val jid = core.verify(programID, silicon_without_caching, getAstByFileName(ver_error_file))
      assert(jid != null)
      assert(jid.id >= 0)
      ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map {
        messages: List[Message] =>
          val ofms = messages collect {
            case ofm: OverallFailureMessage => ofm
          }
          assert(ofms.length === 1)
      }(context)
    })

    s"not be able to call `getMessagesFuture` with an non-existent JobId" in withServer[ViperCoreServer]({ (core, context) =>
      val wrong_jid = VerJobId(42)
      ViperCoreServerUtils.getMessagesFuture(core, wrong_jid)(context).failed.transform({
        case JobNotFoundException => Succeeded
        case ex => throw new TestFailedException(s"unexpected exception occurred ($ex)", 0)
      }, ex => throw new TestFailedException(s"expected an exception but none occurred ($ex)", 0))(context)
    })

    s"be able to eventually produce an OverallFailureMessage @$ver_error_file and retrieve the cached results upon requesting to verify the same AST" in withServer[ViperCoreServer]({ (core, context) =>
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
          // no error should be cached:
          assert(ofms.head.result.errors.forall(!_.cached))
      }(context)
      // verify same file again and check whether result comes from cache:
      firstVerification.flatMap (_ => {
        val jid2 = verifySiliconWithCaching(core, ver_error_file)
        ViperCoreServerUtils.getMessagesFuture(core, jid2)(context).map {
          messages: List[Message] =>
            val ofms = messages collect {
              case ofm: OverallFailureMessage => ofm
            }
            val efms = messages collect {
              case efm: EntityFailureMessage => efm
            }
            assert(efms.length === 1 && efms.last.cached)
            assert(ofms.length === 1)
            // list of errors should not be empty:
            assert(ofms.head.result.errors.nonEmpty)
            // errors should be cached:
            assert(ofms.head.result.errors.forall(_.cached))
        }(context)
      })(context)
    })

    s"report the same file location if the error is cached as when it's first verified - Issue #23" in withServer[ViperCoreServer]({ (core, context) =>
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

    s"be able to call flushCache and get an EntityFailure message with cleared cached flag" in withServer[ViperCoreServer]({ (core, context) =>
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

    s"keep caches for different backends separate - Silver Issue #550" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val siliconJid = verifySiliconWithoutCaching(core, ver_error_file)
      val siliconVerification = ViperCoreServerUtils.getResultsFuture(core, siliconJid).map {
        case VerifierFailure(Seq(err)) => assert(!err.cached, "first verification should not be cached")
        case res => fail(s"expected a single entity failure from Silicon but got $res")
      }
      siliconVerification
        .map(_ => verifyCarbonWithCaching(core, ver_error_file))
        .flatMap(carbonJid => ViperCoreServerUtils.getResultsFuture(core, carbonJid))
        .map {
          case VerifierFailure(Seq(err)) => assert(!err.cached, "verification should not reuse cache from other backend")
          case res => fail(s"expected a single entity failure from Carbon but got $res")
        }
    })

    s"run getMessagesFuture() to get Seq[Message] containing the expected verification result" in withServer[ViperCoreServer]({ (core, context) =>
      val jid = verifySiliconWithoutCaching(core, ver_error_file)
      ViperCoreServerUtils.getMessagesFuture(core, jid)(context).map { msgs =>
        msgs.last match {
          case _: OverallFailureMessage => Succeeded
          case m => fail(s"expected failure message but got $m")
        }
      }(context)
    })

    s"be able to verify multiple programs with caching disabled and retrieve results" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val jobIds = files.map(file => (file, verifySiliconWithoutCaching(core, file)))
      val filesAndMessages = jobIds map { case (f, id) => (f, ViperCoreServerUtils.getMessagesFuture(core, id)) }
      val resultFutures = filesAndMessages map { case (f, fut) => fut.map(msgs => {
        core.globalLogger.debug(s"messages for $f: ${msgs.mkString(",")}")
        msgs.last match {
          case _: OverallSuccessMessage => assert(f != ver_error_file)
          case _: OverallFailureMessage => assert(f == ver_error_file)
          case msg => fail(s"unexpected message: $msg")
        }
      })}
      // map resultFuture to a single assertion:
      Future.sequence(resultFutures).map(_ => Succeeded)
    })

    s"be able to verify multiple programs with caching enabled and retrieve results" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val jobIds = files.map(file => (file, verifySiliconWithCaching(core, file)))
      val filesAndMessages = jobIds map { case (f, id) => (f, ViperCoreServerUtils.getMessagesFuture(core, id)(context)) }
      val resultFutures = filesAndMessages map { case (f, fut) => fut.map(msgs => {
        core.globalLogger.debug(s"messages for $f: ${msgs.mkString(",")}")
        msgs.last match {
          case _: OverallSuccessMessage => assert(f != ver_error_file)
          case _: OverallFailureMessage => assert(f == ver_error_file)
          case msg => fail(s"unexpected message: $msg")
        }
      })}
      // map resultFuture to a single assertion:
      Future.sequence(resultFutures).map(_ => Succeeded)
    })

    s"verification errors do not get filtered - Viper-IDE Issue #326" in withServer[ViperServerService]({ (core, context) =>
      // this unit test checks that both errors are correctly collected by the RelayActor
      // The reported errors are equal according to the standard equality on AbstractError even though they occur
      // on different lines.
      implicit val ctx: VerificationExecutionContext = context
      val file = "src/test/resources/viper/issues/00326.vpr"
      val lineNrsOfExpectedVerificationErrors = Seq(11, 18)

      val coordinator = new ClientCoordinator(core)
      coordinator.setClient(new MockClient())
      val fileManager = new FileManager(coordinator, Paths.get(file).toAbsolutePath.toUri.toString)
      val jid = verifyCarbonWithCaching(core, file)
      val relayActorRef = context.actorSystem.actorOf(fileManager.RelayActor.props(fileManager, "carbon"))
      implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
      core.streamMessages(jid, relayActorRef).getOrElse(Future.failed(JobNotFoundException))
        .flatMap(_ => (relayActorRef ? fileManager.RelayActor.GetReportedErrors()).mapTo[Seq[AbstractError]])
        .map(actualErrors => {
          val lineNrsOfActualVerificationErrors = actualErrors
            .map(_.pos match {
              case lc: HasLineColumn => lc.line
              case p => fail(s"error should have positional information but got $p")
            })
            .sorted
          assert(lineNrsOfExpectedVerificationErrors == lineNrsOfActualVerificationErrors)
      })
    })(viperServerServiceFactory)

    class MockClient extends IdeLanguageClient {
      override def requestIdentifier(pos: Position): CompletableFuture[GetIdentifierResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
      override def requestVprFileEndings(): CompletableFuture[GetViperFileEndingsResponse] = CompletableFuture.failedFuture(new UnsupportedOperationException())
      override def notifyLog(param: LogParams): Unit = {}
      override def notifyHint(param: HintMessage): Unit = {}
      override def notifyUnhandledViperServerMessage(params: UnhandledViperServerMessageTypeParams): Unit = {}
      override def notifyVerificationNotStarted(params: VerificationNotStartedParams): Unit = {}
      override def notifyStateChanged(params: StateChangeParams): Unit = {}
      override def telemetryEvent(`object`: Any): Unit = {}
      override def publishDiagnostics(diagnostics: PublishDiagnosticsParams): Unit = {}
      override def showMessage(messageParams: MessageParams): Unit = {}
      override def showMessageRequest(requestParams: ShowMessageRequestParams): CompletableFuture[MessageActionItem] = CompletableFuture.failedFuture(new UnsupportedOperationException())
      override def logMessage(message: MessageParams): Unit = {}
    }

    s"verifyMultipleFiles behalves as expected" in withServer[ViperCoreServer]({ (core, context) =>
      // this unit tests makes sure that verifying two identical files using `verifyMultipleFiles` actually
      // triggers the cache

      val file1 = "src/test/resources/viper/identical-versions/version1.vpr"
      val file2 = "src/test/resources/viper/identical-versions/version2.vpr"

      def handleResult(file: String, res: VerificationResult): Assertion = res match {
        case VerifierFailure(errors) =>
          assert(errors.size == 1)
          assert(errors.head.cached == (file == file2))
        case _ => fail(s"unexpected verification result for file $file")
      }

      verifyMultipleFiles(core, List(file1, file2), handleResult)(context)
    })

    s"adapting an axiom should invalidate the cache" in withServer[ViperCoreServer]({ (core, context) =>
      val fileBeforeModification = "src/test/resources/viper/changed-axiom/version1.vpr"
      val fileAfterModification = "src/test/resources/viper/changed-axiom/version2.vpr"

      def handleResult(file: String, res: VerificationResult): Assertion = res match {
        case VerifierSuccess => assert(file == fileBeforeModification)
        case VerifierFailure(errors) =>
          assert( file == fileAfterModification)
          assert(errors.size == 1)
      }

      verifyMultipleFiles(core, List(fileBeforeModification, fileAfterModification), handleResult)(context)
    })

    /* this is currently not the case (see Silver Issue #548)
    s"adapting a heap-dependent function should invalidate the cache" in withServer({ (core, context) =>
      val fileBeforeModification = "src/test/resources/viper/changed-function/version1.vpr"
      val fileAfterModification = "src/test/resources/viper/changed-function/version2.vpr"

      def handleResult(file: String, res: VerificationResult): Assertion = res match {
        case VerifierSuccess => assert(file == fileBeforeModification)
        case VerifierFailure(errors) =>
          assert( file == fileAfterModification)
          assert(errors.size == 1)
      }

      verifyMultipleFiles(core, List(fileBeforeModification, fileAfterModification), handleResult)(context)
    })
    */

    s"reordering of domains should not invalidate the cache" in withServer[ViperCoreServer]({ (core, context) =>
      val fileBeforeReordering = "src/test/resources/viper/reordered-domains/version1.vpr"
      val fileAfterReordering = "src/test/resources/viper/reordered-domains/version2.vpr"

      def handleResult(file: String, res: VerificationResult): Assertion = (res: @unchecked) match {
        case VerifierFailure(errors) =>
          assert(errors.size == 1)
          val err = errors.head
          if (err.cached) {
            assert(file == fileAfterReordering)
          } else {
            assert(file == fileBeforeReordering)
          }
      }

      verifyMultipleFiles(core, List(fileBeforeReordering, fileAfterReordering), handleResult)(context)
    })

    s"reordering domain components should not invalidate the cache - Issue #52" in withServer[ViperCoreServer]({ (core, context) =>
      val fileBeforeModification = "src/test/resources/viper/reordered-axioms/version1.vpr"
      val fileAfterReorderingDomainFunctions = "src/test/resources/viper/reordered-axioms/version2.vpr"
      val fileAfterReorderingDomainAxioms = "src/test/resources/viper/reordered-axioms/version3.vpr"

      def handleResult(file: String, res: VerificationResult): Assertion = (res: @unchecked) match {
        case VerifierFailure(errors) =>
          assert(errors.size == 1)
          val err = errors.head
          if (err.cached) {
            assert(file == fileAfterReorderingDomainFunctions || file == fileAfterReorderingDomainAxioms)
          } else {
            assert(file == fileBeforeModification)
          }
      }

      verifyMultipleFiles(core, List(fileBeforeModification, fileAfterReorderingDomainFunctions, fileAfterReorderingDomainAxioms), handleResult)(context)
    })

    object ClientActor {
      case object ReportOutcome
      def props(): Props = Props(new ClientActor())
    }

    class ClientActor() extends Actor {

      private var outcome: Option[Boolean] = None
      private val outcomePromise: Promise[Boolean] = Promise()

      override def receive: PartialFunction[Any, Unit] = {
        case m: Message =>
          m match {
            case _: OverallSuccessMessage => outcome = Some(true)
            case _: OverallFailureMessage => outcome = Some(false)
            case _ =>
          }
        case ClientActor.ReportOutcome =>
          sender() ! outcomePromise.future
        case Status.Success =>
          // Success is sent when the stream is completed
          if (outcome.isEmpty) {
            // we should have received an overall message by now
            outcomePromise.failure(new RuntimeException("expected to receive an overall verification message but received none so far"))
          } else {
            outcomePromise.success(outcome.get)
          }
        case Status.Failure(f) => outcomePromise.failure(f)
      }
    }

    s"be able to verify multiple programs with caching disabled and retrieve results via `streamMessages()`" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val test_actors = 0 to 2 map (_ => context.actorSystem.actorOf(ClientActor.props()))
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
      // streamDones futures should eventually be resolved
      val allVerificationsFuture = Future.sequence(streamDones)
      val outcomesFuture = allVerificationsFuture.flatMap(actors => {
        val outcomeFutures = actors.map(actor => {
          implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
          (actor ? ClientActor.ReportOutcome).mapTo[Future[Boolean]].flatten
        })
        Future.sequence(outcomeFutures)
      })
      val assertionsFuture = outcomesFuture.map(_.zip(files) map { case (outcome, file) =>
        assert(outcome == (file != ver_error_file))
      })
      // map assertionsFuture to a single assertion:
      assertionsFuture.map(_ => Succeeded)
    })

    s"be able to start a new verification after maximum capacity was exceeded but earlier verifications have ended" in withServer[ViperCoreServer]({ (core, context) =>
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

    s"be able to successfully verify a program with a refute statement without caching" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val refute_file = "src/test/resources/viper/refute1.vpr"
      val refute_member_name = "foo"
      val jid = verifySiliconWithoutCaching(core, refute_file)
      val messages = ViperCoreServerUtils.getMessagesFuture(core, jid)(context)
      messages.map(checkMessages(expectedSuccess = true, expectedEntityName = refute_member_name, expectedToBeCached = false))
    })

    s"be able to successfully verify a program with a refute statement with caching" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val refute_file = "src/test/resources/viper/refute1.vpr"
      val refute_member_name = "foo"
      val jid1 = verifySiliconWithCaching(core, refute_file)
      val messages1 = ViperCoreServerUtils.getMessagesFuture(core, jid1)(context)

      messages1.map(checkMessages(expectedSuccess = true, expectedEntityName = refute_member_name, expectedToBeCached = false))
        .map(_ => verifySiliconWithCaching(core, refute_file))
        .flatMap(jid2 => ViperCoreServerUtils.getMessagesFuture(core, jid2)(context))
        .map(checkMessages(expectedSuccess = true, expectedEntityName = refute_member_name, expectedToBeCached = true))
    })

    s"be able to verify a program with a failing refute statement without caching" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val refute_file = "src/test/resources/viper/refute2.vpr"
      val refute_member_name = "foo"
      val jid = verifySiliconWithoutCaching(core, refute_file)
      val messages = ViperCoreServerUtils.getMessagesFuture(core, jid)(context)
      messages.map(checkMessages(expectedSuccess = false, expectedEntityName = refute_member_name, expectedToBeCached = false))
    })

    s"be able to successfully verify a program with a failing refute statement with caching" in withServer[ViperCoreServer]({ (core, context) =>
      implicit val ctx: VerificationExecutionContext = context
      val refute_file = "src/test/resources/viper/refute2.vpr"
      val refute_member_name = "foo"
      val jid1 = verifySiliconWithCaching(core, refute_file)
      val messages1 = ViperCoreServerUtils.getMessagesFuture(core, jid1)(context)

      messages1.map(checkMessages(expectedSuccess = false, expectedEntityName = refute_member_name, expectedToBeCached = false))
        .map(_ => verifySiliconWithCaching(core, refute_file))
        .flatMap(jid2 => ViperCoreServerUtils.getMessagesFuture(core, jid2)(context))
        .map(checkMessages(expectedSuccess = false, expectedEntityName = refute_member_name, expectedToBeCached = true))
    })

    s"reordering of methods with refutes should update position information correctly" in withServer[ViperCoreServer]({ (core, context) =>
      val fileBeforeReordering = "src/test/resources/viper/reordered-refutes/version1.vpr"
      val fileAfterReordering = "src/test/resources/viper/reordered-refutes/version2.vpr"

      var errPosBeforeReordering: Option[ast.Position] = None
      var errPosAfterReordering: Option[ast.Position] = None
      def handleResult(file: String, res: VerificationResult): Assertion = (res: @unchecked) match {
        case VerifierFailure(errors) =>
          assert(errors.size == 1)
          val err = errors.head
          if (file == fileBeforeReordering) {
            errPosBeforeReordering = Some(err.pos)
          } else if (file == fileAfterReordering) {
            errPosAfterReordering = Some(err.pos)
          }
          // note that we do not check here whether the `cached` flag is correctly set because this is best effort-only
          // for plugins. In particular for the refute plugin, this flag's value cannot easily be decided and set
          // accordingly.
          succeed
      }

      verifyMultipleFiles(core, List(fileBeforeReordering, fileAfterReordering), handleResult)(context)
        .map(_ => {
          assert(errPosBeforeReordering.isDefined && errPosBeforeReordering.get.isInstanceOf[AbstractSourcePosition])
          assert(errPosAfterReordering.isDefined && errPosAfterReordering.get.isInstanceOf[AbstractSourcePosition])
          val errLineBeforeReordering = errPosBeforeReordering.get.asInstanceOf[AbstractSourcePosition].line
          val errLineAfterReordering = errPosAfterReordering.get.asInstanceOf[AbstractSourcePosition].line
          assert(errLineBeforeReordering > errLineAfterReordering, "we expect that the error occurs earlier after reordering the methods")
        })(context)
    })
  }

  /**
    * verifies multiple files sequentially but uses identical program IDs for both files such that there can be
    * caching behavior
    * @param handleResult function taking file path and verification result as arguments and returning an assertion
    */
  private def verifyMultipleFiles(server: ViperCoreServer,
                          files: List[String],
                          handleResult: (String, VerificationResult) => Assertion)
                         (implicit context: VerificationExecutionContext): Future[Assertion] = {
    val filesAndAsts = files.map(file => (file, getAstByFileName(file)))

    // use a common program ID
    val programId = "some-program-id"
    // iterate over all files & ASTs and verify one after the other:
    filesAndAsts.foldLeft(Future.successful(succeed))((assFuture, fileAndAst) => {
      assFuture
        .map(_ => verifyAstSiliconWithCaching(server, programId, fileAndAst._2))
        .flatMap(jobId => ViperCoreServerUtils.getResultsFuture(server, jobId)(context))
        .map(res => handleResult(fileAndAst._1, res))
    })
  }

  private def checkMessages(expectedSuccess: Boolean, expectedEntityName: String, expectedToBeCached: Boolean)(msgs: List[Message]): Assertion = {
    assert(msgs.exists {
      case _: EntitySuccessMessage => true
      case _: EntityFailureMessage => true
      case _ => false
    }, "expected at least one entity success or failure message")
    assert(msgs.exists {
      case _: OverallSuccessMessage => true
      case _: OverallFailureMessage => true
      case _ => false
    }, "expected at least one overall success or failure message")
    msgs.foreach {
      case m: EntitySuccessMessage =>
        assert(expectedSuccess, s"verification expected to fail but got entity success ${m.result}")
        assert(m.concerning.name == expectedEntityName)
        assert(m.cached == expectedToBeCached)
      case m: EntityFailureMessage =>
        assert(!expectedSuccess, s"verification expected to succeed but got entity failure ${m.result}")
      case m: OverallSuccessMessage =>
        assert(expectedSuccess, s"verification expected to fail but got overall success ${m.result}")
      case m: OverallFailureMessage =>
        assert(!expectedSuccess,s"verification expected to succeed but got overall failure ${m.result}")
      case _ => Succeeded
    }
    Succeeded
  }
}
