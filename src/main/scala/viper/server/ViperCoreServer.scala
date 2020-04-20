package viper.server

import org.reactivestreams.Publisher

import scala.language.postfixOps
import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import akka.{Done}
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{ActorRef, ActorSystem, Actor, Props, PoisonPill}
import akka.stream.scaladsl.{SourceQueueWithComplete, Source, Sink, Keep}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.http.scaladsl.server.Route

import akka.http.scaladsl.Http

import viper.silver.reporter.{Message, Reporter}
import viper.silver.ast
import viper.silver.logger.ViperLogger
import viper.server.ViperServerProtocol._


/*
 # Some Dummy Configurations which will be passed to the backend which
 # will later be exchanged by the real backendconfigurations which are
 # used for silicon and carbon. (Don't think they are actually exactly the same. But can further determine
 # when I have a look at SilFrontend and if there is a possibility to set a config on a SilFrontend with this.)
 #
 # Currently the custom backend config is not supported here. (Add afterwards)
 #
 # I think what is meant here with partialCommandline is the CommandLine Input which would be given to the
 # Verifiers (Silicon / Carbon) but without specifying the FileName because the File already got parsed, ...
 # and transformed into the Program which is then passed.
 # Need to figure out a way to pass the Program directly into the Verifier without having it do the whole parsing, ...
 # steps again.
 */ 
trait BackendConfig {
  val partialCommandLine: List[String]
}
case class SiliconConfig(partialCommandLine: List[String]) extends BackendConfig
case class CarbonConfig(partialCommandLine: List[String]) extends BackendConfig


// We can potentially have more than one verification task at the same time.
// A verification task is distinguished via the corresponding ActorRef,
//  as well as its unique job_id.
case class JobHandle(controller_actor: ActorRef,
                     queue: SourceQueueWithComplete[Message],
                     publisher: Publisher[Message])


class ViperCoreServer(private var _config: ViperConfig) {

  final def config: ViperConfig = _config

  private var _logger: ViperLogger = _
  final def logger: ViperLogger = _logger

  implicit val system: ActorSystem = ActorSystem("Main")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  private var _job_handles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _next_job_id: Int = 0
  val MAX_ACTIVE_JOBS: Int = 3

  private def newJobsAllowed = _job_handles.size < MAX_ACTIVE_JOBS

  private def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _next_job_id
    _job_handles(new_jid) = job_executor(new_jid)
    _next_job_id = _next_job_id + 1
    (new_jid, _job_handles(new_jid))
  }

  private def discardJob(jid: Int): mutable.Map[Int, Future[JobHandle]] = {
    _job_handles -= jid
  }

  /** If the Option is resolved to None, the job does not exist.
    * If the Option is resolved to Some(_),
    *   a) The Future is not yet completed ==> verification in progress.
    *   b) The Future is already completed ==> job done.
    */
  protected def lookupJob(jid: Int): Option[ Future[JobHandle] ] = {
    _job_handles.get(jid)
  }


  // --- Actor: Terminator ---

  protected var _term_actor: ActorRef = _

  object Terminator {
    case object Exit
    case class WatchJob(jid: Int, handle: JobHandle)

    def props(bindingFuture: Future[Http.ServerBinding]): Props = Props(new Terminator(Some(bindingFuture)))
    def props(): Props = Props(new Terminator(None))
  }

  class Terminator(bindingFuture: Option[Future[Http.ServerBinding]]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case Terminator.Exit =>
        bindingFuture match {
          case Some(future) =>
            future
            .flatMap(_.unbind()) // trigger unbinidng from the port
            .onComplete(_ => system.terminate()) // and shutdown when done

          case None =>
            system.terminate() // shutdown
        }
      case Terminator.WatchJob(jid, handle) =>
        val queue_completion_future: Future[Done] = handle.queue.watchCompletion()
        queue_completion_future.onComplete( {
          case Failure(e) =>
            println(s"Terminator detected failure in job #$jid: $e")
            throw e
          case Success(_) =>
            discardJob(jid)
            println(s"Terminator deleted job #$jid")
        })
    }
  }

  // --- Actor: MainActor ---

  // (See model description in ViperServerProtocol.scala)

  object MainActor {
    def props(id: Int, logger: ViperLogger): Props = Props(new MainActor(id, logger))
  }

  class MainActor(private val id: Int, private val logger: ViperLogger) extends Actor {

    private var _verificationTask: Thread = _

    // blocking
    private def interrupt: Boolean = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
        println(s"Job #$id has been successfully interrupted.")
        return true
      }
      false
    }

    private def resetVerificationTask(): Unit = {
      if (_verificationTask != null && _verificationTask.isAlive) {
        _verificationTask.interrupt()
        _verificationTask.join()
      }
      _verificationTask = null
    }

    override def receive: PartialFunction[Any, Unit] = {

      case Stop(call_back_needed) =>
        val did_I_interrupt = interrupt
        if (call_back_needed) {
          // If a callback is expected, then the caller must decide when to kill the actor.
          if (did_I_interrupt) {
            sender ! s"Job #$id has been successfully interrupted."
          } else {
            sender ! s"Job #$id has already been finalized."
          }
        }

      case Verify(args, reporter, program) =>
        resetVerificationTask()
        sender ! verify(args, reporter, program)


      case msg =>
        throw new Exception("Main Actor: unexpected message received: " + msg)
    }

    // TODO: look if it is necessary to pass the my_reporter in the case of reporter getting passed.
    private def verify(config: List[String], reporter: Option[Reporter], program: Option[ast.Program]): JobHandle = {

      // The maximum number of messages in the reporter's message buffer is 10000.
      val (queue, publisher) = Source.queue[Message](10000, OverflowStrategy.backpressure).toMat(Sink.asPublisher(false))(Keep.both).run()

      val my_reporter = system.actorOf(ReporterActor.props(id, queue), s"reporter_$id")

      _verificationTask = new Thread(new VerificationWorker(my_reporter, logger.get, config, program, reporter))
      _verificationTask.start()

      println(s"Starting job #$id...")

      JobHandle(self, queue, publisher)
    }
  }

  // --- Actor: ReporterActor ---

  object ReporterActor {
    def props(jid: Int, queue: SourceQueueWithComplete[Message]): Props = Props(new ReporterActor(jid, queue))
  }

  class ReporterActor(jid: Int, queue: SourceQueueWithComplete[Message]) extends Actor {

    override def receive: PartialFunction[Any, Unit] = {
      case ReporterProtocol.ClientRequest =>
      case ReporterProtocol.ServerReport(msg) =>
        //println(msg)
        queue.offer(msg)
      case ReporterProtocol.FinalServerReport(success) =>
        queue.complete()
        if ( success )
          println(s"Job #$jid has been completed successfully.")
        else
          println(s"Job #$jid has been completed ERRONEOUSLY.")
        self ! PoisonPill
      case _ =>
    }
  }


  def start(): Unit = {
    init(None)
  }

/*
  def start(routes: ViperLogger => Route): Unit = {
    init(Some(routes))
  }
*/

  protected def init(routes: Option[ViperLogger => Route]): Unit = {
    config.verify()

    _logger = ViperLogger("ViperServerLogger", config.getLogFileWithGuarantee, config.logLevel())
    println(s"Writing [level:${config.logLevel()}] logs into ${if (!config.logFile.isSupplied) "(default) " else ""}journal: ${logger.file.get}")

    ViperCache.initialize(logger.get, config.backendSpecificCache())

    routes match {
      case Some(routes) => {
        val port = config.port()
        val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(routes(logger), "localhost", port)

        _term_actor = system.actorOf(Terminator.props(bindingFuture), "terminator")

        println(s"ViperServer online at http://localhost:$port")
      }
      case None => {
        _term_actor = system.actorOf(Terminator.props(), "terminator")

        println(s"ViperServer online in CoreServer mode")
      }
    }
  }

  /*
   # Note: I think the program which is given here can be verified directly with either cached verification
   # or the normal one. The program given as input here is I think the same one as in line 249 of VerificationWorker.scala.
   #
   # However the verifier frontend ... has to be created here. The only parts dropping out are parsing, semanticAnalysis, translation, consistencyCheck.
   # (Changes in VerificationWorker have to be done for this.)
   #
   # Perhaps make two methods in the reporteractor. One of them accepts still the args: List[String] and the other one accepts program: ast.Program
   # and make both of the methods create a VerificationWorker with two different constructors.
   # One other major difference would be the need to accept such a BackendConfig in ViperBackend which then gets used to instanciate Silver or Carbon
   # or whatever Verifier is used. (Can do this with maybe Option[BackendConfig] which then gets split into the cases where only the args are used
   # and the case where the BackendConfig is used instead of the args) -> I think we can just use the verify method from Verifier (which is for example extended
   # by Silicon) which takes a Program.
   # Also need to slightly change the doCachedVerification method when we want to do it in this way. (needs to accept a Program when it is given in this way.)
   # Note: Verifier is a field of SilFrontend and can thus just be extracted for the verification -> similar way to implement execute method in ViperBackend
   # for both cases. (It's the output of createVerifier which is a method in SilFrontend)
   #
   # For the moment the partialCommandLine in BackendConfig should be just the normal command line (for example: "silicon DUMMY_FILENAME.sil")
   */
  def verify(args: List[String], reporter: Reporter, program: ast.Program): Option[Future[JobHandle]] = {
    val (_, jobHandle) = createJobHandle(args, Some(reporter), Some(program))
    jobHandle
  }

  def verify(args: List[String]): Option[Future[JobHandle]] = {
    val (_, jobHandle) = createJobHandle(args)
    jobHandle
  }

  protected def createJobHandle(args: List[String]): (Option[Int], Option[Future[JobHandle]]) = {
    createJobHandle(args, None, None)
  }

  private def createJobHandle(args: List[String], reporter: Option[Reporter], program: Option[ast.Program]): (Option[Int], Option[Future[JobHandle]]) = {
    if (newJobsAllowed) {
      val (id, jobHandle) = bookNewJob((new_jid: Int) => {
        implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
        val main_actor = system.actorOf(MainActor.props(new_jid, logger), s"main_actor_$new_jid")
        val new_job_handle: Future[JobHandle] = (main_actor ? ViperServerProtocol.Verify(args, reporter, program)).mapTo[JobHandle]
        new_job_handle
      })
      (Some(id), Some(jobHandle))
    } else {
      println(s"the maximum number of active verification jobs are currently running ($MAX_ACTIVE_JOBS).")
      (None, None)
    }
  }

  def stop(): Unit = {
    println(s"Stopping ViperCoreServer")

    getInterruptFutureList() onComplete {
      case Success(_) =>
        _term_actor ! Terminator.Exit
        println(s"shutting down...")
      case Failure(err_msg) =>
        println(s"Interrupting one of the verification threads timed out: $err_msg")
        _term_actor ! Terminator.Exit
        println(s"forcibly shutting down...")
    }
  }


  protected def getInterruptFutureList(): Future[List[String]] = {
    val interrupt_future_list: List[Future[String]] = _job_handles map { case (jid, handle_future) =>
      handle_future.flatMap {
        case JobHandle(actor, _, _) =>
          implicit val askTimeout: Timeout = Timeout(5000 milliseconds)
          (actor ? Stop(true)).mapTo[String]
      }
    } toList
    val overall_interrupt_future: Future[List[String]] = Future.sequence(interrupt_future_list)
    overall_interrupt_future
  }
    


}




// CODE FROM THE IDE SYNC MEETING ###########################################################################################
/*
case class VerificationJobHandler(id: Int) extends Serializable //Note: serializable is extended automatically in case classes (think so)

case class ViperServerConfig(/** unspecified for now, Silas, just use the default options for now */)

//Note: this should probably be a class and not a trait.
trait ViperCoreServer {

  def start(config: ViperServerConfig): Unit // or in the constructor
  def stop(): Unit

  /*
   # Not quite sure but I think in the HTTP case the reporter always has to be one of these ActorReporters.
   */
  def verify(config: BackendConfig, reporter: Reporter, program: ast.Program): VerificationJobHandler

// Silas, up until here and the parts below if necessary

//  def flushCash(jobId: Int): Unit // maybe required for testing
//  def getFuture(jobId: Int): Future[VerificationResult]
//  def discardJob(jobId: Int): Boolean

//  def isCompleted(jobId: Int): Boolean // Maybe remove
//  def hasFailed(jobId: Int): Boolean // Maybe remove
  // ... some more cash methods
}

//Note: at the moment this backendConfig should just be the default in the cases (so Config for Silicon, ...)
trait BackendConfig

case class SiliconConfig(partialCommandLine: Seq[String]) extends BackendConfig

case class CarbonConfig(partialCommandLine: Seq[String]) extends BackendConfig

trait CustomConfig extends BackendConfig {
  def createVerifier(partialCommandLine: Seq[String]): Verifier
}
*/
//##############################################################################################################################