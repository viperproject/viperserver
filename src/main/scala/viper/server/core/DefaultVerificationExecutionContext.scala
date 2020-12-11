package viper.server.core

import java.util.concurrent.{ExecutorService, Executors, Future, ThreadFactory}

import akka.actor.{ActorSystem, BootstrapSetup}
import akka.actor.setup.ActorSystemSetup

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object DefaultVerificationExecutionContext {
  // at least 2 threads are needed if the actor system is also using the same thread pool (as opposed to creating a
  // actor system with the default executor like `ActorSystem(actorSystemName)`)
  val minNumberOfThreads: Int = 2
}

class DefaultVerificationExecutionContext(actorSystemName: String = "Actor_System", threadNamePrefix: String = "thread") extends VerificationExecutionContext {
  protected lazy val nThreads: Int = Math.max(DefaultVerificationExecutionContext.minNumberOfThreads, Runtime.getRuntime.availableProcessors())
  protected lazy val threadStackSize: Long = 128L * 1024L * 1024L // 128M seems to consistently be recommended by Silicon and Carbon
  protected lazy val executorService: ExecutorService = Executors.newFixedThreadPool(
    nThreads, new ThreadFactory() {

      import java.util.concurrent.atomic.AtomicInteger

      private val mCount = new AtomicInteger(1)
      override def newThread(runnable: Runnable): Thread = {
        val threadName = s"$threadNamePrefix-${mCount.getAndIncrement()}"
        new Thread(null, runnable, threadName, threadStackSize)
      }
    })

  private lazy val context: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(executorService)

  override def execute(runnable: Runnable): Unit = context.execute(runnable)

  override def reportFailure(cause: Throwable): Unit = context.reportFailure(cause)

  override lazy val actorSystem: ActorSystem = ActorSystem(actorSystemName,
    ActorSystemSetup(BootstrapSetup(None, None, Some(context))))

  override def submit(r: Runnable): Future[_] = context.submit(r)
}
