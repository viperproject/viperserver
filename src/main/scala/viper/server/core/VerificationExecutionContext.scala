// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory, TimeUnit}
import java.util.{concurrent => java_concurrent}

import akka.actor.ActorSystem

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}

trait VerificationExecutionContext extends ExecutionContext {
  def executorService: ExecutorService
  def actorSystem: ActorSystem
  def submit(r: Runnable): java_concurrent.Future[_]
  /** terminate executor and actor system */
  def terminate(timeoutMSec: Long = 1000): Unit
  /** restart actor system */
  def restart(): Future[Unit]
}

object DefaultVerificationExecutionContext {
  // at least 3 threads seem to be needed if the actor system is also using the same thread pool. Otherwise, if the actor
  // system is started using its own (default) executor, a different number of thread might be sufficient.
  // actor system with the default executor like `ActorSystem(actorSystemName)`)
  /** minimum number of threads needed for the thread pool in the DefaultVerificationExecutionContext */
  val minNumberOfThreads: Int = 3
}

/**
  * This class provides a default verification execution context based on a fixed thread pool. The actor system is
  * not using that thread pool but is using akka's default executor. (There have been issues starting the (default)
  * akka logger in the unit tests because of some blocking behavior of the thread pool when sharing the same thread
  * pool.)
  * The thread pool uses as many threads as there are processors available (but at least 2). Each started thread
  * gets a stack of 128MB.
  * The purpose of a verification execution context is that it can be passed to ViperServer and ViperServer will
  * use the provided verification execution context whenever an actor or any task requiring a separate thread is
  * executed.
  */
class DefaultVerificationExecutionContext(actorSystemName: String = "Actor_System",
                                          threadNamePrefix: String = "thread",
                                          threadPoolSize: Option[Int] = None) extends VerificationExecutionContext {
  protected lazy val nThreads: Int = threadPoolSize.getOrElse(
    Math.max(DefaultVerificationExecutionContext.minNumberOfThreads, Runtime.getRuntime.availableProcessors()))
  protected lazy val threadStackSize: Long = 128L * 1024L * 1024L // 128M seems to consistently be recommended by Silicon and Carbon
  private lazy val service: ExecutorService = Executors.newFixedThreadPool(
    nThreads, new ThreadFactory {

      import java.util.concurrent.atomic.AtomicInteger

      private val mCount = new AtomicInteger(1)
      override def newThread(runnable: Runnable): Thread = {
        val threadName = s"$threadNamePrefix-${mCount.getAndIncrement()}"
        new Thread(null, runnable, threadName, threadStackSize)
      }
    })
  override def executorService: ExecutorService = service

  private lazy val context: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(executorService)

  override def execute(runnable: Runnable): Unit = context.execute(runnable)

  override def reportFailure(cause: Throwable): Unit = context.reportFailure(cause)

  private var system: Option[ActorSystem] = Some(ActorSystem(actorSystemName))
  override def actorSystem: ActorSystem = system.getOrElse(throw new IllegalStateException(s"actor system has been terminated"))

  override def submit(r: Runnable): java_concurrent.Future[_] = context.submit(r)

  @throws(classOf[InterruptedException])
  override def terminate(timeoutMSec: Long = 1000): Unit = {
    val oldSystem = actorSystem
    system = None
    Await.ready(oldSystem.terminate(), FiniteDuration(timeoutMSec, TimeUnit.MILLISECONDS))
    executorService.shutdown()
    executorService.awaitTermination(timeoutMSec, TimeUnit.MILLISECONDS)
  }

  override def restart(): Future[Unit] = {
    // the executor service stays untouched, i.e. only the actor system is restarted
    val oldSystem = actorSystem
    system = None
    oldSystem.terminate().map(_ => {
      // set new actor system:
      system = Some(ActorSystem(actorSystemName))
    })(this)
  }
}
