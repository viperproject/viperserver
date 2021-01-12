// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import java.util.concurrent.{ExecutorService, Executors, Future, ThreadFactory}

import akka.actor.ActorSystem

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

trait VerificationExecutionContext extends ExecutionContext {
  def actorSystem: ActorSystem
  def submit(r: Runnable): Future[_]
}

object DefaultVerificationExecutionContext {
  // at least 2 threads are needed if the actor system is also using the same thread pool. Otherwise, if the actor
  // sytem is started using its own (default) executor, a single thread is sufficient.
  // actor system with the default executor like `ActorSystem(actorSystemName)`)
  /** minimum number of threads needed for the thread pool in the DefaultVerificationExecutionContext */
  val minNumberOfThreads: Int = 1
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
class DefaultVerificationExecutionContext(actorSystemName: String = "Actor_System", threadNamePrefix: String = "thread") extends VerificationExecutionContext {
  protected lazy val nThreads: Int = Math.max(DefaultVerificationExecutionContext.minNumberOfThreads, Runtime.getRuntime.availableProcessors())
  protected lazy val threadStackSize: Long = 128L * 1024L * 1024L // 128M seems to consistently be recommended by Silicon and Carbon
  protected lazy val executorService: ExecutorService = Executors.newFixedThreadPool(
    nThreads, new ThreadFactory {

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

  override lazy val actorSystem: ActorSystem = ActorSystem(actorSystemName)

  override def submit(r: Runnable): Future[_] = context.submit(r)
}
