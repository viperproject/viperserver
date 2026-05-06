// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory, TimeUnit}
import java.util.{concurrent => java_concurrent}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

trait VerificationExecutionContext extends ExecutionContext {
  def executorService: ExecutorService
  def scheduler: ScheduledExecutorService
  def submit(r: Runnable): java_concurrent.Future[_]
  /** Terminate executor and scheduler. */
  def terminate(timeoutMSec: Long = 1000): Unit
  /** Restart (no-op now that the actor system is gone; retained for API
    * compatibility with prior consumers).
    */
  def restart(): Future[Unit]
}

object DefaultVerificationExecutionContext {
  /** Minimum number of threads needed for the thread pool. */
  val minNumberOfThreads: Int = 3
}

/**
  * Default verification execution context backed by a fixed thread pool.
  *
  * The pool uses as many threads as there are processors available (but at
  * least 2). Each started thread gets a stack of 128MB. A separate
  * single-threaded daemon `ScheduledExecutorService` handles delayed
  * callbacks (used to be Akka schedulers).
  */
class DefaultVerificationExecutionContext(threadNamePrefix: String = "thread",
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

  private lazy val schedulerService: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    private val mCount = new java.util.concurrent.atomic.AtomicInteger(1)
    override def newThread(runnable: Runnable): Thread = {
      val t = new Thread(runnable, s"$threadNamePrefix-scheduler-${mCount.getAndIncrement()}")
      t.setDaemon(true)
      t
    }
  })
  override def scheduler: ScheduledExecutorService = schedulerService

  private lazy val context: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(executorService)

  override def execute(runnable: Runnable): Unit = context.execute(runnable)

  override def reportFailure(cause: Throwable): Unit = context.reportFailure(cause)

  override def submit(r: Runnable): java_concurrent.Future[_] = context.submit(r)

  @throws(classOf[InterruptedException])
  override def terminate(timeoutMSec: Long = 1000): Unit = {
    schedulerService.shutdownNow()
    executorService.shutdown()
    executorService.awaitTermination(timeoutMSec, TimeUnit.MILLISECONDS)
  }

  override def restart(): Future[Unit] = Future.unit
}
