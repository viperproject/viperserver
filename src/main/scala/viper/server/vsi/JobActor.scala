// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import java.util.concurrent.{Executor, FutureTask}

/** Owns the FutureTask of a single AST construction or verification job and
  * exposes synchronous start/cancel operations. Replaces the prior actor-based
  * lifecycle wrapper.
  */
final class JobExecution[T](task: FutureTask[T]) {
  def start(executor: Executor): Unit = executor.execute(task)

  /** Returns true iff this call actually interrupted a still-running task. */
  def cancel(): Boolean = task.cancel(true)

  def isDone: Boolean = task.isDone
}
