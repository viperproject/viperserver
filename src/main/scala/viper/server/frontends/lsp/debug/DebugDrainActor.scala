// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug

import akka.actor.{Actor, PoisonPill, Props, Status}
import viper.silver.reporter._

/**
  * Consumes the message stream of a debug run.
  *
  * A debug run must not be relayed to the file's usual `RelayActor`: it would overwrite the user's
  * diagnostics, reset the LSP containers of the file and emit spurious state-change notifications. At the same
  * time the stream has to be consumed by somebody, since it is backpressured and its completion is what
  * triggers the cleanup of the job. This actor therefore drains everything and only forwards coarse progress
  * information.
  */
class DebugDrainActor(onProgress: String => Unit) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case StatisticsReport(nOfMethods, nOfFunctions, nOfPredicates, _, _) =>
      onProgress(s"Verifying $nOfMethods method(s), $nOfFunctions function(s) and $nOfPredicates predicate(s)...")
    case EntitySuccessMessage(_, entity, _, _) =>
      onProgress(s"Verified ${entity.name}")
    case EntityFailureMessage(_, entity, _, _, _) =>
      onProgress(s"Found an error in ${entity.name}")
    case _: Message =>
    case Status.Success =>
      self ! PoisonPill
    case Status.Failure(_) =>
      self ! PoisonPill
    case _ =>
  }
}

object DebugDrainActor {
  def props(onProgress: String => Unit): Props = Props(new DebugDrainActor(onProgress))
}
