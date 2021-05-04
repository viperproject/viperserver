// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.actor.{Actor, PoisonPill, Props}
import akka.stream.scaladsl.SourceQueueWithComplete

// --- Actor: MessageActor ---

object QueueActor {
  def props(queue: SourceQueueWithComplete[Envelope]): Props = Props(new QueueActor(queue))
}

class QueueActor(queue: SourceQueueWithComplete[Envelope]) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case TaskProtocol.BackendReport(msg) =>
      val offer_status = queue.offer(msg)
      sender() ! offer_status
    case TaskProtocol.FinalBackendReport(_) =>
      println(s"QueueActor: FinalBackendReport received")
      queue.complete()
      self ! PoisonPill
    case _ =>
  }
}
