// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.actor.{Actor, Props}
import akka.http.scaladsl.Http
import viper.server.core.VerificationExecutionContext

import scala.concurrent.Future

// --- Actor: Terminator ---


object Terminator {
  case object Exit
  case class WatchJobQueue(jid: JobId, handle: JobHandle)

  def props[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
               ver_jobs: JobPool[VerJobId, VerHandle],
               bindingFuture: Option[Future[Http.ServerBinding]] = None)
            (implicit ctx: VerificationExecutionContext): Props
  = Props(new Terminator(ast_jobs, ver_jobs, bindingFuture)(ctx))
}

class Terminator[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
                    ver_jobs: JobPool[VerJobId, VerHandle],
                    bindingFuture: Option[Future[Http.ServerBinding]])
                (implicit val ctx: VerificationExecutionContext) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case Terminator.Exit =>
      bindingFuture match {
        case Some(future) =>
          future
            .flatMap(_.unbind()) // trigger unbinding from the port
        case None =>
      }
      // note that the execution context is NOT terminated such that clients
      // have better control over when the execution context should be terminated
  }
}