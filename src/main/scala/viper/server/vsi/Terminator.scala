// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}

// --- Actor: Terminator ---


object Terminator {
  case object Exit
  case class WatchJobQueue(jid: JobId, handle: JobHandle)

  def props[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
               ver_jobs: JobPool[VerJobId, VerHandle],
               bindingFuture: Option[Future[Http.ServerBinding]] = None)
            (implicit ctx: ExecutionContext, sys: ActorSystem): Props
  = Props(new Terminator(ast_jobs, ver_jobs, bindingFuture)(ctx, sys))
}

class Terminator[R](ast_jobs: JobPool[AstJobId, AstHandle[R]],
                    ver_jobs: JobPool[VerJobId, VerHandle],
                    bindingFuture: Option[Future[Http.ServerBinding]])
                (implicit val ctx: ExecutionContext,
                 implicit val sys: ActorSystem) extends Actor {

  override def receive: PartialFunction[Any, Unit] = {
    case Terminator.Exit =>
      bindingFuture match {
        case Some(future) =>
          future
            .flatMap(_.unbind()) // trigger unbinding from the port
            .onComplete(_ => {
              sys.terminate() // and shutdown when done
            })
        case None =>
          sys.terminate() // shutdown
      }
  }
}