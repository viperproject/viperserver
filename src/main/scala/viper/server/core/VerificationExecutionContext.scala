package viper.server.core

import java.util.concurrent.Future

import akka.actor.ActorSystem

import scala.concurrent.ExecutionContext

trait VerificationExecutionContext extends ExecutionContext {
  def actorSystem: ActorSystem
  def submit(r: Runnable): Future[_]
}
