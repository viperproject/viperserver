// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.actor.{Actor, Props, Status}
import akka.pattern.ask
import akka.util.Timeout
import ch.qos.logback.classic.Logger
import viper.server.vsi.{JobNotFoundException, VerJobId}
import viper.silver.reporter.{Message, OverallFailureMessage, OverallSuccessMessage}
import viper.silver.verifier.{VerificationResult, Failure => VerificationFailure, Success => VerificationSuccess}

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

object ViperCoreServerUtils {

  private object SeqActor {
    case object Result
    def props(jid: VerJobId, logger: Logger): Props = Props(new SeqActor(jid, logger))
  }

  class SeqActor(jid: VerJobId, logger: Logger) extends Actor {

    var messages: List[Message] = List()
    private val msgPromise: Promise[List[Message]] = Promise()

    override def receive: PartialFunction[Any, Unit] = {
      case m: Message =>
        logger.trace(s"SeqActor(JID ${jid.id}) received message $m")
        messages = messages :+ m
      case SeqActor.Result =>
        sender() ! msgPromise.future // return a future that will be completed as soon as all messages have been received
      case Status.Success =>
        // Success is sent when the stream is completed
        logger.trace(s"SeqActor(JID ${jid.id}) has successfully completed receiving messages")
        msgPromise.success(messages)
      case Status.Failure(cause) =>
        logger.trace(s"SeqActor(JID ${jid.id}) has failed receiving all messages: $cause")
        msgPromise.failure(cause)
    }
  }

  /** Get a Future containing all messages generated by the backend.
    *
    * This is a utility function and not part of ViperCoreServer. Therefore, an instance of ViperCoreServer as well as
    * an instance of an actor system must be provided.
    *
    * Deletes the jobHandle on completion.
    */
  def getMessagesFuture(core: ViperCoreServer, jid: VerJobId)(implicit executor: VerificationExecutionContext): Future[List[Message]] = {
    import scala.language.postfixOps

    val actor = executor.actorSystem.actorOf(SeqActor.props(jid, core.globalLogger))
    val complete_future = core.streamMessages(jid, actor, include_ast = true).getOrElse(Future.failed(JobNotFoundException))
    complete_future.flatMap(_ => {
      implicit val askTimeout: Timeout = Timeout(core.config.actorCommunicationTimeout() milliseconds)
      (actor ? SeqActor.Result).mapTo[Future[List[Message]]].flatten
    })
  }

  /** Get a Future containing only verification results.
    *
    * This is a utility function and not part of ViperCoreServer. Therefore, an instance of ViperCoreServer as well as
    * an instance of an actor system must be provided.
    *
    * Deletes the jobHandle on completion.
    */
  def getResultsFuture(core: ViperCoreServer, jid: VerJobId)(implicit executor: VerificationExecutionContext): Future[VerificationResult] = {
    val messages_future = getMessagesFuture(core, jid)
    val result_future: Future[VerificationResult] = messages_future.map(msgs => {
      // note that errors cannot be extracted from entity failure messages because some backends (e.g. Carbon) do not
      // produce entity messages but only an overall message at the very end
      val overallResults = msgs.collect {
        case _: OverallSuccessMessage => VerificationSuccess
        case m: OverallFailureMessage => VerificationFailure(m.result.errors)
      }
      assert(overallResults.length == 1, s"every verification should result in exactly one overall success or failure message but got $msgs")
      overallResults.head
    })
    result_future
  }
}
