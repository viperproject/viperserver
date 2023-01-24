// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import viper.server.vsi.MessageStreamingTask
import viper.silver.reporter.{Entity, EntityFailureMessage, EntitySuccessMessage, Message, Reporter, Time, VerificationResultMessage}
import viper.silver.verifier.{Success, VerificationResult}

trait MessageReportingTask[T] extends MessageStreamingTask[T] with ViperPost {

  def executor: VerificationExecutionContext
  def logger: Logger

  protected def enqueueMessage(msg: Message): Unit = {
    super.enqueueMessage(pack(msg), logger)
  }

  protected def registerTaskEnd(success: Boolean): Unit = {
    super.registerTaskEnd(success, logger)
  }

  /** implementations of this functions should let plugins map verification results for each entity */
  def mapEntityVerificationResult(entity: Entity, result: VerificationResult): VerificationResult

  private def processEntityResultMessage(verifier: String, entity: Entity,
                                         verificationTime: Time, result: VerificationResult, cached: Boolean): Unit = {
    val mappedResult = mapEntityVerificationResult(entity, result)
    enqueueMessage(VerificationResultMessage(verifier, entity, verificationTime, mappedResult, cached))
  }

  // Implementation of the Reporter interface used by the backend.
  class ActorReporter(tag: String) extends Reporter {
    val name = s"ViperServer_$tag"

    def report(msg: Message): Unit = {
      logger.trace(s"ActorReport received msg $msg")
      msg match {
        case m: EntityFailureMessage if m.concerning.info.isCached =>
        case m: EntitySuccessMessage if m.concerning.info.isCached =>
          // Do not re-send messages about AST nodes that have been cached;
          // the information about these nodes is going to be reported anyway.

        // to properly support streaming of (partial) verification results, we must invoke
        // the plugins to do the post-processing and enqueue the resulting message:
        case EntitySuccessMessage(verifier, entity, verificationTime, cached) =>
          processEntityResultMessage(verifier, entity, verificationTime, Success, cached)
        case EntityFailureMessage(verifier, entity, verificationTime, failure, cached) =>
          processEntityResultMessage(verifier, entity, verificationTime, failure, cached)

        case m =>
          enqueueMessage(m)
      }
    }
  }
}
