// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import viper.server.vsi.MessageStreamingTask
import viper.silver.ast.Program
import viper.silver.reporter.{EntityFailureMessage, EntitySuccessMessage, Message, Reporter}

trait MessageReportingTask extends MessageStreamingTask[Program] with ViperPost {

  protected def enqueueMessage(msg: Message): Unit = {
    super.enqueueMessage(pack(msg))
  }

  // Implementation of the Reporter interface used by the backend.
  class ActorReporter(tag: String) extends Reporter {
    val name = s"ViperServer_$tag"

    def report(msg: Message): Unit = {
      //TODO use logger
      //println(s">>> ActorReporter.report($msg)")
      msg match {
        case m: EntityFailureMessage if m.concerning.info.isCached =>
        case m: EntitySuccessMessage if m.concerning.info.isCached =>
          // Do not re-send messages about AST nodes that have been cached;
          // the information about these nodes is going to be reported anyway.

        case m =>
          enqueueMessage(m)
      }
    }
  }

}
