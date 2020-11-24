package viper.server.core

import viper.server.vsi.MessageStreamingTask
import viper.silver.ast.Program
import viper.silver.reporter.{Message, Reporter}

trait MessageReportingTask extends MessageStreamingTask[Program] with ViperPost {

//  private val _reporter:

  protected def enqueueMessage(msg: Message): Unit = {
    super.enqueueMessage(pack(msg))
  }

  // Implementation of the Reporter interface used by the backend.
  class ActorReporter(tag: String) extends Reporter {
    val name = s"ViperServer_$tag"

    def report(msg: Message): Unit = {
      println(s">>> ActorReporter.report($msg)")
      enqueueMessage(msg)
    }
  }

}
