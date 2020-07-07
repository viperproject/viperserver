package viper.server.vsi

import akka.stream.scaladsl.SourceQueueWithComplete
import org.reactivestreams.Publisher

object TaskProtocol {
  case object ClientRequest
  case class ServerReport(msg: Envelope)
  case class FinalServerReport(success: Boolean)
}

object VerificationProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(task: Thread, queue: SourceQueueWithComplete[Envelope], publisher: Publisher[Envelope])

  // VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  // Verification interrupt request to Main Actor
  case class Stop(call_me_back: Boolean)
}