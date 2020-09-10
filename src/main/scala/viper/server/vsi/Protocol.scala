package viper.server.vsi

import akka.stream.scaladsl.SourceQueueWithComplete
import org.reactivestreams.Publisher
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import viper.server.vsi.Requests.{jsonFormat1, jsonFormat2}

object TaskProtocol {
  case object ClientRequest
  case class ServerReport(msg: Letter)
  case class FinalServerReport(success: Boolean)
}

object VerificationProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(task: Thread, queue: SourceQueueWithComplete[Letter], publisher: Publisher[Letter])

  // VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  // Verification interrupt request to Main Actor
  case class Stop(call_me_back: Boolean)
}

object Requests extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  case class VerificationRequest(arg: String)
  implicit val VerificationRequest_format: RootJsonFormat[VerificationRequest] = jsonFormat1(VerificationRequest.apply)

  case class CacheResetRequest(backend: String, file: String)
  implicit val CacheResetRequest_format: RootJsonFormat[CacheResetRequest] = jsonFormat2(CacheResetRequest.apply)
}