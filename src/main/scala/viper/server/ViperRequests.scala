package viper.server

import spray.json._

object ViperRequests extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  // Legacy verification request format.
  // TODO: use JSon for submitting verification requests.
  case class VerificationRequest(arg: String)
  implicit val VerificationRequest_format: RootJsonFormat[VerificationRequest] = jsonFormat1(VerificationRequest.apply)

  case class CacheResetRequest(backend: String, file: String)
  implicit val CacheResetRequest_format: RootJsonFormat[CacheResetRequest] = jsonFormat2(CacheResetRequest.apply)

  // Other requests go below this line.

}
