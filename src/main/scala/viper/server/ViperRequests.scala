package viper.server

import spray.json._

object ViperRequests extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  // Legacy verification request format.
  // TODO: use JSon for submitting verification requests.
  case class VerificationRequest(arg: String)

  implicit val verifyStuff = jsonFormat1(VerificationRequest.apply)


  // Other requests go below this line.
  case class AlloyGenerationRequest(arg: String)

  implicit val generateStuff = jsonFormat1(AlloyGenerationRequest.apply)
}
