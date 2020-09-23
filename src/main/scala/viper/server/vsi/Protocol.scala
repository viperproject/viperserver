// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import akka.stream.scaladsl.SourceQueueWithComplete
import org.reactivestreams.Publisher
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import viper.server.vsi.Requests.{jsonFormat1, jsonFormat2}

object TaskProtocol {
  case class BackendReport(msg: Envelope)
  case class FinalBackendReport(success: Boolean)
}

object VerificationProtocol {

  // Request Job Actor to execute verification task
  case class Verify(task: Thread, queue: SourceQueueWithComplete[Envelope], publisher: Publisher[Envelope])

  // Verification interrupt request to Terminator Actor
  case class Stop()
}

object Requests extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  case class VerificationRequest(arg: String)
  implicit val VerificationRequest_format: RootJsonFormat[VerificationRequest] = jsonFormat1(VerificationRequest.apply)

  case class CacheResetRequest(backend: String, file: String)
  implicit val CacheResetRequest_format: RootJsonFormat[CacheResetRequest] = jsonFormat2(CacheResetRequest.apply)
}