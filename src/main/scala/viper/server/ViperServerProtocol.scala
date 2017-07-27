/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.actor.ActorRef

object ViperServerProtocol {

  //Main Actor requests Verification
  case class Verify(args: List[String])

  //VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  //Main Actor requests verification stop
  case object Stop

  //Verification Actor reports end of verification
  case object Stopped
}