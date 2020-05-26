/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package viper.server

object ViperServerProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(args: List[String],
                    reporter: Option[viper.silver.reporter.Reporter],
                    program: viper.silver.ast.Program)

  // Main Actor requests Verification with AST Program
 // case class VerifyAst(config: List[String], reporter: viper.silver.reporter.Reporter, program: viper.silver.ast.Program)

  // VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  // Verification interrupt request to Main Actor
  case class Stop(call_me_back: Boolean)
}
