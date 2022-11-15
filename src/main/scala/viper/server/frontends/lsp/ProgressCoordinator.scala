// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

class ProgressCoordinator(coordinator: ClientCoordinator, val nofPredicates: Int, val nofFunctions: Int, val nofMethods: Int) {

  var currentFunctions = 0
  var currentMethods = 0
  var currentPredicates = 0

  def updateProgress(output: BackendOutput): Unit = {
    try {
      output.typ match {
        case BackendOutputType.MethodVerified => currentMethods += 1
        case BackendOutputType.FunctionVerified => currentFunctions += 1
        case BackendOutputType.PredicateVerified => currentPredicates += 1
      }
    } catch {
      case e: Throwable => coordinator.logger.debug(s"Error updating progress: $e")
    }
  }

  def toPercent: Double = {
    val total = nofFunctions + nofMethods + nofPredicates
    val current = currentFunctions + currentMethods + currentPredicates
    100 * current / total
  }
}
