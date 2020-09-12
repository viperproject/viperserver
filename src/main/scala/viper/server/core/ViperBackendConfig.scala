// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

trait ViperBackendConfig {
  val partialCommandLine: List[String]
}

object ViperBackendConfigs {
  object EmptyConfig extends ViperBackendConfig {val partialCommandLine: List[String] = Nil}
  
  case class SiliconConfig(partialCommandLine: List[String]) extends ViperBackendConfig
  case class CarbonConfig(partialCommandLine: List[String]) extends ViperBackendConfig
  case class CustomConfig(partialCommandLine: List[String]) extends ViperBackendConfig
}