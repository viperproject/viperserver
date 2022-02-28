// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core


import viper.server.utility.Helpers.getArgListFromArgString


trait ViperBackendConfig {
  val backend_name: String
  val partialCommandLine: List[String]

  def toList: List[String] = this match {
    case _ : SiliconConfig => "silicon" :: partialCommandLine
    case _ : CarbonConfig => "carbon" :: partialCommandLine
    case cc : CustomConfig => "custom" :: cc.backend_name :: partialCommandLine
  }
}

case class SiliconConfig(partialCommandLine: List[String]) extends ViperBackendConfig {
  override val backend_name = "silicon"
}

case class CarbonConfig(partialCommandLine: List[String]) extends ViperBackendConfig {
  override val backend_name = "carbon"
}

case class CustomConfig(partialCommandLine: List[String], backend_name: String) extends ViperBackendConfig

object ViperBackendConfig {

  def apply(args: List[String]): ViperBackendConfig = args match {
    //    case Nil => EmptyConfig
    case "silicon" :: args =>  SiliconConfig(args)
    case "carbon" :: args => CarbonConfig(args)
    case custom :: args => CustomConfig(args, custom)
    case invalid => throw new IllegalArgumentException(s"cannot build ViperConfig from string `$invalid`")
  }
  
  def apply(input: String): ViperBackendConfig = apply(getArgListFromArgString(input))
}