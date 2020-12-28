// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import viper.server.frontends.lsp.LogLevel._

object Log {

  def log(message: String, logLevel: LogLevel) = {
    require(message != null && logLevel != null)
    Coordinator.client.notifyLog(message, logLevel.id)
  }

  def toLogFile(message: String) = log(message, Default)

  def debug(message: String) = log(message, Debug)

  def debug(message: String, error: Throwable) = log(s"$message $error", Debug)

  def info(message: String) = log(message, Info)

  def lowLevel(message: String) = log(message, LowLevelDebug)

  private var lastProgress: Double = _

  def startProgress() = {
    lastProgress = 0
  }

  def progress(domain: String, cur: Double, len: Double, logLevel: LogLevel) = {
    val progress = 100.0 * cur / len
    if (Math.floor(progress) > lastProgress) {
      lastProgress = progress
      val data = ProgressReport(domain, cur, len, progress, Double.NaN)
      Coordinator.client.notifyProgress(data, logLevel.id)
    }
  }

  def logWithOrigin(origin: String, message: String, logLevel: LogLevel) = {
    if (message != null) {
      val indicator: String = if (logLevel >= Debug) "[" + origin + "]: " else ""
      val msg_with_origin: String = indicator + message
      log(msg_with_origin, logLevel)
    }
  }

  def hint(message: String, showSettingsButton: Boolean = false, showViperToolsUpdateButton: Boolean = false) = {
    Coordinator.client.notifyHint(S2C_Commands.Hint, Hint(message, showSettingsButton, showViperToolsUpdateButton ))
  }
}