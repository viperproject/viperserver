package viper.server

import LogLevel._

object Log {
  def log(message: String, logLevel: LogLevel) = {
    Coordinator.client.notifyLog(message, logLevel.id)
  }

  def toLogFile(message: String) = log(message, Default)

  def debug(message: String) = log(message, Debug)

  def debug(message: String, error: Throwable) = log(message, Debug)

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
      val data = Progress(domain, cur, len, progress, null)
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

// -> ??
//  def logOutput(process: child_process.ChildProcess, label: String) = {
//    process.stdout.on('data', (data: String) => {
//      Log.logWithOrigin(label, data, LogLevel.LowLevelDebug)
//    })
//    process.stdout.on('data', (data: String) => {
//      Log.logWithOrigin(label + " error", data, LogLevel.LowLevelDebug)
//    })
//  }

  def hint(message: String, showSettingsButton: Boolean = false, showViperToolsUpdateButton: Boolean = false) = {
    Coordinator.client.notifyHint(S2C_Commands.Hint, Hint(message, showSettingsButton, showViperToolsUpdateButton ))
  }
}