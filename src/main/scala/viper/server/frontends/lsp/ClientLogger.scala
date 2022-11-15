// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2022 ETH Zurich.

package viper.server.frontends.lsp

import ch.qos.logback.classic.Level.toLevel
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.core.UnsynchronizedAppenderBase
import org.slf4j.LoggerFactory
import viper.server.frontends.lsp.LogLevel.LogLevel

class ClientLogger(coordinator: ClientCoordinator, name: String, level: String) {

  lazy val get: Logger = createLoggerFor(name, level)

  // inspired from ViperLogger
  private def createLoggerFor(string: String, str_level: String): Logger = {
    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logger = LoggerFactory.getLogger(string).asInstanceOf[Logger]
    val clientAppender = new ClientAppender(coordinator)
    clientAppender.setContext(lc)
    clientAppender.start()
    logger.addAppender(clientAppender)
    logger.setLevel(toLevel(str_level))
    logger.setAdditive(false) /* set to true if root should log too */
    logger
  }
}

object ClientLogger {
  def apply(coordinator: ClientCoordinator, name: String, level: String = "ERROR"): ClientLogger =
    new ClientLogger(coordinator, name, level)
}

class ClientAppender(coordinator: ClientCoordinator) extends UnsynchronizedAppenderBase[ILoggingEvent] {
  override def append(event: ILoggingEvent): Unit = {
    def getParams(logLevel: LogLevel): LogParams =
      LogParams(event.getMessage, logLevel.id)

    if (!coordinator.isAlive) return
    event.getLevel match {
      case Level.OFF =>
      case Level.ERROR => coordinator.client.notifyLog(getParams(LogLevel.Info))
      case Level.WARN => coordinator.client.notifyLog(getParams(LogLevel.Info))
      case Level.INFO => coordinator.client.notifyLog(getParams(LogLevel.Info))
      case Level.DEBUG => coordinator.client.notifyLog(getParams(LogLevel.Debug))
      case Level.TRACE => coordinator.client.notifyLog(getParams(LogLevel.LowLevelDebug))
      case Level.ALL => coordinator.client.notifyLog(getParams(LogLevel.LowLevelDebug))
    }
  }
}
