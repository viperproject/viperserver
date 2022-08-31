// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2022 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory

/** combines multiple loggers such that each log message is logged to all `loggers` */
class MultiLogger(name: String, loggers: Seq[Logger]) {
  lazy val get: Logger = createLoggerFor(name, loggers)

  private def createLoggerFor(name: String, loggers: Seq[Logger]): Logger = {
    require(loggers.nonEmpty)
    val logger = LoggerFactory.getLogger(name).asInstanceOf[Logger]
    loggers.foreach(l => l.iteratorForAppenders().forEachRemaining(appender => logger.addAppender(appender)))
    logger.setLevel(loggers.head.getLevel)
    logger.setAdditive(loggers.head.isAdditive)
    logger
  }
}

object MultiLogger {
  def apply(name: String, loggers: Seq[Logger]): MultiLogger =
    new MultiLogger(name, loggers)
}
