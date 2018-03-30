package viper.server

import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.FileAppender
import org.slf4j.LoggerFactory


class ViperServerLogger(val file: String, val level: String) {

  private val str_to_level: String => Level = {
    case "OFF" => Level OFF
    case "ERROR" => Level ERROR
    case "INFO" => Level INFO
    case "WARN" => Level WARN
    case "DEBUG" => Level DEBUG
    case "TRACE" => Level TRACE
    case "ALL" => Level ALL
  }

  /** Borrowed from https://stackoverflow.com/questions/16910955/programmatically-configure-logback-appender */
  private def createLoggerFor(string: String, file: String, str_level: String) = {
    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val ple = new PatternLayoutEncoder
    ple.setPattern("%date %level [%thread] %logger{10} [%file:%line] %msg%n")
    ple.setContext(lc)
    ple.start()
    val fileAppender = new FileAppender[ILoggingEvent]
    fileAppender.setFile(file)
    fileAppender.setEncoder(ple)
    fileAppender.setContext(lc)
    fileAppender.start()
    val logger = LoggerFactory.getLogger(string).asInstanceOf[Logger]
    logger.addAppender(fileAppender)
    logger.setLevel( str_to_level(str_level) )
    logger.setAdditive(false) /* set to true if root should log too */
    logger
  }

  lazy val get: Logger = createLoggerFor("ViperServerLogger", file, level)
}
