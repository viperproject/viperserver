package viper.server.frontends.mcp

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.ConsoleAppender
import org.slf4j.LoggerFactory

object McpLogging {
  /**
   * In MCP mode, stdout is the JSON-RPC channel. Logback's default configuration makes the root logger print to
   * stdout, which breaks the protocol for all loggers without their own appenders (e.g. Reactor, the MCP SDK, Silicon).
   * Hence, we replace the root logger's appenders by one printing to stderr. Loggers with their own appenders and
   * additivity disabled (e.g. ViperLogger) are not affected.
   */
  def redirectRootLoggerToStdErr(): Unit = {
    val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = context.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)

    val encoder = new PatternLayoutEncoder
    encoder.setContext(context)
    encoder.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n")
    encoder.start()

    val stdErrAppender = new ConsoleAppender[ILoggingEvent]
    stdErrAppender.setContext(context)
    stdErrAppender.setName("STDERR")
    stdErrAppender.setTarget("System.err")
    stdErrAppender.setEncoder(encoder)
    stdErrAppender.start()

    rootLogger.detachAndStopAllAppenders()
    rootLogger.addAppender(stdErrAppender)
  }

  def redirectRootLoggerToNull(): Unit = {
    val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = context.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    rootLogger.detachAndStopAllAppenders()
  }
}
