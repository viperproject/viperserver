package viper.server.utility

import ch.qos.logback.classic.Logger
import viper.silver.frontend.{SilFrontend, ReformatterAstProvider}
import viper.silver.reporter.{NoopReporter, Reporter}

class ReformatterAstGenerator(private val _logger: Logger,
                              private val _reporter: Reporter = NoopReporter,
                              private val argList: Seq[String] = Seq(),
                              private val disablePlugins: Boolean = false) extends AstGenerator(_logger, _reporter, argList, disablePlugins) {

  protected override val _frontend: SilFrontend = {
    _logger.info(s"Creating new AstGenerator instance.")
    new ReformatterAstProvider(_reporter)
  }
}
