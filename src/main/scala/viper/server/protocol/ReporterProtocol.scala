package viper.server.protocol

object ReporterProtocol {
    case class ServerReport(msg: viper.silver.reporter.Message)
    case class FinalServerReport(success: Boolean)
}
