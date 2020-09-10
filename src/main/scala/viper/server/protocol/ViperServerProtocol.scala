package viper.server.protocol

object ViperServerProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(args: List[String],
                    program: viper.silver.ast.Program)

  // Verification interrupt request to Main Actor
  case class Stop()
}
