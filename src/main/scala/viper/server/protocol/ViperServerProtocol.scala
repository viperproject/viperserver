package viper.server.protocol

object ViperServerProtocol {

  // Main Actor requests Verification with File Name
  case class Verify(args: List[String],
                    program: viper.silver.ast.Program)

  // Main Actor requests Verification with AST Program
 // case class VerifyAst(config: List[String], reporter: viper.silver.reporter.Reporter, program: viper.silver.ast.Program)

  // VerificationActor sends backend to Main Actor
  case class Backend(backend: viper.silver.verifier.Verifier)

  // Verification interrupt request to Main Actor
  case class Stop(call_me_back: Boolean)
}
