package  viper.server

import org.rogach.scallop.{ScallopConf, ScallopOption, singleArgConverter}

import viper.silicon.Silicon

class ViperConfig(args: Seq[String]) extends ScallopConf(args) {

  val logLevel: ScallopOption[String] = opt[String]("logLevel",
    descr = "One of the log levels ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF (default: OFF)",
    default = Some("WARN"),
    noshort = true,
    hidden = Silicon.hideInternalOptions
  )(singleArgConverter(level => level.toUpperCase))

  val ideMode: ScallopOption[Boolean] = opt[Boolean]("ideMode",
    descr = ("Used for VS Code IDE. Report errors in json format, and write"
      + "errors in the format '<file>,<line>:<col>,<line>:<col>,<message>' to"
      + "a file (see option ideModeErrorFile)."),
    default = Some(false),
    noshort = true,
    hidden = false
  )

  val backendSpecificCache: ScallopOption[Boolean] = opt[Boolean]("backendSpecificCache",
    descr = "Use a separate cache for each backend?",
    default = Some(false),
    noshort = true,
    hidden = false
  )

  val ideModeAdvanced: ScallopOption[Boolean] = opt[Boolean]("ideModeAdvanced",
    descr = ("Used for VS Code IDE. Write symbolic execution log into .vscode/executionTreeData.js file, "
      + "write execution tree graph into .vscode/dot_input.dot, "
      + "and output z3's counter example models."),
    default = Some(false),
    noshort = true,
    hidden = true
  )

  dependsOnAll(ideModeAdvanced, ideMode :: Nil)
}
