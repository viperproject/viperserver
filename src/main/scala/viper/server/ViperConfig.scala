package  viper.server

import java.io.File

import org.rogach.scallop.{ScallopConf, ScallopOption, singleArgConverter}

class ViperConfig(args: Seq[String]) extends ScallopConf(args) {

  private val logging_levels = Array("ALL", "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "OFF")

  val logLevel: ScallopOption[String] = opt[String]("logLevel",
    descr = s"One of the log levels: ${logging_levels.mkString(",")}",
    default = Some("ALL"),
    validate = (ll: String) => logging_levels.contains(ll.toUpperCase),
    noshort = true,
    hidden = false
  )(singleArgConverter(level => level.toUpperCase))

  val logFile: ScallopOption[String] = opt[String]("logFile",
    descr = "Specifies the location of the log file to be used by ViperServer and the verification " +
            "backends it creates.",
    default = {
      val temp: File = java.io.File.createTempFile("viperserver_journal_" + System.currentTimeMillis(), ".log")
      Some(temp.getAbsolutePath)
    },
    validate = (path: String) => {
      val f = new File(canonizedLogFile(path))
      f.isFile && f.canWrite || f.getParentFile.canWrite
    },
    noshort = true,
    hidden = false)

  def canonizedLogFile(some_file_path: String): String = {
    val f = new File(some_file_path)
    if (f.isAbsolute) {
      some_file_path
    } else {
      java.nio.file.Paths.get(System.getProperty("user.dir"), some_file_path).toAbsolutePath.toString
    }
  }

  def canonizedLogFile(): String = canonizedLogFile(logFile())

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
