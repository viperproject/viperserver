package  viper.server

import java.io.File

import org.rogach.scallop.{ScallopConf, ScallopOption, singleArgConverter}

class ViperConfig(args: Seq[String]) extends ScallopConf(args) {

  private val logging_levels = Array("ALL", "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "OFF")

  val logLevel: ScallopOption[String] = opt[String]("logLevel",
    descr = s"One of the log levels: ${logging_levels.mkString(",")}.",
    default = Some("ERROR"),
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
      val f = canonizedLogFile(path)
      (f.isFile || f.isDirectory) && f.canWrite || f.getParentFile.canWrite
    },
    noshort = true,
    hidden = false)

  private def canonizedLogFile(some_file_path: String): File = {
    val f = new File(some_file_path)
    if (f.isAbsolute) {
      f
    } else {
      java.nio.file.Paths.get(System.getProperty("user.dir"), some_file_path).toFile
    }
  }
  def getLogFileWithGuarantee: String = {
    val cf: File = canonizedLogFile(logFile())
    if ( cf.isDirectory ) {
      val log: File = java.io.File.createTempFile("viperserver_journal_" + System.currentTimeMillis(), ".log", cf)
      log.toString
    } else {
      cf.toString
    }
  }

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
