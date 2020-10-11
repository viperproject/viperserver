package viper.server

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.Position

object Common {
  //TODO Get this from client programatically
  var viperFileEndings: Array[String] = Array("vpr", ".sil")

  def uriFromString(uri: String): URI = {
    URI.create(uri)
  }

  def uriToPath(uri: URI): Path = {
    Path.of(uri)
  }

//  def pathToUri(path: String): String = {
//    Paths.get(path).toUri.toString
//  }

  def filenameFromUri(uri: String): String = {
    Paths.get(uri).getFileName.toString
  }

  def refreshEndings(): CompletableFuture[Void] = {
    Coordinator.client.requestVprFileEndings().thenAccept((s: Array[String]) => {
      viperFileEndings = s
    }).exceptionally(e => {
      Log.debug(s"GetViperFileEndings request was rejected by the client: $e")
      null
    })
  }

  def isViperSourceFile(uri: String): CompletableFuture[Boolean] = {
    def areEndingsDefined: Boolean = viperFileEndings != null && viperFileEndings.nonEmpty
    if (areEndingsDefined) {
      val endingMatches = viperFileEndings.exists(ending => uri.endsWith(ending))
      CompletableFuture.completedFuture(endingMatches)
    } else { // need to refresh endings and then compare
      Log.debug("Refreshing the viper file endings.")
      refreshEndings().thenApply(a => {
        if (areEndingsDefined) {
          println("Endings are defined!")
          viperFileEndings.foreach(s => s.drop(1))
          viperFileEndings.exists(ending => uri.endsWith(ending))
        } else {
          println("Endings not are defined!")
          false
        }
      })
    }
  }

//  def prettyRange(range: Range): String = {
//    s"${prettyPos(range.start)}-${prettyPos(range.end)}"
//  }

//  def prettyPos(pos: Position): String = {
//    s"${pos.line + 1}:${pos.character + 1}"
//  }

//  def getPositionOfState(index): Position = {
//    if (index >= 0 && index < this.steps.length) {
//      if (this.steps[index].position) {
//        return this.steps[index].position
//      } else {
//        return {
//          line: 0
//          , character: 0
//        }
//      }
//    } else {
//      return {
//        line: -1
//        , character: -1
//      }
//    }
//  }

  def comparePosition(a: Position, b: Position): Int = {
    if (a == null && b == null) return 0
    if (a != null) return -1
    if (b != null) return 1
    if (a.getLine < b.getLine || (a.getLine == b.getLine && a.getCharacter < b.getCharacter)) {
      -1
    } else if (a.getLine == b.getLine && a.getCharacter == b.getCharacter) {
      0
    } else {
      1
    }
  }

//  private def comparePositionAndIndex(a: Statement, b: Statement): Int = {
//    if (!a && !b) return 0
//    if (!a) return -1
//    if (!b) return 1
//    if (a.position.line < b.position.line || (a.position.line === b.position.line && a.position.character < b.position.character)) {
//      return -1
//    } else if (a.position.line === b.position.line && a.position.character === b.position.character) {
//      return (a.index < b.index) ? -1: 1
//    } else {
//      return 1
//    }
//  }

//  private def compareByIndex(a: Statement, b: Statement): Int = {
//    if (!a && !b) return 0
//    if (!a) return -1
//    if (!b) return 1
//    if (a.index < b.index) {
//      return -1
//    } else if (a.index === b.index) {
//      return 0
//    } else {
//      return 1
//    }
//  }

//  private def prettySteps(steps: Array[Statement]): String = {
//    try {
//      val res: String = ""
//      val methodIndex = - 1
//      val currentMethodOffset = - 1
//      val maxLine = 0
//      val indent = ""
//      val allBordersPrinted = false
//
//      //      val currentMethod
//
//      val numberOfClientSteps = 0
//      steps.foreach((element, i) => {
//        val clientNumber = element.decorationOptions ? "" + element.decorationOptions.numberToDisplay: ""
//
//        if (element.canBeShownAsDecoration) {
//          numberOfClientSteps ++
//        }
//
//        val parent = element.getClientParent ()
//        if (parent && element.decorationOptions) {
//          clientNumber += " " + parent.decorationOptions.numberToDisplay
//        }
//
//        val serverNumber = "" + i
//        val spacesToPut = 8 - clientNumber.length - serverNumber.length
//        spacesToPut = spacesToPut < 0 ? 0: spacesToPut
//        res += `\n\t${clientNumber} ${"\t".repeat(spacesToPut)}(${serverNumber})|${"\t".repeat(element.depthLevel())} ${element.firstLine()}`
//      })
//
//      res += '\ nNumberOfClientSteps: ' + numberOfClientSteps
//      //Log.log("Steps:\n" + res, LogLevel.LowLevelDebug)
//      return res
//    } catch (e) {
//      Log.debug ("Runtime Error in Pretty Steps: " + e)
//    }
//  }

  //Helper methods for child processes
//  def executer(command: String, dataHandler?: (String) => void, errorHandler?: (String) => void, exitHandler?: () => void): child_process.ChildProcess = {
//    try {
//      Log.logWithOrigin("executer", command, LogLevel.Debug)
//      val child: child_process.ChildProcess = child_process.exec(command, function (error, stdout, stderr) {
//        Log.logWithOrigin('executer:stdout', stdout, LogLevel.LowLevelDebug)
//        if (dataHandler) {
//          dataHandler(stdout)
//        }
//        Log.logWithOrigin('executer:stderr', stderr, LogLevel.LowLevelDebug)
//        if (errorHandler) {
//          errorHandler(stderr)
//        }
//        if (error !== null) {
//          Log.logWithOrigin('executer', ''+error, LogLevel.LowLevelDebug)
//        }
//        if (exitHandler) {
//          Log.logWithOrigin('executer', 'done', LogLevel.LowLevelDebug)
//          exitHandler()
//        }
//      })
//      return child
//    } catch (e) {
//      Log.error("Error executing " + command + ": " + e)
//    }
//  }

//  def sudoExecuter(command: String, name: String, callback) = {
//    Log.log("sudo-executer: " + command, LogLevel.Debug)
//    val options = { name: name }
//    val child = sudo.exec(command, options, function (error, stdout, stderr) {
//      Log.logWithOrigin('stdout', stdout, LogLevel.LowLevelDebug)
//      Log.logWithOrigin('stderr', stderr, LogLevel.LowLevelDebug)
//      if (error) {
//        Log.error('sudo-executer error: ' + error)
//      }
//      callback()
//    })
//  }

//  def spawner(command: String, args: String[]): child_process.ChildProcess = {
//    Log.log("spawner: " + command + " " + args.join(" "), LogLevel.Debug)
//    try {
//    val child = child_process.spawn(command, args, { detached: true })
//    child.on('stdout', data => {
//    Log.logWithOrigin('spawner stdout', data, LogLevel.LowLevelDebug)
//  })
//    child.on('stderr', data => {
//    Log.logWithOrigin('spawner stderr', data, LogLevel.LowLevelDebug)
//  })
//    child.on('exit', data => {
//    Log.log('spawner done: ' + data, LogLevel.LowLevelDebug)
//  })
//    return child
//  } catch (e) {
//    Log.error("Error spawning command: " + e)
//  }
//  }

//  def backendRestartNeeded(settings: ViperSettings, oldBackendName: String, newBackendName: String) = {
//    if (!settings)
//      return true
//
//    val oldBackend = settings.verificationBackends.find(value => value.name == oldBackendName)
//    val newBackend = settings.verificationBackends.find(value => value.name == newBackendName)
//
//    if (oldBackend && newBackend && oldBackend.engine.toLowerCase() == 'viperserver' && newBackend.engine.toLowerCase() == 'viperserver')
//    return false
//
//    return true
//  }

//  def makeSureFileExistsAndCheckForWritePermission(filePath: String, firstTry = true): Future[any] = {
//    return new Future((resolve, reject) => {
//      try {
//        val folder = pathHelper.dirname(filePath)
//        mkdirp(folder, (err) => {
//          if (err && err.code != 'EEXIST') {
//            resolve(err.code + ": Error creating " + folder + " " + err.message)
//          } else {
//            fs.open(filePath, 'a', (err, file) => {
//              if (err) {
//                resolve(err.code + ": Error opening " + filePath + " " + err.message)
//              } else {
//                fs.close(file, err => {
//                  if (err) {
//                    resolve(err.code + ": Error closing " + filePath + " " + err.message)
//                  } else {
//                    fs.access(filePath, 2, (e) => { //fs.constants.W_OK is 2
//                      if (e) {
//                        resolve(e.code + ": Error accessing " + filePath + " " + e.message)
//                      } else {
//                        resolve(null)
//                      }
//                    })
//                  }
//                })
//              }
//            })
//          }
//        })
//      } catch (e) {
//        resolve(e)
//      }
//    })
//  }

//  def extract(filePath: String): Future[Boolean] = {
//    return new Future((resolve, reject) => {
//      try {
//        //extract files
//        Log.log("Extracting files...", LogLevel.Info)
//        Log.startProgress()
//        val unzipper = new DecompressZip(filePath)
//
//        unzipper.on('progress', function (fileIndex, fileCount) {
//          Log.progress("Extracting Viper Tools", fileIndex + 1, fileCount, LogLevel.Debug)
//        })
//
//        unzipper.on('error', function (e) {
//          if (e.code && e.code == 'ENOENT') {
//            Log.debug("Error updating the Viper Tools, missing create file permission in the viper tools directory: " + e)
//          } else if (e.code && e.code == 'EACCES') {
//            Log.debug("Error extracting " + filePath + ": " + e + " | " + e.message)
//          } else {
//            Log.debug("Error extracting " + filePath + ": " + e)
//          }
//          resolve(false)
//        })
//
//        unzipper.on('extract', function (log) {
//          resolve(true)
//        })
//
//        unzipper.extract({
//          path: pathHelper.dirname(filePath),
//          filter: function (file) {
//            return file.type !== "SymbolicLink"
//          }
//        })
//      } catch (e) {
//        Log.debug("Error extracting viper tools: " + e)
//        resolve(false)
//      }
//    })
//  }

//  def getParentDir(fileOrFolderPath: String): String = {
//    if (!fileOrFolderPath) return null
//    val obj = pathHelper.parse(fileOrFolderPath)
//    if (obj.base) {
//      return obj.dir
//    }
//    val folderPath = obj.dir
//    val is_matching = folderPath.match(/(^.*)[\/\\].+$/) //the regex retrieves the parent directory
//    if (is_matching) {
//      if (is_matching[1] == fileOrFolderPath) {
//        Log.debug("getParentDir has a fixpoint at " + fileOrFolderPath)
//        return null
//      }
//      return match[1]
//    }
//    else {
//      return null
//    }
//  }
}