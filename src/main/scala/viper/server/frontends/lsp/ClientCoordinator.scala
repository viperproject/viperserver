// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2022 ETH Zurich.

package viper.server.frontends.lsp

import ch.qos.logback.classic.Logger
import org.eclipse.lsp4j.SymbolInformation
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.LogLevel.LogLevel
import viper.server.frontends.lsp.VerificationState.Ready

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

/** manages per-client state and interacts with the server instance (which is shared among all clients) */
class ClientCoordinator(val server: ViperServerService)(implicit executor: VerificationExecutionContext) {
  val logger: Logger = ClientLogger(this, "Client logger", server.config.logLevel()).get
  private var _client: Option[IdeLanguageClient] = None
  private var _backend: Option[BackendProperties] = None // currently selected backend
  private val _files: ConcurrentMap[String, FileManager] = new ConcurrentHashMap() // each file is managed individually.
  private var _vprFileEndings: Option[Array[String]] = None
  private var _exited: Boolean = false

  def client: IdeLanguageClient = {
    _client.getOrElse({assert(false, "getting client failed - client has not been set yet"); ???})
  }

  def setClient(c: IdeLanguageClient): Unit = {
    assert(_client.isEmpty, "setting client failed - client has already been set")
    _client = Some(c)
  }

  /** called when client disconnects; the server should however remain running */
  def exit(): Unit = {
    _exited = true
    _client = None
    _backend = None
    _files.clear()
  }

  def isAlive: Boolean = {
    !_exited
  }

  def backend: Option[BackendProperties] = _backend

  def changeBackendIfNecessary(newBackend: BackendProperties): Unit = {
    if (backend.contains(newBackend)) {
      logger.debug("No need to restart backend. It is still the same")
      val param = BackendReadyParams(newBackend.backend_type, restarted = false, isViperServer = true)
      client.notifyBackendReady(param)
    } else {
      val oldBackend = _backend
      _backend = Some(newBackend)
      if (oldBackend.isEmpty) {
        client.notifyBackendStarted(BackendStartedParams(newBackend.backend_type))
      } else {
        val param = BackendReadyParams(newBackend.backend_type, restarted = true, isViperServer = true)
        client.notifyBackendReady(param)
      }
    }
  }

  def addFileIfNecessary(uri: String): Unit = {
    _files.putIfAbsent(uri, new FileManager(this, uri))
    logger.trace(s"FileManager created for $uri")
  }

  def removeFileIfExists(uri: String): Unit = {
    logger.trace(s"Removing FileManager for $uri")
    _files.remove(uri)
  }

  /** clears definitions and symbols associated with a file */
  def resetFile(uri: String): Unit = {
    Option(_files.get(uri))
      .foreach(fm => {
        fm.symbolInformation = ArrayBuffer.empty
        fm.definitions = ArrayBuffer.empty
      })
  }

  def resetDiagnostics(uri: String): Unit = {
    Option(_files.get(uri))
      .foreach(fm => fm.resetDiagnostics())
  }

  def getSymbolsForFile(uri: String): Array[SymbolInformation]= {
    Option(_files.get(uri))
      .map(fm => fm.symbolInformation.toArray)
      .getOrElse(Array.empty)
  }

  def getDefinitionsForFile(uri: String): ArrayBuffer[Definition] = {
    Option(_files.get(uri))
      .map(fm => fm.definitions)
      .getOrElse(ArrayBuffer.empty)
  }

  /** Checks if verification can be started for a given file.
    *
    * Informs client differently depending on whether or not verification attempt was triggered manually
    * */
  def canVerificationBeStarted(uri: String, manuallyTriggered: Boolean): Boolean = {
    // check if there is already a verification task for that file
    if(!_files.containsKey(uri)){
      logger.debug(s"No verification task found for file: $uri")
      false
    } else if (!server.isRunning) {
      if (manuallyTriggered) {
        hint("The server is not ready yet")
      }
      logger.debug("The server is not ready yet")
      false
    } else {
      true
    }
  }

  def stopRunningVerification(uri: String): Future[Boolean] = {
    Option(_files.get(uri))
      .map(fm => fm.stopVerification()
        .map(_ => {
          val params = StateChangeParams(Ready.id, verificationCompleted = 0, verificationNeeded = 0, uri = uri)
          sendStateChangeNotification(params, Some(fm))
          true
        })
        .recover(_ => false))
      .getOrElse(Future.successful(false))
  }

  /** Stops all running verifications.
    *
    * Returns a CF that is successfully completed if all running verifications were stopped
    * successfully. Otherwise, a failed CF is returned
    * */
  def stopAllRunningVerifications(): Future[Unit] = {
    val tasks = _files.values().asScala.map(fm => fm.stopVerification())
    Future.sequence(tasks).map(_ => ())
  }

  /** returns true if verification was started */
  def startVerification(uri: String, manuallyTriggered: Boolean): Boolean = {
    Option(_files.get(uri))
      .exists(fm => fm.startVerification(manuallyTriggered))
  }

  /** flushes verification cache, optionally only for a particular file */
  def flushCache(uriOpt: Option[String]): Unit = {
    (uriOpt, backend) match {
      case (Some(uri), Some(backendProps)) =>
        val success = server.flushCachePartially(Some((uri, backendProps.backend_type)), Some(logger))
        if (!success) {
          hint(s"Flushing the cache failed because no cache for backend ${backendProps.backend_type} and file $uri not found")
        }

      case (Some(_), None) =>
        hint(s"Flushing the cache failed because no backend is currently selected")

      case (None, _) =>
        logger.info("Flushing entire cache...")
        val success = server.flushCachePartially(None, Some(logger))
        if (!success) {
          hint(s"Flushing the cache failed")
        }
    }
  }

  /** Notifies the client about a state change
    *
    * If state change is related to a particular file, its manager's state is also updated.
    * */
  def sendStateChangeNotification(params: StateChangeParams, task: Option[FileManager]): Unit = {
    // update file manager's state:
    task.foreach(fm => fm.state = VerificationState(params.newState))
    try {
      client.notifyStateChanged(params)
    } catch {
      case e: Throwable => logger.debug(s"Error while changing state: $e")
    }
  }

  def refreshEndings(): Future[Array[String]] = {
    client.requestVprFileEndings().asScala.map(endings => {
      _vprFileEndings = Some(endings)
      endings
    })
  }

  def isViperSourceFile(uri: String): Future[Boolean] = {
    _vprFileEndings
      // if available, take it:
      .map(endings => Future.successful(endings))
      // if file endings are not yet available, get them from the client:
      .getOrElse(refreshEndings())
      // now check that `uri` ends with one of the file endings:
      .map(endings => endings.exists(ending => uri.endsWith(ending)))
  }

  private var lastProgress: Double = 0

  def startProgress(): Unit = {
    lastProgress = 0
  }

  def progress(domain: String, cur: Double, len: Double, logLevel: LogLevel): Unit = {
    if (!isAlive) return
    val progress = 100.0 * cur / len
    if (Math.floor(progress) > lastProgress) {
      lastProgress = progress
      val data = ProgressReport(domain, cur, len, progress, Double.NaN)
      client.notifyProgress(data, logLevel.id)
    }
  }

  def hint(message: String, showSettingsButton: Boolean = false, showViperToolsUpdateButton: Boolean = false): Unit = {
    if (!isAlive) return
    client.notifyHint(S2C_Commands.Hint, Hint(message, showSettingsButton, showViperToolsUpdateButton ))
  }
}
