// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2022 ETH Zurich.

package viper.server.frontends.lsp

import ch.qos.logback.classic.Logger
import org.eclipse.lsp4j.{DocumentSymbol, FoldingRange, Position, Range}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp.VerificationState.Ready

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

/** manages per-client state and interacts with the server instance (which is shared among all clients) */
class ClientCoordinator(val server: ViperServerService)(implicit executor: VerificationExecutionContext) {
  val localLogger: Logger = ClientLogger(this, "Client logger", server.config.logLevel()).get
  val logger: Logger = server.combineLoggers(Some(localLogger))
  private var _client: Option[IdeLanguageClient] = None
  private val _files: ConcurrentMap[String, FileManager] = new ConcurrentHashMap() // each file is managed individually.
  private var _vprFileEndings: Option[Array[String]] = None
  private var _exited: Boolean = false
  private val _toProjectRoot: ConcurrentMap[String, String] = new ConcurrentHashMap()

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
    _files.clear()
  }

  def isAlive: Boolean = {
    !_exited
  }

  def addFileIfNecessary(uri: String): Unit = {
    logger.trace(s"Adding FileManager for $uri if it does not exist yet")
    val coordinator = this
    val createFMFunc = new java.util.function.Function[String, FileManager]() {
      override def apply(t: String): FileManager = {
        logger.trace(s"FileManager created for $uri")
        new FileManager(coordinator, uri)
      }
    }
    // we use `computeIfAbsent` instead of `putIfAbsent` such that a new FileManager is only created if it's absent
    _files.computeIfAbsent(uri, createFMFunc)
  }

  def removeFileIfExists(uri: String): Unit = {
    logger.trace(s"Removing FileManager for $uri")
    _files.remove(uri)
  }

  /** clears definitions and symbols associated with a file, when not verifying */
  def resetFileInfo(uri: String): Unit = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .foreach(_.resetFileInfo())
  }

  def resetDiagnostics(uri: String): Unit = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .foreach(_.resetDiagnostics())
  }

  def handleChange(uri: String, range: Range, text: String): Unit = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .foreach(_.handleChange(uri, range, text))
  }

  def getSymbolsForFile(uri: String): Seq[DocumentSymbol] = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .map(_.dsm.symbolInformationForFile(uri))
      .getOrElse(Seq.empty)
  }

  def getDefinitionsForPos(uri: String, pos: Position): Seq[Definition] = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .map(_.definitions.filter(d =>
        Common.isGlobalRange(d.scope) || (uri == d.uri && Common.containsPos(d.scope, pos))
      ).toSeq)
      .getOrElse(Seq.empty)
  }

  def getFoldingRangesForFile(uri: String): Seq[FoldingRange] = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .map(_.foldingRanges.get(uri).toSeq.flatten)
      .getOrElse(Seq.empty)
  }

  def getSemanticTokens(uri: String): Seq[SemanticToken] = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .map(_.semanticTokens.get(uri).toSeq.flatten)
      .getOrElse(Seq.empty)
  }

  def registerSignatureHelpStart(uri: String, pos: Position): Unit = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .foreach(_.signatureHelp = Some(pos))
  }

  def getSignatureHelpStart(uri: String): Option[Position] = {
    val project = toProjectRoot(uri)
    Option(_files.get(project))
      .flatMap(_.signatureHelp)
  }

  /** Checks if verification can be started for a given file.
    *
    * Informs client differently depending on whether or not verification attempt was triggered manually
    * */
  def canVerificationBeStarted(uri: String, manuallyTriggered: Boolean): Boolean = {
    logger.trace("canVerificationBeStarted")
    if (server.isRunning) {
      logger.trace("server is running")
      // This will be the new project root
      removeFromProject(uri)
      addFileIfNecessary(uri)
      true
    } else {
      logger.trace("server is not running")
      if (manuallyTriggered) {
        hint("The server is not ready yet")
      }
      logger.debug("The server is not ready yet")
      false
    }
  }

  def stopRunningVerification(uri: String): Future[Boolean] = {
    Option(_files.get(uri))
      .map(fm => fm.stopVerification()
        .map(_ => {
          logger.trace(s"stopVerification has completed for ${fm.uri}")
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
    val tasks = _files.values().asScala.map(fm =>
      fm.stopVerification().map(_ => {
        logger.trace(s"stopVerification has completed for ${fm.uri}")
      }))
    Future.sequence(tasks).map(_ => {
      logger.debug("all running verifications have been stopped")
    })
  }

  /** returns true if verification was started */
  def startVerification(backendClassName: String, customArgs: String, uri: String, manuallyTriggered: Boolean): Boolean = {
    Option(_files.get(uri))
      .exists(fm => fm.startVerification(backendClassName, customArgs, manuallyTriggered))
  }

  /** flushes verification cache, optionally only for a particular file */
  def flushCache(uriOpt: Option[String], backendOpt: Option[String]): Future[Unit] = {
    (uriOpt, backendOpt) match {
      case (Some(uri), Some(backend)) =>
        val success = server.flushCachePartially(Some((backend, uri)), Some(localLogger))
        if (success) {
          Future.unit
        } else {
          Future.failed(new Exception(s"Flushing the cache failed because no cache for backend $backend and file $uri not found"))
        }

      case (Some(_), None) =>
        Future.failed(new Exception(s"Flushing the cache failed because no backend is currently selected"))

      case (None, _) =>
        logger.info("Flushing entire cache...")
        val success = server.flushCachePartially(None, Some(localLogger))
        if (success) {
          Future.unit
        } else {
          Future.failed(new Exception(s"Flushing the cache failed"))
        }
    }
  }

  /** Notifies the client about a state change
    *
    * If state change is related to a particular file, its manager's state is also updated.
    * */
  def sendStateChangeNotification(params: StateChangeParams, task: Option[FileManager]): Unit = {
    // update file manager's state:
    task.foreach(fm => fm.data.state = VerificationState(params.newState))
    try {
      client.notifyStateChanged(params)
    } catch {
      case e: Throwable => logger.debug(s"Error while changing state: $e")
    }
  }

  def refreshEndings(): Future[Array[String]] = {
    client.requestVprFileEndings().asScala.map(response => {
      _vprFileEndings = Some(response.fileEndings)
      response.fileEndings
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

  def hint(message: String, showSettingsButton: Boolean = false, showViperToolsUpdateButton: Boolean = false): Unit = {
    if (!isAlive) return
    client.notifyHint(HintMessage(message, showSettingsButton, showViperToolsUpdateButton ))
  }

  def setupProject(root: String, otherUris: Array[String]) = {
    for (uri <- otherUris) {
      _toProjectRoot.put(uri, root)
    }
    val setupProject = SetupProjectParams(root, otherUris)
    client.requestSetupProject(setupProject)
  }

  def removeFromProject(uri: String) = {
    _toProjectRoot.remove(uri)
  }

  def toProjectRoot(uri: String): String = {
    _toProjectRoot.getOrDefault(uri, uri)
  }
}
