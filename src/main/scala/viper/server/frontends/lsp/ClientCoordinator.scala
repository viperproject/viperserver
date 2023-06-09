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
import viper.server.frontends.lsp.file.FileManager
import viper.server.frontends.lsp.file.VerificationManager
// import viper.server.frontends.lsp.file.RangeSelector
import viper.silver.ast.utility
import viper.silver.ast.utility.lsp
import org.eclipse.lsp4j.Diagnostic
import viper.silver.ast.utility.lsp.HoverHint
import viper.silver.ast.utility.lsp.GotoDefinition
import viper.silver.ast.utility.lsp.RangePosition
import org.eclipse.lsp4j.InlayHint
import viper.silver.ast.AbstractSourcePosition
import viper.silver.ast.Trigger
import org.eclipse.lsp4j.CodeLens
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.LocationLink

/** manages per-client state and interacts with the server instance (which is shared among all clients) */
class ClientCoordinator(val server: ViperServerService)(implicit executor: VerificationExecutionContext) {
  val localLogger: Logger = ClientLogger(this, "Client logger", server.config.logLevel()).get
  val logger: Logger = server.combineLoggers(Some(localLogger))
  private var _client: Option[IdeLanguageClient] = None
  private val _files: ConcurrentMap[String, FileManager] = new ConcurrentHashMap() // each file is managed individually.
  private var _vprFileEndings: Option[Array[String]] = None
  private var _exited: Boolean = false
  // private val _toProjectRoot: ConcurrentMap[String, String] = new ConcurrentHashMap()

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

  def closeFile(uri: String): Unit = {
    val toRemove = Option(_files.get(uri)).map(fm => {
      fm.isOpen = false
      fm.removeDiagnostics()
      fm.isRoot
    }).getOrElse(false)
    if (toRemove) {
      logger.trace(s"Removing FileManager for $uri")
      _files.remove(uri)
    }
  }

  /** clears definitions and getSymbols associated with a file, when not verifying */
  // def resetFileInfo(uri: String): Unit = {
  //   val project = toProjectRoot(uri)
  //   Option(_files.get(project))
  //     .foreach(_.resetFileInfo())
  // }

  def resetDiagnostics(uri: String): Unit = {
    getFileManager(uri).removeDiagnostics()
  }

  def handleChange(uri: String, range: Range, text: String): Unit = {
    getFileManager(uri).handleContentChange(range, text)
    // TODO: remove
    // val project = toProjectRoot(uri)
    // Option(_files.get(project))
    //   .foreach(_.handleChange(uri, range, text))
  }

  // def getSymbolsForFile(uri: String): Future[Seq[DocumentSymbol]] = {
  //   getFileManager(uri).getDocumentSymbols()
  // }

  // def getSymbolsForFile(uri: String, fixedRange: Option[Range] = None): Seq[DocumentSymbol] = {
  //   getFileManager(uri)
  //     .map(_.getDocumentSymbols(fixedRange))
  //     .getOrElse(Seq.empty)
  // }

  // def getIdentAtPos(uri: String, pos: Position): Option[(String, Range)] = {
  //   // val project = toProjectRoot(uri)
  //   getFileManager(uri).content.getIdentAtPos(pos)
  // }

  // def getHoverHintsForIdent(uri: String, ident: String, pos: Position): Seq[Hover] = {
  //   getFileManager(uri).hoverHints.get((uri, ident, pos))(logger)
  // }

  // def getGotoDefinitionsForIdent(uri: String, ident: String, pos: Position): Seq[LocationLink] = {
  //   getFileManager(uri).gotoDefinitions.get((uri, ident, pos))(logger)
  // }

  // def getFoldingRangesForFile(uri: String): Future[Seq[FoldingRange]] = {
  //   // val project = toProjectRoot(uri)
  //   getFileManager(uri).getFoldingRanges()
  // }

  // def getInlayHints(uri: String, range: Range): Seq[InlayHint] = {
  //   // val project = toProjectRoot(uri)
  //   getFileManager(uri).inlayHint.get(())(logger).filter(ih => {
  //     Common.containsPos(range, ih.getPosition) == 0
  //   })
  // }

  // def getCodeLens(uri: String): Seq[CodeLens] = {
  //   // val project = toProjectRoot(uri)
  //   getFileManager(uri).codeLens.get(())(logger)
  // }
  
  // def getSemanticTokens(uri: String): Seq[Lsp4jSemanticHighlight] = {
  //   // TODO: remove
  //   Option(_files.get(uri))
  //     .map(_.semanticToken.get(())(logger).toSeq)
  //     .getOrElse(Seq.empty)
  // }

  def registerSignatureHelpStart(uri: String, pos: Position): Unit = {
    // TODO:
    // getFileManager(uri).signatureHelpStart = Some(pos)
  }

  def getSignatureHelpStart(uri: String): Option[Position] = {
    // TODO:
    // getFileManager(uri).signatureHelpStart
    None
  }

  // def sendDiags(uri: String, ver: Boolean, diags: Seq[Diagnostic]) = {
  //   getFileManager(uri)
  //     .foreach(_.sendDiags(ver, diags))
  // }

  /** Checks if verification can be started for a given file.
    *
    * Informs client differently depending on whether or not verification attempt was triggered manually
    * */
  def canVerificationBeStarted(uri: String, content: String, manuallyTriggered: Boolean): Boolean = {
    logger.trace("canVerificationBeStarted")
    if (server.isRunning) {
      logger.trace("server is running")
      // This should only be necessary if one wants to verify a closed file for some reason
      val fm = getFileManager(uri, Some(content))
      // This will be the new project root
      makeEmptyRoot(fm)
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
    val fm = getFileManager(uri)
    fm.stop()
      .map(_ => {
        logger.trace(s"stopVerification has completed for ${fm.file.uri}")
        val params = StateChangeParams(Ready.id, verificationCompleted = 0, verificationNeeded = 0, uri = uri)
        sendStateChangeNotification(params, Some(fm))
        true
      })
      .recover(_ => false)
  }

  /** Stops all running verifications.
    *
    * Returns a CF that is successfully completed if all running verifications were stopped
    * successfully. Otherwise, a failed CF is returned
    * */
  def stopAllRunningVerifications(): Future[Unit] = {
    val tasks = _files.values().asScala.map(fm =>
      fm.stop().map(_ => {
        logger.trace(s"stopVerification has completed for ${fm.file.uri}")
      }))
    Future.sequence(tasks).map(_ => {
      logger.debug("all running verifications have been stopped")
    })
  }

  /** returns true if verification was started */
  def startVerification(backendClassName: String, customArgs: String, uri: String, manuallyTriggered: Boolean): Future[Boolean] = {
    val fm = getFileManager(uri)
    fm.startVerification(backendClassName, customArgs, fm.content, manuallyTriggered)
  }

  /** returns true if parse/typecheck was started */
  def startParseTypecheck(uri: String): Boolean = {
    val fm = getFileManager(uri)
    val project = Option(_files.get(uri)).flatMap(_.projectRoot).getOrElse(uri)
    val root = getFileManager(project)
    root.runParseTypecheck(fm.content)
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
  def sendStateChangeNotification(params: StateChangeParams, task: Option[VerificationManager]): Unit = {
    // update file manager's state:
    task.foreach(vm => vm.state = VerificationState(params.newState))
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

  private def getFileManager(uri: String, content: Option[String] = None): FileManager = {
    var createdNew = false
    val coordinator = this
    val createFMFunc = new java.util.function.Function[String, FileManager]() {
      override def apply(t: String): FileManager = {
        logger.trace(s"FileManager created for $uri")
        createdNew = true
        FileManager(uri, coordinator, content)
      }
    }
    // we use `computeIfAbsent` instead of `putIfAbsent` such that a new FileManager is only created if it's absent
    val fm = _files.computeIfAbsent(uri, createFMFunc)
    // Override the content if we are given one and the file manager was not just created
    if (!createdNew && content.isDefined) fm.content.set(content.get)
    fm
  }
  def ensureFmExists(uri: String, content: String): FileManager = {
    getFileManager(uri, Some(content))
  }
  def getRoot(uri: String): FileManager = {
    val fm = getFileManager(uri)
    fm.projectRoot.map(getFileManager(_)).getOrElse(fm)
  }

  // def setSymbols(uri: String, symbs: Seq[lsp.DocumentSymbol]) = {
  //   getFileManager(uri).setSymbols(symbs)
  // }

  // def setHoverHints(uri: String, hints: Seq[HoverHint]) = {
  //   getFileManager(uri).setHoverHints(hints)
  // }

  // def setGotoDefinitions(uri: String, gotoDefns: Seq[GotoDefinition]) = {
  //   getFileManager(uri).setGotoDefinitions(gotoDefns)
  // }
  
  // def setFoldingRanges(uri: String, foldRanges: Seq[utility.lsp.FoldingRange]) = {
  //   getFileManager(uri).setFoldingRanges(foldRanges)
  // }

  // def setInlayHints(uri: String, inlayHints: Seq[utility.lsp.InlayHint]) = {
  //   getFileManager(uri).setInlayHints(inlayHints)
  // }

  // TODO:
  def inlayChosenTriggersAt(uri: String, start: RangePosition, triggers: Seq[Trigger], oldTriggers: Seq[Trigger]) = {
    // getFileManager(uri).inlayChosenTriggersAt(start, triggers, oldTriggers)
  }

  def setQIsInFile(uri: String, pos: RangePosition, instantiations: Int, maxGen: Int, maxCost: Int) = {
    // getFileManager(uri).setQIsInFile(pos, instantiations, maxGen, maxCost)
  }

  ///////////////////////
  // Project management
  ///////////////////////

  def addToProject(uri: String, root: String, getContents: Boolean): (Option[String], Option[Set[String]]) = {
    getFileManager(uri).addToProject(root, getContents)
  }
  def removeFromProject(uri: String, root: String) = {
    Option(_files.get(uri)).map(_.removeFromProject(root))
  }
  def makeEmptyRoot(fm: FileManager) = {
    for (leaves <- fm.projectLeaves; leaf <- leaves) {
      removeFromProject(leaf, fm.file_uri)
    }
    fm.project = Left(Map())
  }
  def handleChangeInLeaf(root: String, leaf: String, range: Range, text: String): Unit = {
    Option(_files.get(root)).map(_.handleChangeInLeaf(leaf, range, text))
  }
}
