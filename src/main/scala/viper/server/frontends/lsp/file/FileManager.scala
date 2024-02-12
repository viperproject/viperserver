// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2023 ETH Zurich.

package viper.server.frontends.lsp.file

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError, ErrorMessage}

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.concurrent.Future

case class PathInfo(file_uri: String) {
  val uri: URI = URI.create(file_uri)
  val path: Path = Paths.get(uri)
  val filename: String = path.getFileName.toString
}

// TODO: change this to contain a `MessageHandler` rather than be a
case class FileManager(file: PathInfo, coordinator: lsp.ClientCoordinator, content: lsp.file.FileContent)(implicit executor: VerificationExecutionContext) extends MessageHandler {
  override val ec: VerificationExecutionContext = executor
  // override val manager: FileManager = this

  // override def thisInFile(uri: String): FileManager = {
  //   val toGet = if (uri == file_uri) projectRoot else Some(uri)
  //   toGet.map(coordinator.getFileManager(_)).getOrElse(this)
  // }

  // Can force a refresh in the future if we get new ones, so return immediately
  def getCodeLens(uri: String): Future[Seq[lsp4j.CodeLens]] =
    Future.successful(getInProject(uri).getCodeLens())
  // Currently unused
  def getDiagnostics(uri: String): Future[Seq[lsp4j.Diagnostic]] =
    Future.successful(getInProject(uri).getDiagnostic())
  def getInlayHints(uri: String): Future[Seq[lsp4j.InlayHint]] =
    Future.successful(getInProject(uri).getInlayHint())
  def getSemanticHighlights(uri: String): Future[Seq[lsp.Lsp4jSemanticHighlight]] =
    Future.successful(getInProject(uri).getSemanticHighlight())

  // Even though we may be returning a stale request
  def getGotoDefinitions(uri: String, pos: lsp4j.Position): Future[Seq[lsp4j.LocationLink]] =
    getInFuture(getGotoDefinitionProject(uri, pos))
  def getHoverHints(uri: String, pos: lsp4j.Position): Future[Seq[lsp4j.Hover]] =
    Future.successful(getHoverHintProject(uri, pos))
  def getFindReferences(uri: String, pos: lsp4j.Position, includeDeclaration: Boolean, fromReferences: Boolean = true): Future[Seq[lsp4j.Location]] =
    getInFuture(getFindReferencesProject(uri, pos, includeDeclaration, fromReferences))
  def getRename(uri: String, pos: lsp4j.Position, newName: String): Future[lsp4j.WorkspaceEdit] =
    getInFuture({
      val references = getFindReferencesProject(uri, pos, true, true)
      getInProject(uri).getRename(references, newName).orNull
    })

  def getDocumentSymbols(uri: String): Future[Seq[lsp4j.DocumentSymbol]] =
    getInFuture(getInProject(uri).getDocumentSymbol())
  def getDocumentLinks(uri: String): Future[Seq[lsp4j.DocumentLink]] =
    getInFuture(getInProject(uri).getDocumentLink())
  def getFoldingRanges(uri: String): Future[Seq[lsp4j.FoldingRange]] =
    getInFuture(getInProject(uri).getFoldingRange())
  def getCompletionProposal(uri: String, pos: lsp4j.Position, char: Option[String]): Future[Seq[lsp4j.CompletionItem]] =
    getInFuture(getCompletionProposal(uri, pos, char, ()))
  // def getSignatureHelps(): Future[Seq[lsp4j.SignatureHelp]] = getInFuture(signatureHelp.get(())(coordinator.logger))

  var isOpen: Boolean = true

  // val dsm: DocumentSymbolManager = DocumentSymbolManager()
  val definitions: ArrayBuffer[lsp.Definition] = ArrayBuffer()
  val members: HashMap[(String, AbstractSourcePosition), ast.Declaration] = HashMap()

  // All files that are relevant to the verification of the current file
  var referencedFiles: Set[String] = Set()
  // val diagnostics: HashMap[String, ArrayBuffer[Diagnostic]] = HashMap()
  // def fileDiagnostics: Array[Diagnostic] = diagnostics.get(file.file_uri).map(_.toArray).getOrElse(Array())

  class FileManagerState {
    var lastSuccess: VerificationSuccess = NA
    var internalErrorMessage: String = ""

    //state specific to one verification
    var is_aborting: Boolean = false
    var is_verifying: Boolean = false
    var state: VerificationState = Stopped
    var manuallyTriggered: Boolean = _
    var canResetReferences: Boolean = false
    var gotNewMembers: Boolean = false

    //verification results
    var jid: Int = -1
    var timeMs: Long = 0
    var parsingCompleted: Boolean = false
    var typeCheckingCompleted: Boolean = false
    var progress: lsp.file.ProgressCoordinator = _
  }
  var data: FileManagerState = new FileManagerState()

  // def resetDiagnostics(): Unit = {
  //   val diagnosticParams = new PublishDiagnosticsParams()
  //   diagnosticParams.setDiagnostics(Seq().asJava)
  //   for (ref <- diagnostics) {
  //     diagnosticParams.setUri(ref._1.toString())
  //     coordinator.client.publishDiagnostics(diagnosticParams)
  //   }
  //   data.diagnosticCount = 0
  //   diagnostics.clear()
  // }

  /* Resets all getSymbols and definitions, only when not verifying */
  def resetFileInfo(): Unit = {
    if (!data.is_verifying) {
      members.clear()
      // symbolInformation.clear()
      definitions.clear()
      // foldingRanges.clear()
    }
  }

  // def handleChange(uri: String, range: Range, text: String): Unit = {
  //   handleChangeST(uri, range, text)
  // }

  def resetLastSuccess(): Unit = {
    data.lastSuccess = NA
  }

  // def receiveNode(node: ast.Node): Seq[DocumentSymbol] = {
  //   val subnodes = node.subnodes.flatMap(receiveNode)
  //   node match {
  //     case decl: ast.Declaration if decl.pos.isInstanceOf[AbstractSourcePosition] => {
  //       Seq(receiveDeclaration(decl, subnodes))
  //     }
  //     case _ => subnodes
  //   }
  // }
  // def receiveDeclaration(decl: ast.Declaration, children: Seq[DocumentSymbol]): DocumentSymbol = {
  //   val pos = decl.pos.asInstanceOf[AbstractSourcePosition]
  //   members += ((decl.name, pos) -> decl)
  //   val end = pos.end.getOrElse(pos.start)
  //   if (end.line - pos.start.line >= 3) {
  //     val key = pos.file.toUri().toString()
  //     val value = new FoldingRange(pos.start.line - 1, end.line - 1)
  //     foldingRanges.getOrElseUpdate(key, ArrayBuffer()) += value
  //   }
  //   val range_start = new Position(pos.start.line - 1, pos.start.column - 1)
  //   val range_end = new Position(end.line - 1, end.column - 1)
  //   val range = new Range(range_start, range_end)

  //   val kind = decl match {
  //     case _: ast.Method => SymbolKind.Method
  //     case _: ast.Function => SymbolKind.Function
  //     case _: ast.Predicate => SymbolKind.Interface
  //     case _: ast.Field => SymbolKind.Field
  //     case _: ast.Domain => SymbolKind.Class
  //     case _: ast.DomainAxiom => SymbolKind.Constant
  //     case _: ast.DomainFunc => SymbolKind.Function
  //     case _: ast.LocalVarDecl => SymbolKind.Variable
  //     case _: ast.ExtensionMember => SymbolKind.Enum
  //     case _ => SymbolKind.Null
  //   }

  //   val detail = decl match {
  //     case typ: ast.Typed => typ.typ.toString()
  //     case _ => null
  //   }
  //   // for now, we use `range` as range & selectionRange. The latter one is supposed to be only a sub-range
  //   // that should be selected when the user selects the symbol.
  //   new DocumentSymbol(decl.name, kind, range, range, detail, children.asJava)
  // }

  // def receiveDefinition(d: Definition): Unit = {
  //   val start = d.scope match {
  //     case Some(s) => new Position(s.start.line - 1, s.start.column - 1)
  //     case _ => null
  //   }
  //   val end = d.scope match {
  //     case Some(s) if s.end.isDefined => new Position(s.end.get.line - 1, s.end.get.column - 1)
  //     case _ => null
  //   }
  //   val range: Range = if(start != null && end != null) {
  //     new Range(start, end)
  //   } else {
  //     null
  //   }
  //   val sourcePos = d.location.asInstanceOf[AbstractSourcePosition]
  //   val sourcePosEnd = sourcePos.end.getOrElse(sourcePos.start)
  //   val startPos: Position = new Position(sourcePos.start.line - 1, sourcePos.start.column - 1)
  //   val endPos: Position = new Position(sourcePosEnd.line - 1, sourcePosEnd.column - 1)
  //   val codePos: Range = new Range(startPos, endPos)
  //   val name = d.name
  //   val path = sourcePos.file.toUri().toString()
  //   val hover = (d.typ, members.get(name, sourcePos)) match {
  //     case (ViperAxiom, _) => s"axiom $name"
  //     case (ViperDomain, Some(value: ast.Domain)) => s"domain $name${value.typVars.mkString("[", ", ", "]")}"
  //     case (ViperDomain, _) => s"domain $name"
  //     case (ViperPredicate, Some(value: ast.Predicate)) => {
  //       val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
  //       if (value.body.isDefined) s"$sig\n{ ... }" else sig
  //     }
  //     case (ViperPredicate, _) => s"predicate $name"
  //     case (ViperMethod, Some(value: ast.Method)) => {
  //       val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
  //       if (value.body.isDefined) s"$sig\n{ ... }" else sig
  //     }
  //     case (ViperMethod, _) => s"method $name"
  //     case (ViperFunction, Some(value: ast.Function)) => {
  //       val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
  //       if (value.body.isDefined) s"$sig\n{ ... }" else sig
  //     }
  //     case (ViperFunction, Some(value: ast.DomainFunc)) => value.toString().trim()
  //     case (ViperFunction, _) => s"function $name"
  //     case (ViperField(viperType), _) => s"field $name: $viperType"
  //     case (ViperArgument(viperType), _) => s"$name: $viperType"
  //     case (ViperReturnParameter(viperType), _) => s"returns $name: $viperType"
  //     case (ViperUntypedLocalDefinition, _) => s"var $name"
  //     case (ViperTypedLocalDefinition(viperType), _) => s"var $name: $viperType"
  //   }
  //   def argsToIdx(start: Int, formalArgs: Seq[ast.AnyLocalVarDecl]): (String, Seq[ParameterInformation]) = {
  //       val args = formalArgs.map {
  //         case arg: ast.LocalVarDecl => s"${arg.name}: ${arg.typ}"
  //         case arg => s"${arg.typ}"
  //       }
  //       var idx = start
  //       val offsets = args.map(arg => {
  //         val start = idx
  //         idx += arg.length() + 2
  //         val paramInfo = new ParameterInformation()
  //         paramInfo.setLabel(new Two[Integer,Integer](start, idx-2))
  //         paramInfo
  //       })
  //       (args.mkString(", "), offsets)
  //   }
  //   val signatureHelp = (d.typ, members.get(name, sourcePos)) match {
  //     case (ViperMethod, Some(value: ast.Method)) => {
  //       val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
  //       val returns = value.formalReturns.map(arg => s"${arg.name}: ${arg.typ}").mkString(", ")
  //       Some(lsp.SignatureHelp(s"${value.name}($args) returns ($returns)", offsets))
  //     }
  //     case (ViperFunction, Some(value: ast.Function)) => {
  //       val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
  //       Some(lsp.SignatureHelp(s"${value.name}($args): ${value.typ}", offsets))
  //     }
  //     case (ViperFunction, Some(value: ast.DomainFunc)) => {
  //       val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
  //       Some(lsp.SignatureHelp(s"${value.name}($args): ${value.typ}", offsets))
  //     }
  //     case (ViperPredicate, Some(value: ast.Predicate)) => {
  //       val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
  //       Some(lsp.SignatureHelp(s"${value.name}($args)", offsets))
  //     }
  //     case _ => None
  //   }
  //   val definition: lsp.Definition = lsp.Definition(d.typ, name, path, hover, codePos, range, signatureHelp)
  //   definitions.append(definition)
  // }

  //////////////////
  // Verification //
  //////////////////

  // def prepareVerification(): Unit = {
  //   data.is_verifying = true
  //   data.is_aborting = false
  //   data.gotNewMembers = false
  //   data.state = Stopped
  //   data.timeMs = 0
  //   resetDiagnostics()
  //   data.parsingCompleted = true
  //   data.typeCheckingCompleted = true
  //   data.internalErrorMessage = ""
  //   // Do not reset them just yet: parsing might fail
  //   // in which case we should keep the old ones
  //   data.canResetReferences = true
  // }

  // def stopParseTypecheck(): Future[Unit] = {
  //   coordinator.logger.trace(s"stop verification of $file_uri")
  //   if (!data.is_verifying) {
  //     coordinator.logger.trace(s"verification of $file_uri did not have to be stopped because there is no ongoing verification")
  //     return Future.unit
  //   }
  //   coordinator.logger.info("Aborting running verification.")
  //   data.is_aborting = true
  //   coordinator.server.stopAstConstruction(AstJobId(data.jid), Some(coordinator.localLogger)).transform(
  //     _ => {
  //       data.is_verifying = false
  //       data.lastSuccess = Aborted
  //     },
  //     e => {
  //       coordinator.logger.debug(s"Error aborting verification of $filename: $e")
  //       e
  //     }
  //   )
  // }

  // def stopVerification(): Future[Unit] = {
  //   coordinator.logger.trace(s"stop verification of $file_uri")
  //   if (!data.is_verifying) {
  //     coordinator.logger.trace(s"verification of $file_uri did not have to be stopped because there is no ongoing verification")
  //     return Future.unit
  //   }
  //   coordinator.logger.info("Aborting running verification.")
  //   data.is_aborting = true
  //   coordinator.server.stopVerification(VerJobId(data.jid), Some(coordinator.localLogger)).transform(
  //     _ => {
  //       data.is_verifying = false
  //       data.lastSuccess = Aborted
  //     },
  //     e => {
  //       coordinator.logger.debug(s"Error aborting verification of $filename: $e")
  //       e
  //     }
  //   )
  // }

  // /** the file that should be verified has to already be part of `customArgs` */
  // def getVerificationCommand(backendClassName: String, customArgs: String): String = {
  //   if (backendClassName != "silicon" && backendClassName != "carbon") {
  //     throw new Error(s"Invalid verification backend value. " +
  //       s"Possible values are [silicon | carbon] " +
  //       s"but found $backendClassName")
  //   }
  //   s"$backendClassName $customArgs"
  // }

  // /** Run parsing and typechecking but no verification */
  // def startParseTypecheck(loader: FileContent): Boolean = {
  //   // TODO:
  //   prepareVerification()

  //   coordinator.logger.info(s"parse/typecheck $filename")

  //   // val params = StateChangeParams(VerificationRunning.id, filename = filename)
  //   // coordinator.sendStateChangeNotification(params, Some(this))

  //   val handle = coordinator.server.parseTypecheckFile(path.toString(), Some(coordinator.localLogger), Some(loader))
  //   data.jid = handle.id
  //   if (data.jid >= 0) {
  //     coordinator.server.startStreamingAst(handle, RelayActor.props(this, None), Some(coordinator.localLogger))
  //     true
  //   } else {
  //     false
  //   }
  // }

  // def startVerification(backendClassName: String, customArgs: String, manuallyTriggered: Boolean): Boolean = {
  //   prepareVerification()
  //   this.data.manuallyTriggered = manuallyTriggered

  //   coordinator.logger.info(s"verify $filename")

  //   val params = lsp.StateChangeParams(VerificationRunning.id, filename = filename)
  //   coordinator.sendStateChangeNotification(params, Some(this))

  //   val command = getVerificationCommand(backendClassName, customArgs)
  //   coordinator.logger.debug(s"verification command: $command")
  //   val handle = coordinator.server.verifyWithCommand(command, Some(coordinator.localLogger))
  //   data.jid = handle.id
  //   if (data.jid >= 0) {
  //     coordinator.server.startStreaming(handle, RelayActor.props(this, Some(backendClassName)), Some(coordinator.localLogger))
  //     true
  //   } else {
  //     false
  //   }
  // }
}
  
object FileManager {
  def apply(file_uri: String, coordinator: lsp.ClientCoordinator, c: Option[String])(implicit executor: VerificationExecutionContext): FileManager = {
    val file = PathInfo(file_uri)
    // In some cases the IDE doesn't send the content to use, then load it ourselves
    val fileContent = c.getOrElse(ast.utility.DiskLoader.loadContent(file.path).get)
    val content = lsp.file.FileContent(file.path, fileContent)
    new FileManager(file, coordinator, content)
  }
}
