// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2023 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import akka.actor.{Actor, Props, Status}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentSymbol, FoldingRange, Position, PublishDiagnosticsParams, Range, SymbolKind}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.server.frontends.lsp.VerificationState._
import viper.server.frontends.lsp.VerificationSuccess._
import viper.server.vsi.VerJobId
import viper.silver.ast
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}
import viper.silver.reporter._
import viper.silver.verifier.{AbortedExceptionally, AbstractError, ErrorMessage}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.concurrent.Future
import org.eclipse.lsp4j.ParameterInformation
import org.eclipse.lsp4j.jsonrpc.messages.Tuple.Two

case class FileManager(coordinator: ClientCoordinator, file_uri: String)(implicit executor: VerificationExecutionContext) {
  // File information
  val uri: URI = URI.create(file_uri)
  val path: Path = Paths.get(uri)
  val filename: String = path.getFileName.toString

  val dsm: DocumentSymbolManager = DocumentSymbolManager()
  val definitions: ArrayBuffer[lsp.Definition] = ArrayBuffer()
  val members: HashMap[(String, AbstractSourcePosition), ast.Declaration] = HashMap()
  val foldingRanges: HashMap[String, ArrayBuffer[FoldingRange]] = HashMap()
  val semanticTokens: HashMap[String, ArrayBuffer[lsp.SemanticToken]] = HashMap()
  var signatureHelp: Option[Position] = None
  // All files that are relevant to the verification of the current file
  var referencedFiles: Set[String] = Set()
  val diagnostics: HashMap[String, ArrayBuffer[Diagnostic]] = HashMap()
  def fileDiagnostics: Array[Diagnostic] = diagnostics.get(file_uri).map(_.toArray).getOrElse(Array())

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
    // does not correspond to `diagnostics.size` when
    // there are errors in other files
    var diagnosticCount: Int = 0

    //verification results
    var jid: Int = -1
    var timeMs: Long = 0
    var parsingCompleted: Boolean = false
    var typeCheckingCompleted: Boolean = false
    var progress: ProgressCoordinator = _
  }
  var data: FileManagerState = new FileManagerState()

  def resetDiagnostics(): Unit = {
    val diagnosticParams = new PublishDiagnosticsParams()
    diagnosticParams.setDiagnostics(Seq().asJava)
    for (ref <- diagnostics) {
      diagnosticParams.setUri(ref._1.toString())
      coordinator.client.publishDiagnostics(diagnosticParams)
    }
    data.diagnosticCount = 0
    diagnostics.clear()
  }

  /* Resets all symbols and definitions, only when not verifying */
  def resetFileInfo(): Unit = {
    if (!data.is_verifying) {
      members.clear()
      dsm.symbolInformation.clear()
      definitions.clear()
      foldingRanges.clear()
    }
  }

  def handleChange(uri: String, range: Range, text: String): Unit = {
    val lines = text.split("\n", -1)
    val deltaLines = lines.length - 1 + range.getStart.getLine - range.getEnd.getLine
    val startCharacter = if (lines.length == 1) range.getStart.getCharacter else 0
    val deltaChars = startCharacter + lines.last.length - range.getEnd.getCharacter
    // If the change cannot ruin the meaning of a semantic token at the start,
    // adjust the range start to avoid overlaps with adjacent tokens
    if (text.isEmpty || !text.head.isLetterOrDigit) {
      range.getStart.setCharacter(range.getStart.getCharacter + 1)
    }
    // If the change cannot ruin the meaning of a semantic token at the end,
    // adjust the range end to avoid overlaps with adjacent tokens
    if (text.isEmpty || !text.last.isLetterOrDigit) {
      range.getEnd.setCharacter(range.getEnd.getCharacter - 1)
    }
    // Remove overlapping semantic tokens and update positions of those after change
    semanticTokens.get(uri).map(tokens => {
      tokens.filterInPlace(token => {
        val cmp = token.compare(range)
        if (cmp == 1) {
          // Token after change so may have moved
          if (token.start.getLine == range.getEnd.getLine) {
            token.start.setCharacter(token.start.getCharacter + deltaChars)
          }
          token.start.setLine(token.start.getLine + deltaLines)
        }
        // Remove overlaps
        cmp != 0
      })
    })
  }

  def resetLastSuccess(): Unit = {
    data.lastSuccess = NA
  }

  def receiveNode(node: ast.Node): Seq[DocumentSymbol] = {
    val subnodes = node.subnodes.flatMap(receiveNode)
    node match {
      case decl: ast.Declaration if decl.pos.isInstanceOf[AbstractSourcePosition] => {
        Seq(receiveDeclaration(decl, subnodes))
      }
      case _ => subnodes
    }
  }
  def receiveDeclaration(decl: ast.Declaration, children: Seq[DocumentSymbol]): DocumentSymbol = {
    val pos = decl.pos.asInstanceOf[AbstractSourcePosition]
    members += ((decl.name, pos) -> decl)
    val end = pos.end.getOrElse(pos.start)
    if (end.line - pos.start.line >= 3) {
      val key = pos.file.toUri().toString()
      val value = new FoldingRange(pos.start.line - 1, end.line - 1)
      foldingRanges.getOrElseUpdate(key, ArrayBuffer()) += value
    }
    val range_start = new Position(pos.start.line - 1, pos.start.column - 1)
    val range_end = new Position(end.line - 1, end.column - 1)
    val range = new Range(range_start, range_end)

    val kind = decl match {
      case _: ast.Method => SymbolKind.Method
      case _: ast.Function => SymbolKind.Function
      case _: ast.Predicate => SymbolKind.Interface
      case _: ast.Field => SymbolKind.Field
      case _: ast.Domain => SymbolKind.Class
      case _: ast.DomainAxiom => SymbolKind.Constant
      case _: ast.DomainFunc => SymbolKind.Function
      case _: ast.LocalVarDecl => SymbolKind.Variable
      case _: ast.ExtensionMember => SymbolKind.Enum
      case _ => SymbolKind.Null
    }

    val detail = decl match {
      case typ: ast.Typed => typ.typ.toString()
      case _ => null
    }
    // for now, we use `range` as range & selectionRange. The latter one is supposed to be only a sub-range
    // that should be selected when the user selects the symbol.
    new DocumentSymbol(decl.name, kind, range, range, detail, children.asJava)
  }

  def receiveDefinition(d: Definition): Unit = {
    val start = d.scope match {
      case Some(s) => new Position(s.start.line - 1, s.start.column - 1)
      case _ => null
    }
    val end = d.scope match {
      case Some(s) if s.end.isDefined => new Position(s.end.get.line - 1, s.end.get.column - 1)
      case _ => null
    }
    val range: Range = if(start != null && end != null) {
      new Range(start, end)
    } else {
      null
    }
    val sourcePos = d.location.asInstanceOf[AbstractSourcePosition]
    val sourcePosEnd = sourcePos.end.getOrElse(sourcePos.start)
    val startPos: Position = new Position(sourcePos.start.line - 1, sourcePos.start.column - 1)
    val endPos: Position = new Position(sourcePosEnd.line - 1, sourcePosEnd.column - 1)
    val codePos: Range = new Range(startPos, endPos)
    val name = d.name
    val path = sourcePos.file.toUri().toString()
    val hover = (d.typ, members.get(name, sourcePos)) match {
      case (ViperAxiom, _) => s"axiom $name"
      case (ViperDomain, Some(value: ast.Domain)) => s"domain $name${value.typVars.mkString("[", ", ", "]")}"
      case (ViperDomain, _) => s"domain $name"
      case (ViperPredicate, Some(value: ast.Predicate)) => {
        val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
        if (value.body.isDefined) s"$sig\n{ ... }" else sig
      }
      case (ViperPredicate, _) => s"predicate $name"
      case (ViperMethod, Some(value: ast.Method)) => {
        val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
        if (value.body.isDefined) s"$sig\n{ ... }" else sig
      }
      case (ViperMethod, _) => s"method $name"
      case (ViperFunction, Some(value: ast.Function)) => {
        val sig = value.copy(body = None)(value.pos, value.info, value.errT).toString().trim()
        if (value.body.isDefined) s"$sig\n{ ... }" else sig
      }
      case (ViperFunction, Some(value: ast.DomainFunc)) => value.toString().trim()
      case (ViperFunction, _) => s"function $name"
      case (ViperField(viperType), _) => s"field $name: $viperType"
      case (ViperArgument(viperType), _) => s"$name: $viperType"
      case (ViperReturnParameter(viperType), _) => s"returns $name: $viperType"
      case (ViperUntypedLocalDefinition, _) => s"var $name"
      case (ViperTypedLocalDefinition(viperType), _) => s"var $name: $viperType"
    }
    def argsToIdx(start: Int, formalArgs: Seq[ast.AnyLocalVarDecl]): (String, Seq[ParameterInformation]) = {
        val args = formalArgs.map {
          case arg: ast.LocalVarDecl => s"${arg.name}: ${arg.typ}"
          case arg => s"${arg.typ}"
        }
        var idx = start
        val offsets = args.map(arg => {
          val start = idx
          idx += arg.length() + 2
          val paramInfo = new ParameterInformation()
          paramInfo.setLabel(new Two[Integer,Integer](start, idx-2))
          paramInfo
        })
        (args.mkString(", "), offsets)
    }
    val signatureHelp = (d.typ, members.get(name, sourcePos)) match {
      case (ViperMethod, Some(value: ast.Method)) => {
        val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
        val returns = value.formalReturns.map(arg => s"${arg.name}: ${arg.typ}").mkString(", ")
        Some(SignatureHelp(s"${value.name}($args) returns ($returns)", offsets))
      }
      case (ViperFunction, Some(value: ast.Function)) => {
        val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
        Some(SignatureHelp(s"${value.name}($args): ${value.typ}", offsets))
      }
      case (ViperFunction, Some(value: ast.DomainFunc)) => {
        val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
        Some(SignatureHelp(s"${value.name}($args): ${value.typ}", offsets))
      }
      case (ViperPredicate, Some(value: ast.Predicate)) => {
        val (args, offsets) = argsToIdx(value.name.length() + 1, value.formalArgs)
        Some(SignatureHelp(s"${value.name}($args)", offsets))
      }
      case _ => None
    }
    val definition: lsp.Definition = lsp.Definition(d.typ, name, path, hover, codePos, range, signatureHelp)
    definitions.append(definition)
  }

  def receiveSemanticTokens(tokens: List[ast.utility.SemanticHighlight]) = {
    semanticTokens.clear()
    for (t@ast.utility.SemanticHighlight(start, end, typ, modifiers) <- tokens) {
      if (start.line != end.line) {
        coordinator.logger.error(s"Multiline semantic tokens are not supported: ${t.toString}.")
      } else {
        val file = start.file.toUri().toString()
        val tokenArray = semanticTokens.getOrElseUpdate(file, ArrayBuffer())

        val mod = modifiers.foldLeft(0)((acc, m) => acc | (1 << m.id))
        val startPos = new Position(start.line - 1, start.column - 1)
        val semToken = lsp.SemanticToken(startPos, end.column - start.column, typ.id, mod)
        tokenArray += semToken
      }
    }
    semanticTokens.values.foreach(_.sortInPlaceBy(t => (t.start.getLine, t.start.getCharacter)))
    semanticTokens.values.foreach(tokenArray => {
      var (prevLine, prevColumn) = (0, 0)
      tokenArray.filterInPlace(token => {
        val remove = token.start.getLine <= prevLine && token.start.getCharacter <= prevColumn
        prevLine = token.start.getLine
        prevColumn = token.start.getCharacter + token.len
        if (remove) {
          coordinator.logger.error(s"Overlapping semantic tokens are not supported: ${token.toString}.")
        }
        !remove
      })
    })
    coordinator.client.refreshSemanticTokens()
  }

  //////////////////
  // Verification //
  //////////////////

  def prepareVerification(): Unit = {
    data.is_verifying = true
    data.is_aborting = false
    data.gotNewMembers = false
    data.state = Stopped
    data.timeMs = 0
    resetDiagnostics()
    data.parsingCompleted = true
    data.typeCheckingCompleted = true
    data.internalErrorMessage = ""
    // Do not reset them just yet: parsing might fail
    // in which case we should keep the old ones
    data.canResetReferences = true
  }

  def stopVerification(): Future[Unit] = {
    coordinator.logger.trace(s"stop verification of $file_uri")
    if (!data.is_verifying) {
      coordinator.logger.trace(s"verification of $file_uri did not have to be stopped because there is no ongoing verification")
      return Future.unit
    }
    coordinator.logger.info("Aborting running verification.")
    data.is_aborting = true
    coordinator.server.stopVerification(VerJobId(data.jid), Some(coordinator.localLogger)).transform(
      _ => {
        data.is_verifying = false
        data.lastSuccess = Aborted
      },
      e => {
        coordinator.logger.debug(s"Error aborting verification of $filename: $e")
        e
      }
    )
  }

  /** the file that should be verified has to already be part of `customArgs` */
  def getVerificationCommand(backendClassName: String, customArgs: String): String = {
    if (backendClassName != "silicon" && backendClassName != "carbon") {
      throw new Error(s"Invalid verification backend value. " +
        s"Possible values are [silicon | carbon] " +
        s"but found $backendClassName")
    }
    s"$backendClassName $customArgs"
  }

  def startVerification(backendClassName: String, customArgs: String, manuallyTriggered: Boolean): Boolean = {
    prepareVerification()
    this.data.manuallyTriggered = manuallyTriggered

    coordinator.logger.info(s"verify $filename")

    val params = StateChangeParams(VerificationRunning.id, filename = filename)
    coordinator.sendStateChangeNotification(params, Some(this))

    val command = getVerificationCommand(backendClassName, customArgs)
    coordinator.logger.debug(s"verification command: $command")
    val handle = coordinator.server.verifyWithCommand(command, Some(coordinator.localLogger))
    data.jid = handle.id
    if (data.jid >= 0) {
      coordinator.server.startStreaming(handle, RelayActor.props(this, backendClassName), Some(coordinator.localLogger))
      true
    } else {
      false
    }
  }

  object RelayActor {
    def props(task: FileManager, backendClassName: String): Props = Props(new RelayActor(task, backendClassName))

    case class GetReportedErrors()
  }

  class RelayActor(task: FileManager, backendClassName: String) extends Actor {

    /**
      * errors together with the offending node's position (only for errors that have an offending node (i.e. implement
      * `ErrorMessage`). Storing the position along with the error fixes the issue that two errors with offending nodes
      * at different position just get dropped.
      */
    var reportedErrors: Set[(AbstractError, Option[ast.Position])] = Set.empty
    /** helper function to add an error to `reportedErrors` */
    private def markErrorAsReported(err: AbstractError): Unit = err match {
      case err: ErrorMessage => reportedErrors = reportedErrors + ((err, Some(err.offendingNode.pos)))
      case _ => reportedErrors = reportedErrors + ((err, None))
    }
    /** helper function to add errors to `reportedErrors` */
    private def markErrorsAsReported(errs: Seq[AbstractError]): Unit = errs.foreach(markErrorAsReported)
    /** helper function to check whether an error is contained in `reportedErrors` */
    private def hasAlreadyBeenReported(err: AbstractError): Boolean = err match {
      case err: ErrorMessage => reportedErrors.contains((err, Some(err.offendingNode.pos)))
      case _ => reportedErrors.contains((err, None))
    }

    override def receive: PartialFunction[Any, Unit] = {
      case m if data.is_aborting =>
        coordinator.logger.debug(s"ignoring message because we are aborting: $m")

      case ProgramImportsReport(imports) =>
        // New project
        coordinator.logger.debug(s"got new imports for $filename: ${imports.toString()}")
        val files = imports.map(i => i.file.toUri().toString()).toSet
        if (files != referencedFiles) {
          for (uri <- referencedFiles) {
            coordinator.removeFromProject(uri)
          }
          referencedFiles = files
          coordinator.setupProject(file_uri, referencedFiles.toArray)
        }
        // Update symbols
        dsm.symbolInformation.clear()
        for (i <- imports) {
          val key = i.from.file.toUri().toString()
          dsm.symbolInformation.getOrElseUpdate(key, ArrayBuffer()) += ImportMember(i)
        }
      case SemanticTokensReport(tokens) =>
        coordinator.logger.debug(s"got semantic tokens for $filename: ${tokens.toString()}")
        receiveSemanticTokens(tokens)
      case ProgramOutlineReport(newMembers) =>
        data.gotNewMembers = true
        members.clear()
        for (member <- newMembers) {
          // Traverse all nodes
          val newSymbols = receiveNode(member).map(DocumentSymbolMember)
          val key = member.pos.asInstanceOf[AbstractSourcePosition].file.toUri().toString()
          dsm.symbolInformation.getOrElseUpdate(key, ArrayBuffer()) ++= newSymbols
        }
      case ProgramDefinitionsReport(defs) =>
        definitions.clear()
        assert(data.gotNewMembers, "got definitions before members")
        defs.foreach(receiveDefinition)
      case StatisticsReport(m, f, p, _, _) =>
        data.progress = new ProgressCoordinator(coordinator, p, f, m)
        val params = StateChangeParams(VerificationRunning.id, progress = 0, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(task))
      case AstConstructionFailureMessage(_, res) =>
        markErrorsAsReported(res.errors)
        processErrors(backendClassName, res.errors, Some("Constructing the AST has failed:"))
      case ExceptionReport(e) =>
        processErrors(backendClassName, Seq(AbortedExceptionally(e)))
      case InvalidArgumentsReport(_, errors) =>
        markErrorsAsReported(errors)
        processErrors(backendClassName, errors, Some(s"Invalid arguments have been passed to the backend $backendClassName:"))
      case EntitySuccessMessage(_, concerning, _, _) =>
        if (data.progress == null) {
          coordinator.logger.debug("The backend must send a VerificationStart message before the ...Verified message.")
        } else {
          val output = BackendOutput(BackendOutputType.FunctionVerified, name = concerning.name)
          data.progress.updateProgress(output)
          val progressPercentage = data.progress.toPercent
          val params = StateChangeParams(VerificationRunning.id, progress = progressPercentage, filename = filename)
          coordinator.sendStateChangeNotification(params, Some(task))
        }
      case EntityFailureMessage(_, _, _, res, _) =>
        markErrorsAsReported(res.errors)
        processErrors(backendClassName, res.errors)
      case OverallSuccessMessage(_, verificationTime) =>
        data.state = VerificationReporting
        data.timeMs = verificationTime
        // the completion handler is not yet invoked (but as part of Status.Success)
      case OverallFailureMessage(_, verificationTime, failure) =>
        data.state = VerificationReporting
        data.timeMs = verificationTime
        // we filter `failure.errors` such that only new errors are reported
        // this is important since Silicon provides errors as part of EntityFailureMessages and the OverallFailureMessage
        // where else Carbon does not produce EntityFailureMessages (except for cached members in which case
        // ViperServer produces EntitySuccessMessage and EntityFailureMessages)
        val newErrors = failure.errors.filterNot(hasAlreadyBeenReported)
        markErrorsAsReported(newErrors)
        processErrors(backendClassName, newErrors)
      // the completion handler is not yet invoked (but as part of Status.Success)
      case m: Message => coordinator.client.notifyUnhandledViperServerMessage(UnhandledViperServerMessageTypeParams(m.name, m.toString, LogLevel.Info.id))
      case Status.Success =>
        // Success is sent when the stream is completed
        completionHandler(0)
      case Status.Failure(cause) =>
        coordinator.logger.info(s"Streaming messages has failed in RelayActor with cause $cause")
        completionHandler(-1) // no success
      case RelayActor.GetReportedErrors() => sender() ! reportedErrors.toSeq.map(_._1)
      case e: Throwable => coordinator.logger.debug(s"RelayActor received throwable: $e")
    }

    private def processErrors(backendClassName: String, errors: Seq[AbstractError], errorMsgPrefix: Option[String] = None): Unit = {
      data.diagnosticCount += errors.size
      for (err <- errors) {
        var errorType: String = ""
        if (err.fullId != null && err.fullId == "typechecker.error") {
          data.typeCheckingCompleted = false
          errorType = "Typechecker error"
        } else if (err.fullId != null && err.fullId == "parser.error") {
          data.parsingCompleted = false
          data.typeCheckingCompleted = false
          errorType = "Parser error"
        } else {
          errorType = "Verification error"
        }

        val range = {
          val startPos = err.pos match {
            case p: HasLineColumn => new Position(p.line - 1, p.column - 1) // in case of SourcePosition, this indeed corresponds to the start position
            case _ => new Position(0, 0)
          }
          val endPos = err.pos match {
            // returning `startPos` instead of (0, 0) ensures that we do not create an end position that occurs before the start position
            case sp: AbstractSourcePosition => sp.end.map(end => new Position(end.line - 1, end.column - 1)).getOrElse(startPos)
            case _ => startPos
          }
          new Range(startPos, endPos)
        }
        val file = err.pos match {
          case sp: AbstractSourcePosition => sp.file.toUri()
          case _ => path.toUri()
        }

        val errMsgPrefixWithWhitespace = errorMsgPrefix.map(s => s"$s ").getOrElse("")
        val errFullId = if(err.fullId != null) s"[${err.fullId}] " else ""
        coordinator.logger.debug(s"$errorType: [$backendClassName] $errFullId" +
          s"${range.getStart.getLine + 1}:${range.getStart.getCharacter + 1} $errMsgPrefixWithWhitespace${err.readableMessage}s")


        val cachFlag: String = if(err.cached) "(cached)" else ""
        val diagnostic = new Diagnostic(range, errMsgPrefixWithWhitespace + err.readableMessage + cachFlag, DiagnosticSeverity.Error, "")
        val key = file.toString()
        diagnostics.getOrElseUpdate(key, ArrayBuffer()) += diagnostic
      }
      diagnostics.foreach { case (file, diags) =>
        val params = StateChangeParams(
          VerificationRunning.id, filename = Paths.get(file).getFileName.toString,
          uri = file.toString(), diagnostics = diags.toArray)
        coordinator.sendStateChangeNotification(params, Some(task))
      }
    }
  }

  private def determineSuccess(code: Int): VerificationSuccess = {
    if (code != 0) {
      Error
    } else if (!data.parsingCompleted) {
      ParsingFailed
    } else if (!data.typeCheckingCompleted) {
      TypecheckingFailed
    } else if (data.is_aborting) {
      Aborted
    } else if (data.diagnosticCount != 0) {
      VerificationFailed
    } else {
      VerificationSuccess.Success
    }
  }

  private def completionHandler(code: Int): Unit = {
    data.is_verifying = false
    try {
      coordinator.logger.debug(s"completionHandler is called with code $code")

      var params: StateChangeParams = null
      var success = NA
      val mt = if (this.data.manuallyTriggered) 1 else 0

      val isVerifyingStage = true

      // do we need to start a followUp Stage?
      if (isVerifyingStage) {
        // inform client about postProcessing
        success = determineSuccess(code)
        params = StateChangeParams(PostProcessing.id, filename = filename)
        coordinator.sendStateChangeNotification(params, Some(this))

        // notify client about outcome of verification
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = data.timeMs.toDouble / 1000, verificationCompleted = 1, uri = file_uri,
          error = data.internalErrorMessage, diagnostics = fileDiagnostics)
        coordinator.sendStateChangeNotification(params, Some(this))

        if (code != 0 && code != 1 && code != 899) {
          coordinator.logger.debug(s"Verification Backend Terminated Abnormally: with code $code")
        }
      } else {
        success = if (data.is_aborting) Aborted else Success
        params = StateChangeParams(Ready.id, success = success.id, manuallyTriggered = mt,
          filename = filename, time = data.timeMs.toDouble / 1000, verificationCompleted = 0, uri = file_uri,
          error = data.internalErrorMessage, diagnostics = fileDiagnostics)
        coordinator.sendStateChangeNotification(params, Some(this))
      }

      // reset for next verification
      data.lastSuccess = success
      data.timeMs = 0
    } catch {
      case e: Throwable =>
        data.is_verifying = false
        coordinator.client.notifyVerificationNotStarted(VerificationNotStartedParams(file_uri))
        coordinator.logger.debug(s"Error handling verification completion: $e")
    }
  }
}
