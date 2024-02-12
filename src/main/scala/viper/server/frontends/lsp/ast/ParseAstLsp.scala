// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.silver.parser

import viper.silver.ast.utility.lsp._
import viper.silver.ast.LineColumnPosition
import viper.silver.parser.PStringLiteral
import viper.silver.plugin.standard.adt._

object HasCodeLens {
  def apply(p: PProgram): Seq[CodeLens] = p.deepCollect(PartialFunction.empty).flatten
}

object HasDocumentSymbol {
  def apply(p: PNode): Seq[DocumentSymbol] = {
    val getSymbol = PartialFunction.fromFunction(HasDocumentSymbol.getSymbol(_))
    val get = { case Some(s) => s }: PartialFunction[Option[DocumentSymbol], DocumentSymbol]
    p.subnodes flatMap (_ shallowCollect (getSymbol andThen get))
  }

  def getSymbol(p: PNode): Option[DocumentSymbol] = p match {
    case p: PDeclaration => PLspDeclaration.getSymbol(p)
    case p: PImport => PLspImport.getSymbol(p)
    case _ => None
  }
}

object HasHoverHints {
  def apply(p: PProgram): Seq[HoverHint] = p.deepCollect({
    case n: PIdnUse => PLspIdnUse.getHoverHints(n)
    case n: PExp => PLspExp.getHoverHints(n)
    case n: PDeclaration => PLspDeclaration.getHoverHints(n)
    case n: PReserved[_] => PLspReserved.getHoverHints(n)
  }).flatten
}

object HasGotoDefinitions {
  def apply(p: PProgram): Seq[GotoDefinition] = p.deepCollect({
    case n: PIdnUse => PLspIdnUse.getGotoDefinitions(n)
    case n: PDeclaration => PLspDeclaration.getGotoDefinitions(n)
  }).flatten
}

object HasReferenceTos {
  def apply(p: PProgram): Seq[ReferenceTo] = p.deepCollect({
    case n: PIdnUse => PLspIdnUse.getReferenceTos(n)
    case n: PDeclaration => PLspDeclaration.getReferenceTos(n)
  }).flatten
}

object HasFoldingRanges {
  def apply(p: PProgram): Seq[FoldingRange] = p.deepCollect({
    case c: PCallable =>
      val thisRange = RangePosition(c).filter(rp => rp.start.line != rp._end.line)
      val sameStart = thisRange.zip(c.body.flatMap(RangePosition(_))).exists(rps => rps._1.start.line == rps._2.start.line)
      if (sameStart) Nil else thisRange.map(FoldingRange(_)).toSeq
    case d: PGrouped[_, _] if d.l.rs.isInstanceOf[PSym.Brace#L] && d.r.rs.isInstanceOf[PSym.Brace#R] =>
      RangePosition(d).map(FoldingRange(_)).toSeq
  }).flatten
}

object HasInlayHints {
  def apply(p: PProgram): Seq[InlayHint] = p.deepCollect({
    case n: PCall => PLspCall.getInlayHints(n)
    case n: PLet => PLspLet.getInlayHints(n)
  }).flatten
}

object HasSemanticHighlights {
  def apply(p: PProgram): Seq[SemanticHighlight] = p.deepCollect({
    case n: PAnnotation => PLspAnnotation.getSemanticHighlights(n)
    case n: PStringLiteral => PLspStringLiteral.getSemanticHighlights(n)
    case n: PIdnUse => PLspIdnUse.getSemanticHighlights(n)
    case n: PReserved[_] => PLspReserved.getSemanticHighlights(n)
    case n: PDeclaration => PLspDeclaration.getSemanticHighlights(n)
  }).flatten
}

object HasSignatureHelps {
  def apply(p: PProgram): Seq[SignatureHelp] = p.deepCollect({
    case n: PCallable => {
    val bound = SelectionBoundKeyword(n.idndef.name)
    // Start
    val start = SignatureHelpPart(false, s"${n.keyword.pretty}${n.idndef.pretty}${n.args.l.pretty}", None)
    // Args
    def faToSigHelpPart(fa: PAnyFormalArgDecl): SignatureHelpPart = SignatureHelpPart(true, fa.pretty, None)
    val args = n.args.inner.first.map(faToSigHelpPart).toSeq ++ n.args.inner.inner.flatMap {
      case (c, fa) => Seq(SignatureHelpPart(false, c.pretty, None), faToSigHelpPart(fa))
    }
    // Tail
    val tail = SignatureHelpPart(false, s"${n.args.r.pretty}${n.returnNodes.map(_.pretty).mkString}", None)
    Seq(SignatureHelp(start +: args :+ tail, PLspDeclaration.documentation(n), bound))
  }
  }).flatten
}

object HasSuggestionScopeRanges {
  def apply(p: PProgram): Seq[SuggestionScopeRange] = p.deepCollect({
    case n: PAnyFunction =>
      RangePosition(n).map(SuggestionScopeRange(_, CallableSignatureScope)).toSeq ++
      n.body.flatMap(RangePosition(_)).map(SuggestionScopeRange(_, ExpressionScope)).toSeq
    case n: PDomain =>
      RangePosition(n.members).map(SuggestionScopeRange(_, DomainScope)).toSeq
    case n: PMethod =>
      RangePosition(n).map(SuggestionScopeRange(_, CallableSignatureScope)).toSeq ++
      n.body.flatMap(RangePosition(_)).map(SuggestionScopeRange(_, StatementScope)).toSeq
  }).flatten ++ p.shallowCollect({
    case n: PExp => RangePosition(n).map(SuggestionScopeRange(_, ExpressionScope))
  }).flatten
}

object HasCompletionProposals {
  def apply(p: PProgram): Seq[CompletionProposal] = DefaultCompletionProposals.getCompletionProposals ++ p.deepCollect({
    case n: PDeclaration => PLspDeclaration.getCompletionProposals(n)
  }).flatten
}


////
// Identifiers (uses and definitions)
////

object PLspIdnUse {
  def getSemanticHighlights(self: PIdnUse): Seq[SemanticHighlight] = (self.decl, RangePosition(self)) match {
    case (Some(d), Some(sp)) => Seq(
      SemanticHighlight(sp, PLspDeclaration.tokenType(d), (if (self.assignUse) Seq(TokenModifier.Modification) else Nil) ++ PLspDeclaration.tokenModifiers(d))
    )
    case _ => Nil
  }

  def decls(self: PIdnUse): Seq[(RangePosition, PDeclarationInner)] = RangePosition(self).toSeq.flatMap(ip => (self match {
    case i: PIdnUseName[_] => i.decls
    case _ => self.decl.toSeq
  }) map (ip -> _))

  def getHoverHints(self: PIdnUse): Seq[HoverHint] = PLspIdnUse.decls(self).map {
    case (ip, d) => HoverHint(PLspDeclaration.hint(d), PLspDeclaration.documentation(d), Some(ip), SelectionBoundScope(ip))
  }

  def getGotoDefinitions(self: PIdnUse): Seq[GotoDefinition] = PLspIdnUse.decls(self).flatMap(d =>
    (PLspDeclaration.idndefRange(d._2), PLspDeclaration.declRange(d._2)) match {
      case (Some(ip), Some(dp)) => Some(GotoDefinition(dp, ip, SelectionBoundScope(d._1)))
      case _ => None
    }
  )

  def getReferenceTos(self: PIdnUse): Seq[ReferenceTo] = PLspIdnUse.decls(self).flatMap(d =>
    PLspDeclaration.idndefRange(d._2).map(ip => ReferenceTo(ip, d._1))
  )
}

////
// Keywords
////

object PLspReserved {
  def getSemanticHighlights[T <: PReservedString](self: PReserved[T]): Seq[SemanticHighlight] = (RangePosition(self), PLspReservedString.maybeTokenType(self.rs)) match {
    case (Some(sp), Some(tokenType)) => Seq(SemanticHighlight(sp, tokenType, PLspReservedString.tokenModifiers(self.rs)))
    case _ => Nil
  }
  def getHoverHints[T <: PReservedString](self: PReserved[T]): Seq[HoverHint] = (Documentation.skip(self.rs), RangePosition(self)) match {
    case (false, Some(rp)) => Seq(HoverHint(self.pretty, Documentation(self.rs), Some(rp), SelectionBoundScope(rp)))
    case _ => Nil
  }
}

object PLspReservedString {
  def maybeTokenType(self: PReservedString): Option[TokenType.TokenType] = self match {
    case _: PKeywordLang => Some(TokenType.Keyword)
    case _: PKeywordStmt => Some(TokenType.Keyword)
    case _: PKeywordType => Some(TokenType.Type)
    case _: PKeywordConstant => Some(TokenType.Keyword)
    case _: PSymbolLang => None
    case _: POperatorKeyword => Some(TokenType.Keyword)
    case _: POperator => Some(TokenType.Operator)
    case _ => None
  }
  def tokenModifiers(self: PReservedString): Seq[TokenModifier.TokenModifier] = self match {
    case _: PKeywordStmt => Seq(TokenModifier.ControlFlow)
    case _ => Nil
  }
}

////
// Operator applications
////

object PLspExp {
  def ops(self: PExp): Seq[PReserved[POperator]] = self.subnodes flatMap (_.shallowCollect({
    case r: PReserved[_] if r.rs.isInstanceOf[POperator] => Some(r.asInstanceOf[PReserved[POperator]])
    case _: PExp => None
  }).flatten)
  def hovers(self: PExp): Seq[RangePosition] = ops(self).flatMap(RangePosition(_))
  def hint(self: PExp): String = {
    val pretty = self.prettyMapped({
      case e: PExp => e.typ.pretty
    })
    s"$pretty: ${self.typ.pretty}"
  }
  def documentation(self: PExp): Option[String] = {
    val docs = ops(self).flatMap(op => Documentation(op.rs).map(op -> _))
    docs.length match {
      case 0 => None
      case 1 => Some(docs.head._2)
      case _ => Some(docs.map(doc => s"`${doc._1.display}`: ${doc._2}").mkString("\n\n---\n\n"))
    }
  }
  def getHoverHints(self: PExp): Seq[HoverHint] =
    hovers(self).map(hp => HoverHint(hint(self), documentation(self), RangePosition(self), SelectionBoundScope(hp)))
}

object PLspCall {
  def formalArgs(self: PCall): Option[Seq[PAnyFormalArgDecl]] = self.idnref.decl.map(_.formalArgs)
  def idnUseMatchesArg(decl: String, use: String): Boolean = {
    val d = decl.toLowerCase()
    val parts = use.toLowerCase().split('_')
    parts.head == d || parts.last == d
  }
  def inlayHint(idndef: PIdnDef, arg: PExp): Option[InlayHint] = arg match {
    case PIdnUseExp(use) if idnUseMatchesArg(idndef.name, use) => None
    case _ => RangePosition(arg).map(argRp => {
      val declName = InlayHintLabelPart(idndef.pretty, None, RangePosition(idndef))
      val label = Seq(declName, InlayHintLabelPart(":"))
      InlayHint(argRp, label, Some(InlayHintKind.Parameter), false, true)
    })
  }
  def getInlayHints(self: PCall): Seq[InlayHint] = formalArgs(self).toSeq.flatMap(_.zip(self.args).flatMap {
    case (PDomainFunctionArg(Some(decl), _, _), arg) => inlayHint(decl, arg)
    case (decl: PDeclaration, arg) => inlayHint(decl.idndef, arg)
    case _ => None
  })
}

////
// Statements
////

object PLspLet {
  def getInlayHints(self: PLet): Seq[InlayHint] =
    if (!self.exp.inner.typ.isValidOrUndeclared) Nil else
      RangePosition(self.variable).map(vp => {
        vp.start = vp._end
        val label = InlayHintLabelPart(s": ${self.exp.inner.typ.pretty}")
        InlayHint(vp, Seq(label), Some(InlayHintKind.Type), true, false)
      }).toSeq
}

////
// Members
////

object PLspImport {
  def getSymbol(self: PImport): Option[DocumentSymbol] = (RangePosition(self), RangePosition(self.file), self.resolved) match {
    case (Some(tp), Some(fp), Some(resolved)) =>
      // We avoid any circular dependencies since `resolved` is only set for imports which caused a
      // file to actually be imported.
      Some(DocumentSymbol(resolved.getFileName.toString(), None, tp, fp, SymbolKind.File, Nil, Some(resolved)))
    case _ => None
  }
}

object PLspDeclaration {
  def tokenType(self: PDeclarationInner): TokenType.TokenType = self match {
    case _: PFormalArgDecl => TokenType.Parameter
    case _: PFormalReturnDecl => TokenType.Variable
    case _: PLogicalVarDecl => TokenType.Parameter
    case _: PLetNestedScope => TokenType.Parameter
    case _: PLocalVarDecl => TokenType.Variable
    case _: PFieldDecl => TokenType.Property
    case _: PDefineParam => TokenType.Parameter
    case _: PTypeVarDecl => TokenType.TypeParameter
    case _: PLabel => TokenType.Event
    case _: PPredicate => TokenType.Struct
    case _: PDefine => TokenType.Macro
    case _: PDomain => TokenType.Interface
    case _: PMethod => TokenType.Method
    case _: PAdt => TokenType.Enum
    case _: PAdtConstructor => TokenType.EnumMember

    case _: PAnyFunction => TokenType.Function

    case _ => TokenType.Comment
  }
  def tokenModifiers(self: PDeclarationInner): Seq[TokenModifier.TokenModifier] = self match {
    case _: PFormalArgDecl => Seq(TokenModifier.Readonly)
    case _: PLogicalVarDecl => Seq(TokenModifier.Readonly)
    case _ => Nil
  }
  def getSemanticHighlights(self: PDeclaration): Seq[SemanticHighlight] = RangePosition(self.idndef) match {
    case Some(sp) => Seq(SemanticHighlight(sp, tokenType(self), TokenModifier.Definition +: tokenModifiers(self)))
    case _ => Nil
  }

  def hint(self: PDeclarationInner): String = self match {
    case d: PFieldDecl => s"${d.decl.map(_.field).getOrElse(PReserved.implied(PKw.Field)).pretty}${self.pretty}"
    case d: PDomain => s"${d.domain.pretty}${d.idndef.pretty}${d.typVars.map(_.pretty).getOrElse("")}"
    case d: PAdt => s"${d.adt.pretty}${d.idndef.pretty}${d.typVars.map(_.pretty).getOrElse("")}"
    case d: PCallable =>
      val firstLine = s"${d.keyword.pretty}${d.idndef.pretty}${d.args.pretty}${d.returnNodes.map(_.pretty).mkString}"
      val contract = (d.pres.toSeq ++ d.posts.toSeq).map(_.pretty)
      val bodyString = (contract.length, d.body) match {
        case (_, None) => ""
        case (0, Some(_)) => " { ... }"
        case (_, Some(_)) => "\n{ ... }"
      }
      s"$firstLine${contract.mkString}$bodyString"
    case _ => self.pretty
  }
  def documentation(self: PDeclarationInner): Option[String] = self match {
    case d: PAnnotated =>
      val docs = d.annotations.filter(_.key.str == "doc").map(_.values.inner.toSeq.map(_.grouped.inner).mkString("\n"))
      if (docs.isEmpty) None else Some(docs.mkString("\n\n"))
    case _ => None
  }
  def getHoverHints(self: PDeclaration): Seq[HoverHint] = idndefRange(self).map(ip =>
      HoverHint(hint(self), documentation(self), RangePosition(self), SelectionBoundScope(ip))
    ).toSeq

  def declRange(self: PDeclarationInner): Option[RangePosition] = self match {
    case n: PLetNestedScope => RangePosition(n.outerLet)
    case _ => RangePosition(self)
  }
  def idndefRange(self: PDeclarationInner): Option[RangePosition] = RangePosition(self.idndef)
  def detail(self: PDeclarationInner): Option[String] = self match {
    case n: PTypedDeclaration => Some(n.typ.pretty)
    case _ => None
  }
  def symbolKind(self: PDeclarationInner): SymbolKind.SymbolKind = self match {
    case _: PFieldDecl => SymbolKind.Property
    case _: PTypeVarDecl => SymbolKind.TypeParameter
    case _: PLabel => SymbolKind.Event
    case _: PPredicate => SymbolKind.Struct
    case _: PDefine => SymbolKind.Function
    case _: PDomain => SymbolKind.Interface
    case _: PMethod => SymbolKind.Method
    case _: PLetNestedScope => SymbolKind.Variable
    case _: PAdt => SymbolKind.Enum
    case _: PAdtConstructor => SymbolKind.EnumMember

    case _: PTypedVarDecl => SymbolKind.Variable
    case _: PAnyFunction => SymbolKind.Function

    case _ => SymbolKind.Null
  }
  def tags(self: PDeclarationInner): Seq[SymbolTag.SymbolTag] = if (isDeprecated(self)) Seq(SymbolTag.Deprecated) else Nil
  def isDeprecated(self: PDeclarationInner): Boolean = false
  def getSymbol(self: PDeclarationInner): Option[DocumentSymbol] = (declRange(self), idndefRange(self)) match {
    case (Some(range), Some(selectionRange)) => Some(DocumentSymbol(self.idndef.pretty, detail(self), range, selectionRange, symbolKind(self), tags(self), None, HasDocumentSymbol(self)))
    case _ => None
  }

  def getGotoDefinitions(self: PDeclaration): Seq[GotoDefinition] = (declRange(self), idndefRange(self)) match {
    case (Some(tp), Some(ip)) => Seq(GotoDefinition(tp, ip, SelectionBoundScope(ip)))
    case _ => Nil
  }

  def getReferenceTos(self: PDeclaration): Seq[ReferenceTo] = idndefRange(self).map(ip => ReferenceTo(ip, ip)).toSeq

  def completionScope(self: PDeclarationInner): Map[SuggestionScope, Byte] = self match {
    case _: PLetNestedScope => Map(ExpressionScope -> 0, StatementScope -> -50)
    case _: PFieldDecl => Map(ExpressionScope -> 0, StatementScope -> -50)
    case _: PTypeVarDecl => Map(TypeScope -> 0)
    case _: PLabel => Map(LabelScope -> 0, StatementScope -> -50)
    case d: PDefine => d.body match {
      case _: PExp => Map(ExpressionScope -> 0, TypeScope -> 0, StatementScope -> -50)
      case _: PStmt => Map(StatementScope -> 0)
      case _ => Map(MemberScope -> -50)
    }
    case _: PDomain => Map(TypeScope -> 0)
    case _: PMethod => Map(StatementScope -> 0, ExpressionScope -> -20)
    case _: PAdt => Map(TypeScope -> 0)
    case _: PAdtConstructor => Map(ExpressionScope -> 0, StatementScope -> -50)

    case _: PAnyFunction => Map(ExpressionScope -> 0, StatementScope -> -50)
    case _: PTypedVarDecl => Map(ExpressionScope -> 0, StatementScope -> -50)

    case _ => Map()
  }
  def completionKind(self: PDeclarationInner): CompletionKind.CompletionKind = self match {
    case _: PLetNestedScope => CompletionKind.Variable
    case _: PFieldDecl => CompletionKind.Property
    case _: PTypeVarDecl => CompletionKind.TypeParameter
    case _: PLabel => CompletionKind.Event
    case _: PPredicate => CompletionKind.Struct
    case _: PDefine => CompletionKind.Snippet
    case _: PDomain => CompletionKind.Interface
    case _: PMethod => CompletionKind.Method
    case _: PAdt => CompletionKind.Enum
    case _: PAdtConstructor => CompletionKind.EnumMember

    case _: PAnyFunction => CompletionKind.Function
    case _: PTypedVarDecl => CompletionKind.Variable

    case _ => CompletionKind.Unit
  }
  def completionChars(self: PDeclarationInner): Option[Map[Char, Byte]] = self match {
    case _: PFieldDecl => Some(Map('.' -> 50))
    case _: PDomain => Some(Map(':' -> 50))
    case _: PTypeVarDecl => Some(Map(':' -> 50))
    case _ => None
  }
  def newText(self: PDeclarationInner): Option[(String, InsertTextFormat.InsertTextFormat)] = self match {
    case d: PCallable => {
      val args = d.formalArgs.zipWithIndex.map {
        case (ad: PFormalArgDecl, i) => s"$${${i+1}:${ad.idndef.pretty}}"
        case (_, i) => s"$${${i+1}:arg${i+1}}"
      }
      Some((s"${d.idndef.pretty}(${args.mkString(", ")})", InsertTextFormat.Snippet))
    }
    case _ => None
  }
  def typeHint(self: PDeclarationInner): Option[String] = self match {
    case n: PTypedDeclaration => n.typ match {
      case typ: PFunctionType => Some(typ.pretty)
      case typ => Some(": " + typ.pretty)
    }
    case n: PMethod => {
      val args = n.args.prettyMapped({ case fa: PTypedVarDecl => fa.typ.pretty })
      val rets = n.returns.map(_.prettyMapped({ case fa: PTypedVarDecl => fa.typ.pretty })).getOrElse("")
      Some(s"$args$rets")
    }
    case _ => None
  }
  def description(self: PDeclarationInner): String = self match {
    case _: PFormalArgDecl => "Argument Binding"
    case _: PFormalReturnDecl => "Return Variable"
    case _: PLogicalVarDecl => "Logical Binding"
    case _: PLocalVarDecl => "Local Variable"
    case _: PFieldDecl => "Field"
    case _: PLetNestedScope => "Let Binding"
    case _: PDefineParam => "Macro Parameter"
    case _: PTypeVarDecl => "Type Variable"
    case _: PLabel => "Label"
    case _: PPredicate => "Predicate"
    case _: PDefine => "Macro"
    case _: PDomain => "Domain"
    case _: PDomainFunction => "Domain Function"
    case _: PMethod => "Method"
    case _: PFunction => "Function"
    case _: PAdt => "Adt"
    case _: PAdtConstructor => "Adt Constructor"
    case _ => "Declaration"
  }
  def getCompletionProposals(self: PDeclaration): Seq[CompletionProposal] = RangePosition(self).map(tp => {
    val encScope = self match {
      case _: PGlobalDeclaration => None
      case _: PMemberDeclaration => self.getAncestor[PMember]
      case _: PLocalDeclaration => self.getAncestor[PScope]
    }
    val rp = encScope.flatMap(RangePosition(_)).map(sp =>
      if (self.isInstanceOf[PBackwardDeclaration]) sp else RangePosition(tp.file, tp.start, sp._end)
    )
    val bound = SuggestionBound(rp, completionScope(self), completionChars(self))
    val nt = newText(self)
    val format = nt.map(_._2).getOrElse(InsertTextFormat.PlainText)
    CompletionProposal(self.idndef.pretty, completionKind(self), typeHint(self), Some(description(self)), nt.map(_._1), format, Nil, None, None, bound)
  }).toSeq
}

object PLspAnnotation {
  def getSemanticHighlights(self: PAnnotation): Seq[SemanticHighlight] = RangePosition(self.key).map(SemanticHighlight(_, TokenType.Decorator)).toSeq
}

object PLspStringLiteral {
  def multilineRangePositions(self: PStringLiteral): Seq[RangePosition] = RangePosition(self.grouped.inner).toSeq.flatMap(rp => {
      var last = rp.start
      val lines = self.grouped.inner.str.split("\n")
      val linesRp = lines.map(line => {
        val delta = line.length()
        val end = last.deltaColumn(delta)
        val lineRp = RangePosition(rp.file, last, end)
        last = LineColumnPosition(last.line + 1, 1)
        lineRp
      })
      linesRp.head.start = linesRp.head.start.deltaColumn(-self.grouped.l.rs.symbol.length())
      linesRp.last._end = linesRp.last._end.deltaColumn(+self.grouped.r.rs.symbol.length())
      val thisRp = RangePosition(self).get
      assert(linesRp.head.start.line == thisRp.start.line, s"Multiline string literal range start line positions do not match up: ${linesRp.head} vs $thisRp")
      assert(linesRp.head.start.column == thisRp.start.column, s"Multiline string literal range start column positions do not match up: ${linesRp.head} vs $thisRp")
      assert(linesRp.last._end.line == thisRp._end.line, s"Multiline string literal range end line positions do not match up: ${linesRp.last} vs $thisRp")
      assert(linesRp.last._end.column == thisRp._end.column, s"Multiline string literal range end column positions do not match up: ${linesRp.last} vs $thisRp")
      linesRp
    })
  def tokenType(self: PStringLiteral): TokenType.TokenType = if (self.getAncestor[PAnnotation].exists(_.key.str == "doc")) TokenType.Comment else TokenType.String
  def getSemanticHighlights(self: PStringLiteral): Seq[SemanticHighlight] = multilineRangePositions(self).map(sp => SemanticHighlight(sp, tokenType(self))).toSeq
}

object Documentation {
  def skip(rs: PReservedString): Boolean = rs.isInstanceOf[PSymbolLang] || rs.isInstanceOf[POperator]
  def apply(rs: PReservedString): Option[String] = Some(rs match {
    case PKw.Import => BuiltinFeature.Import.description
    case PKw.Define => BuiltinFeature.Macro.description
    case PKw.Field => BuiltinFeature.Field.description
    case PKw.Method => BuiltinFeature.Method.description
    case PKw.Function => BuiltinFeature.Function.description
    case PKw.FunctionD => BuiltinFeature.DomainFunction.description
    case PKw.Predicate => BuiltinFeature.Predicate.description
    case PKw.Domain => BuiltinFeature.Domain.description
    case PKw.Axiom => BuiltinFeature.DomainAxiom.description

    case PKw.Assert => BuiltinFeature.Assert.description
    case PKw.Assume => BuiltinFeature.Assume.description
    case PKw.Exhale => BuiltinFeature.Exhale.description
    case PKw.Inhale => BuiltinFeature.Inhale.description
    case PKw.Var => BuiltinFeature.LocalVarDecl.description
    case PKw.Fold => BuiltinFeature.Fold.description
    case PKw.Unfold => BuiltinFeature.Unfold.description
    case PKw.New => BuiltinFeature.New.description
    case PKw.Package => BuiltinFeature.Package.description
    case PKw.Apply => BuiltinFeature.Apply.description

    case PKwOp.Old => BuiltinFeature.Old.description
    case _ => return None
  })
}
