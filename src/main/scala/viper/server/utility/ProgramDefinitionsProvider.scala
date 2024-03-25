// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.utility

import viper.silver.ast.{AbstractSourcePosition, Declaration, Domain, Field, Function, LocalVarDecl, Method, NamedDomainAxiom, Positioned, Predicate, Program, Scope, Typed}
import viper.silver.frontend.SilFrontend
import viper.silver.reporter._

import scala.language.postfixOps

trait ProgramDefinitionsProvider {
  protected val _frontend: SilFrontend

  def collect(program: Program): List[Definition] = {
    (program.members.collect {
      case t: Method =>
        (Definition(t.name, ViperMethod, t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg =>
              Definition(arg.name, ViperArgument(arg.typ), arg.pos, Some(p)) } ++
              t.formalReturns.map { arg =>
                Definition(arg.name, ViperReturnParameter(arg.typ), arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ t.deepCollectInBody {
          case scope: Scope with Positioned =>
            scope.pos match {
              case p: AbstractSourcePosition =>
                scope.scopedDecls.map {
                  case typed_decl: Declaration with Typed =>
                    Definition(typed_decl.name, ViperTypedLocalDefinition(typed_decl.typ), typed_decl.pos, Some(p))
                  case untyped_decl =>
                    Definition(untyped_decl.name, ViperUntypedLocalDefinition, untyped_decl.pos, Some(p))
                }
              case _ => Seq()
            }
        }.flatten

      case t: Function =>
        (Definition(t.name, ViperFunction, t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg => Definition(arg.name, ViperArgument(arg.typ), arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ (t.body match {
          case Some(exp) =>
            exp.deepCollect {
              case scope:Scope with Positioned =>
                scope.pos match {
                  case p: AbstractSourcePosition =>
                    scope.scopedDecls.map {
                      case typed_decl: Declaration with Typed =>
                        Definition(typed_decl.name, ViperTypedLocalDefinition(typed_decl.typ), typed_decl.pos, Some(p))
                      case untyped_decl =>
                        Definition(untyped_decl.name, ViperUntypedLocalDefinition, untyped_decl.pos, Some(p))
                    }
                  case _ => Seq()
                }
            } flatten
          case _ => Seq()
        })

      case t: Predicate =>
        (Definition(t.name, ViperPredicate, t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg => Definition(arg.name, ViperArgument(arg.typ), arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ (t.body match {
          case Some(exp) =>
            exp.deepCollect {
              case scope:Scope with Positioned =>
                scope.pos match {
                  case p: AbstractSourcePosition =>
                    scope.scopedDecls.map {
                      case typed_decl: Declaration with Typed =>
                        Definition(typed_decl.name, ViperTypedLocalDefinition(typed_decl.typ), typed_decl.pos, Some(p))
                      case untyped_decl =>
                        Definition(untyped_decl.name, ViperUntypedLocalDefinition, untyped_decl.pos, Some(p))
                    }
                  case _ => Seq()
                }
            } flatten
          case _ => Seq()
        })

      case t: Domain =>
        (Definition(t.name, ViperDomain, t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.functions.flatMap { func =>
              Definition(func.name, ViperFunction, func.pos, Some(p)) +: (func.pos match {
                case func_p: AbstractSourcePosition =>
                  func.formalArgs.map {
                      case named_arg: LocalVarDecl =>
                        Definition(named_arg.name, ViperArgument(named_arg.typ), named_arg.pos, Some(func_p))
                      case unnamed_Arg =>
                        Definition("unnamed_argument", ViperArgument(unnamed_Arg.typ), unnamed_Arg.pos, Some(func_p))
                    }
                case _ => Seq()
              })
            } ++ t.axioms.flatMap { ax =>
              Definition(ax match {
                case axiom: NamedDomainAxiom => axiom.name
                case _ => ""
              }, ViperAxiom, ax.pos, Some(p)) +: (ax.pos match {
                case _: AbstractSourcePosition =>
                  ax.exp.deepCollect {
                    case scope:Scope with Positioned =>
                      scope.pos match {
                        case p: AbstractSourcePosition =>
                          scope.scopedDecls.map {
                            case typed_decl: Declaration with Typed =>
                              Definition(typed_decl.name, ViperTypedLocalDefinition(typed_decl.typ), typed_decl.pos, Some(p))
                            case untyped_decl =>
                              Definition(untyped_decl.name, ViperUntypedLocalDefinition, untyped_decl.pos, Some(p))
                          }
                        case _ => Seq()
                      }
                  } flatten
                case _ => Seq()
              }) }
          case _ => Seq()
        })) ++ Seq()

      case t: Field =>
        Seq(Definition(t.name, ViperField(t.typ), t.pos))

    } flatten) toList
  }

  def reportProgramStats(): Unit = {
    val prog = _frontend.program.get
    _frontend.reporter.report(ProgramOutlineReport(prog.members.toList))
    _frontend.reporter.report(ProgramDefinitionsReport(collect(prog)))
  }
}