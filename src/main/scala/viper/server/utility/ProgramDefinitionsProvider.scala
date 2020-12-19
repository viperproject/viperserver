package viper.server.utility

import scala.language.postfixOps

import viper.silver.ast.{AbstractSourcePosition, Domain, Field, Function, LocalVarDecl, Method, NamedDomainAxiom, Positioned, Predicate, Program, Scope}
import viper.silver.frontend.SilFrontend
import viper.silver.reporter.{Definition, ProgramDefinitionsReport, ProgramOutlineReport, StatisticsReport}

trait ProgramDefinitionsProvider {
  protected val _frontend: SilFrontend

  def collect(program: Program): List[Definition] = {
    (program.members.collect {
      case t: Method =>
        (Definition(t.name, "Method", t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg => Definition(arg.name, "Argument", arg.pos, Some(p)) } ++
              t.formalReturns.map { arg => Definition(arg.name, "Return", arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ t.deepCollectInBody {
          case scope: Scope with Positioned =>
            scope.pos match {
              case p: AbstractSourcePosition =>
                scope.scopedDecls.map { local_decl => Definition(local_decl.name, "Local", local_decl.pos, Some(p)) }
              case _ => Seq()
            }
        }.flatten

      case t: Function =>
        (Definition(t.name, "Function", t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg => Definition(arg.name, "Argument", arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ (t.body match {
          case Some(exp) =>
            exp.deepCollect {
              case scope:Scope with Positioned =>
                scope.pos match {
                  case p: AbstractSourcePosition =>
                    scope.scopedDecls.map { local_decl => Definition(local_decl.name, "Local", local_decl.pos, Some(p)) }
                  case _ => Seq()
                }
            } flatten
          case _ => Seq()
        })

      case t: Predicate =>
        (Definition(t.name, "Predicate", t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.formalArgs.map { arg => Definition(arg.name, "Argument", arg.pos, Some(p)) }
          case _ => Seq()
        })) ++ (t.body match {
          case Some(exp) =>
            exp.deepCollect {
              case scope:Scope with Positioned =>
                scope.pos match {
                  case p: AbstractSourcePosition =>
                    scope.scopedDecls.map { local_decl => Definition(local_decl.name, "Local", local_decl.pos, Some(p)) }
                  case _ => Seq()
                }
            } flatten
          case _ => Seq()
        })

      case t: Domain =>
        (Definition(t.name, "Domain", t.pos) +: (t.pos match {
          case p: AbstractSourcePosition =>
            t.functions.flatMap { func =>
              Definition(func.name, "Function", func.pos, Some(p)) +: (func.pos match {
                case func_p: AbstractSourcePosition =>
                  func.formalArgs.map { arg => Definition(if (arg.isInstanceOf[LocalVarDecl]) arg.asInstanceOf[LocalVarDecl].name else "unnamed parameter", "Argument", arg.pos, Some(func_p)) }
                case _ => Seq()
              })
            } ++ t.axioms.flatMap { ax =>
              Definition(if (ax.isInstanceOf[NamedDomainAxiom]) ax.asInstanceOf[NamedDomainAxiom].name else "", "Axiom", ax.pos, Some(p)) +: (ax.pos match {
                case ax_p: AbstractSourcePosition =>
                  ax.exp.deepCollect {
                    case scope:Scope with Positioned =>
                      scope.pos match {
                        case p: AbstractSourcePosition =>
                          scope.scopedDecls.map { local_decl => Definition(local_decl.name, "Local", local_decl.pos, Some(p)) }
                        case _ => Seq()
                      }
                  } flatten
                case _ => Seq()
              }) }
          case _ => Seq()
        })) ++ Seq()

      case t: Field =>
        Seq(Definition(t.name, "Field", t.pos))

    } flatten) toList
  }

  private def countInstances(p: Program): Map[String, Int] = p.members.groupBy({
    case m: Method => "method"
    case fu: Function => "function"
    case p: Predicate => "predicate"
    case d: Domain => "domain"
    case fi: Field => "field"
    case _ => "other"
  }).mapValues(_.size).toMap

  def reportProgramStats(): Unit = {
    val prog = _frontend.program.get
    val stats = countInstances(prog)

    _frontend.reporter.report(ProgramOutlineReport(prog.members.toList))
    _frontend.reporter.report(StatisticsReport(
      stats.getOrElse("method", 0),
      stats.getOrElse("function", 0),
      stats.getOrElse("predicate", 0),
      stats.getOrElse("domain", 0),
      stats.getOrElse("field", 0)
    ))
    _frontend.reporter.report(ProgramDefinitionsReport(collect(prog)))
  }
}