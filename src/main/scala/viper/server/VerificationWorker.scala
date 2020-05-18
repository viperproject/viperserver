/**
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * Copyright (c) 2011-2019 ETH Zurich.
  */

package viper.server

import java.nio.file.Paths

import scala.collection.mutable.ListBuffer
import akka.actor.ActorRef
import ch.qos.logback.classic.Logger
import viper.carbon.CarbonFrontend
import viper.silver.verifier.VerificationResult
import viper.silicon.SiliconFrontend
import viper.silver.ast.{Position, _}
import viper.silver.frontend.{DefaultStates, SilFrontend}
import viper.silver.reporter
import viper.silver.reporter.{Reporter, _}
import viper.silver.verifier.errors._
import viper.silver.verifier.{AbstractVerificationError, _}

import scala.language.postfixOps


// Implementation of the Reporter interface used by the backend.
class ActorReporter(private val actor_ref: ActorRef,
                    private val rep: Option[Reporter],
                    val tag: String) extends viper.silver.reporter.Reporter {

  val name = s"ViperServer_$tag"

  // sends report msg to attached reporter Actor
  def report(msg: reporter.Message): Unit = {
    //println(s"ActorReporter reporting >>> ${msg}")
    actor_ref ! ReporterProtocol.ServerReport(msg)

    rep match {
      case Some(reporter) => reporter.report(msg)
      case None =>
    }
  }
}

class ViperServerException extends Exception
case class ViperServerWrongTypeException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name`."
}
case class ViperServerBackendNotFoundException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be found."
}
case class ViperServerPreparationException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be prepared."
}


class VerificationWorker(private val reporterActor: ActorRef,
                         private val logger: Logger,
                         private val command: List[String],
                         private val program: Option[Program],
                         private val reporter: Option[Reporter]) extends Runnable {

  private def resolveCustomBackend(clazzName: String, rep: Reporter): Option[SilFrontend] = {
    (try {
      val constructor = Class.forName(clazzName).getConstructor(
        classOf[viper.silver.reporter.Reporter],
        classOf[ch.qos.logback.classic.Logger])
      Some(constructor.newInstance(rep, logger))
    }catch {
      case e: ClassNotFoundException => None
    })
    match {
      case Some(instance) if instance.isInstanceOf[SilFrontend] =>
        Some(instance.asInstanceOf[SilFrontend])
      case Some(instance) =>
        throw ViperServerWrongTypeException(instance.getClass.getName)
      case _ =>
        throw ViperServerBackendNotFoundException(clazzName)
    }
  }

  private var backend: ViperBackend = _
  private var _backendName: String = _
  def backendName: String = _backendName

  private var _result: Option[VerificationResult] = None
  def result: Option[VerificationResult] = _result

  def run(): Unit = {

    try {
      command match {
        case "silicon" :: args =>
          _backendName = "silicon"
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend(new SiliconFrontend(new ActorReporter(reporterActor, reporter, "silicon"), logger), program)

          _result = backend.execute(args)
        case "carbon" :: args =>
          _backendName = "carbon"
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend(new CarbonFrontend(new ActorReporter(reporterActor, reporter, "carbon"), logger), program)
          _result = backend.execute(args)
        case custom :: args =>
          _backendName = custom
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(resolveCustomBackend(custom, new ActorReporter(reporterActor, reporter, custom)).get, program)
          _result = backend.execute(args)
        case args =>
          logger.error("invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
      }
    }catch {
      case _: InterruptedException =>
      case _: java.nio.channels.ClosedByInterruptException =>
      case e: Throwable =>
        reporter match {
          case Some(rep) => rep.report(ExceptionReport(e))
          case None =>
        }
        reporterActor ! ReporterProtocol.ServerReport(ExceptionReport(e))
        logger.trace(s"Creation/Execution of the verification backend ${if (backend == null) "<null>" else backend.toString} resulted in exception.", e)
    }finally {
      try {
        backend.stop()
      }catch {
        case e: Throwable =>
          logger.trace(s"Stopping the verification backend ${if (backend == null) "<null>" else backend.toString} resulted in exception.", e)
      }
    }
    if (backend != null) {
      logger.info(s"The command `${command.mkString(" ")}` has been executed.")
      reporterActor ! ReporterProtocol.FinalServerReport(true)
    } else {
      logger.error(s"The command `${command.mkString(" ")}` did not result in initialization of verification backend.")
      reporterActor ! ReporterProtocol.FinalServerReport(false)
    }

    result match {
      case Some(res) => reporterActor ! ReporterProtocol.CompleteOverallResult(res)
      case None => reporterActor ! ReporterProtocol.FailOverallResult(ViperServerPreparationException(backendName))
    }
  }
}

class ViperBackend(private val _frontend: SilFrontend, private val _ast: Option[Program] = None) {

  override def toString: String = {
    if ( _frontend.verifier == null )
      s"ViperBackend( ${_frontend.getClass.getName} /with uninitialized verifier/ )"
    else
      s"ViperBackend( ${_frontend.verifier.name} )"
  }

  private def collectDefinitions(program: Program): List[Definition] = {
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
                  func.formalArgs.map { arg => Definition(arg.name, "Argument", arg.pos, Some(func_p)) }
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
  }).mapValues(_.size)
  private def reportProgramStats(prog: Program): Unit = {
    val stats = countInstances(prog)

    _frontend.reporter.report(ProgramOutlineReport(prog.members.toList))
    _frontend.reporter.report(StatisticsReport(
      stats.getOrElse("method", 0),
      stats.getOrElse("function", 0),
      stats.getOrElse("predicate", 0),
      stats.getOrElse("domain", 0),
      stats.getOrElse("field", 0)
    ))
    _frontend.reporter.report(ProgramDefinitionsReport(collectDefinitions(prog)))
  }


  //Executes backend functionality
  def execute(args: Seq[String]): Option[VerificationResult] = {
    _frontend.setStartTime()
    _frontend.setVerifier( _frontend.createVerifier(args.mkString(" ")) )

    if (!_frontend.prepare(args)) return None
    _frontend.init( _frontend.verifier )

    if(!_ast.isDefined) return None
    val prog = _ast.get
    reportProgramStats(prog)

    val ver_result: Option[VerificationResult] = if (_frontend.config.disableCaching()) {
      _frontend.logger.info("Verification with caching disabled")
      Some(_frontend.verifier.verify(prog))
    } else {
      _frontend.logger.info("Verification with caching disabled")
      doCachedVerificationOnAst(prog)
    }
    _frontend.verifier.stop()
    ver_result
  }

  /*
  Tries to determine which of the given errors are caused by the given method.
  If an error's scope field is set, this information is used.
  Otherwise, if both the given method and the error have line/column position information, we calculate if the error's
  position is inside the method.
  If at least one error has no scope set and no position, or the method's position is not set, we cannot determine
  if the error belongs to the method and return None.
   */
  private def getMethodSpecificErrors(m: Method, errors: Seq[AbstractError]): List[AbstractVerificationError] = {
    //The position of the error is used to determine to which Method it belongs.
    val methodStart = m.pos.asInstanceOf[SourcePosition].start.line
    val methodEnd = m.pos.asInstanceOf[SourcePosition].end.get.line
    val result = scala.collection.mutable.ListBuffer[AbstractVerificationError]()

    errors.foreach {
      case e: AbstractVerificationError =>
        e.pos match {
          case pos: HasLineColumn =>
            val errorPos = pos.line
            if (errorPos >= methodStart && errorPos <= methodEnd) result += e
          case _ =>
            throw new Exception("Error determining method specific errors for the cache: The reported errors should have a location")
        }
      case e =>
        throw new Exception("Error with unexpected type found: " + e)
    }
    result.toList
  }

  private def doCachedVerificationOnAst(real_program: Program): Option[VerificationResult] = {
    /** Top level branch is here for the same reason as in
      * {{{viper.silver.frontend.DefaultFrontend.verification()}}} */

    val (methodsToVerify, methodsToCache, cachedErrors) = consultCache(real_program)
    _frontend.logger.debug(
      s"Retrieved data from cache..." +
        s" methodsToCache: ${methodsToCache.map(_.name)};" +
        s" cachedErrors: ${cachedErrors.map(_.loggableMessage)};" +
        s" methodsToVerify: ${methodsToVerify.map(_.name)}.")

//    val real_program = _frontend.program.get
    val prog: Program = Program(real_program.domains, real_program.fields, real_program.functions, real_program.predicates,
      methodsToVerify ++ methodsToCache, real_program.extensions)(real_program.pos, real_program.info, real_program.errT)

    _frontend.logger.trace(s"The cached program is equivalent to: \n${prog.toString()}")
    _frontend.setVerificationResult(_frontend.mapVerificationResult(_frontend.verifier.verify(prog)))

    _frontend.setState(DefaultStates.Verification)

    //update cache
    methodsToVerify.foreach(m => {
      _frontend.getVerificationResult.get match {
        case Failure(errors) =>
          val errorsToCache = getMethodSpecificErrors(m, errors)
          ViperCache.update(backendName, file, prog, m, errorsToCache) match {
            case e :: es =>
              _frontend.logger.debug(s"Storing new entry in cache for method (${m.name}): $e. Other entries for this method: ($es)")
            case Nil =>
              _frontend.logger.warn(s"Storing new entry in cache for method (${m.name}) FAILED. List of errors for this method: $errorsToCache")
          }
        case Success =>
          ViperCache.update(backendName, file, prog, m, Nil) match {
            case e :: es =>
              _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}): $e. Other entries for this method: ($es)")
            case Nil =>
              _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}) FAILED.")
          }
      }
    })


    //combine errors:
    if (cachedErrors.nonEmpty) {
      _frontend.getVerificationResult
    }else{
      _frontend.getVerificationResult
    }
  }

  def backendName: String = _frontend.verifier.getClass.getName
  def file: String = _frontend.config.file()

  def consultCache(prog: Program): (List[Method], List[Method], List[VerificationError]) = {
    val errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
    val methodsToVerify: collection.mutable.ListBuffer[Method] = ListBuffer()
    val methodsToCache: collection.mutable.ListBuffer[Method] = ListBuffer()

    val file: String = _frontend.config.file()

    //read errors from cache
    prog.methods.foreach((m: Method) => {
      ViperCache.get(backendName, file, m) match {
        case Nil =>
          methodsToVerify += m
        case cache_entry_list =>
          cache_entry_list.find { e =>
            prog.dependencyHashMap(m) == e.dependencyHash
          } match {
            case None =>
              //even if the method itself did not change, a re-verification is required if it's dependencies changed
              methodsToVerify += m
            case Some(matched_entry) =>
              try {
                val cachedErrors: Seq[VerificationError] = updateErrorLocation(prog, m, matched_entry)
                errors ++= cachedErrors
                methodsToCache += ViperCache.removeBody(m)
                //Send the intermediate results to the user as soon as they are available. Approximate the time with zero.
                if ( cachedErrors.isEmpty ) {
                  _frontend.reporter.report(EntitySuccessMessage(_frontend.getVerifierName, m, 0))
                } else {
                  _frontend.reporter.report(EntityFailureMessage(_frontend.getVerifierName, m, 0, Failure(cachedErrors)))
                }
              } catch {
                case e: Exception =>
                  _frontend.logger.warn("The cache lookup failed: " + e)
                  //Defaults to verifying the method in case the cache lookup fails.
                  methodsToVerify += m
              }
          }
      }
    })
    (methodsToVerify.toList, methodsToCache.toList, errors.toList)
  }

  private def updateErrorLocation(p: Program, m: Method, cacheEntry: CacheEntry): List[VerificationError] = {
    cacheEntry.errors.map(updateErrorLocation(p, m, _))
  }

  private def updateErrorLocation(p: Program, m: Method, error: LocalizedError): VerificationError = {
    assert(error.error != null && error.accessPath != null && error.reasonAccessPath != null)

    //get the corresponding offending node in the new AST
    //TODO: are these casts ok?
    val offendingNode = ViperCache.getNode(backendName, file, p, error.accessPath, error.error.offendingNode).asInstanceOf[Option[errors.ErrorNode]]
    val reasonOffendingNode = ViperCache.getNode(backendName, file, p, error.reasonAccessPath, error.error.reason.offendingNode).asInstanceOf[Option[errors.ErrorNode]]

    if (offendingNode.isEmpty || reasonOffendingNode.isEmpty) {
      throw new Exception(s"Cache error: no corresponding node found for error: $error")
    }

    //create a new VerificationError that only differs in the Position of the offending Node
    //the cast is fine, because the offending Nodes are supposed to be ErrorNodes
    val updatedOffendingNode = updatePosition(error.error.offendingNode, offendingNode.get.pos).asInstanceOf[errors.ErrorNode]
    val updatedReasonOffendingNode = updatePosition(error.error.reason.offendingNode, reasonOffendingNode.get.pos).asInstanceOf[errors.ErrorNode]
    //TODO: how to also update the position of error.error.reason.offendingNode?
    val updatedError = error.error.withNode(updatedOffendingNode).asInstanceOf[AbstractVerificationError]
    setCached(updatedError)
  }

  def setCached(error: AbstractVerificationError): AbstractVerificationError = {
    error match {
      case e: Internal => e.copy(cached = true)
      case e: AssignmentFailed => e.copy(cached = true)
      case e: CallFailed => e.copy(cached = true)
      case e: ContractNotWellformed => e.copy(cached = true)
      case e: PreconditionInCallFalse => e.copy(cached = true)
      case e: PreconditionInAppFalse => e.copy(cached = true)
      case e: ExhaleFailed => e.copy(cached = true)
      case e: InhaleFailed => e.copy(cached = true)
      case e: IfFailed => e.copy(cached = true)
      case e: WhileFailed => e.copy(cached = true)
      case e: AssertFailed => e.copy(cached = true)
      case e: TerminationFailed => e.copy(cached = true)
      case e: PostconditionViolated => e.copy(cached = true)
      case e: FoldFailed => e.copy(cached = true)
      case e: UnfoldFailed => e.copy(cached = true)
      case e: PackageFailed => e.copy(cached = true)
      case e: ApplyFailed => e.copy(cached = true)
      case e: LoopInvariantNotPreserved => e.copy(cached = true)
      case e: LoopInvariantNotEstablished => e.copy(cached = true)
      case e: FunctionNotWellformed => e.copy(cached = true)
      case e: PredicateNotWellformed => e.copy(cached = true)
      case e: MagicWandNotWellformed => e.copy(cached = true)
      case e: LetWandFailed => e.copy(cached = true)
      case e: HeuristicsFailed => e.copy(cached = true)
      case e: VerificationErrorWithCounterexample => e.copy(cached = true)
      case e: AbstractVerificationError =>
        _frontend.logger.warn("Setting a verification error to cached was not possible for " + e + ". Make sure to handle this types of errors")
        e
    }
  }

  def updatePosition(n: Node, pos: Position): Node = {
    n match {
      case t: Trigger => t.copy()(pos, t.info, t.errT)
      case t: Program => t.copy()(pos, t.info, t.errT)

      //Members
      case t: Field => t.copy()(pos, t.info, t.errT)
      case t: Function => t.copy()(pos, t.info, t.errT)
      case t: Method => t.copy()(pos, t.info, t.errT)
      case t: Predicate => t.copy()(pos, t.info, t.errT)
      case t: Domain => t.copy()(pos, t.info, t.errT)

      //DomainMembers
      case t: NamedDomainAxiom => t.copy()(pos, t.info, t.domainName, t.errT)
      case t: AnonymousDomainAxiom => t.copy()(pos, t.info, t.domainName, t.errT)
      case t: DomainFunc => t.copy()(pos, t.info, t.domainName, t.errT)

      //Statements
      case t: NewStmt => t.copy()(pos, t.info, t.errT)
      case t: LocalVarAssign => t.copy()(pos, t.info, t.errT)
      case t: FieldAssign => t.copy()(pos, t.info, t.errT)
      case t: Fold => t.copy()(pos, t.info, t.errT)
      case t: Unfold => t.copy()(pos, t.info, t.errT)
      case t: Package => t.copy()(pos, t.info, t.errT)
      case t: Apply => t.copy()(pos, t.info, t.errT)
      case t: Inhale => t.copy()(pos, t.info, t.errT)
      case t: Exhale => t.copy()(pos, t.info, t.errT)
      case t: Assert => t.copy()(pos, t.info, t.errT)
      case t: MethodCall => t.copy()(pos, t.info, t.errT)
      case t: Seqn => t.copy()(pos, t.info, t.errT)
      case t: While => t.copy()(pos, t.info, t.errT)
      case t: If => t.copy()(pos, t.info, t.errT)
      case t: Label => t.copy()(pos, t.info, t.errT)
      case t: Goto => t.copy()(pos, t.info, t.errT)
      case t: LocalVarDeclStmt => t.copy()(pos, t.info, t.errT)

      case t: LocalVarDecl => t.copy()(pos, t.info, t.errT)

      //Expressions
      case t: FalseLit => t.copy()(pos, t.info, t.errT)
      case t: NullLit => t.copy()(pos, t.info, t.errT)
      case t: TrueLit => t.copy()(pos, t.info, t.errT)
      case t: IntLit => t.copy()(pos, t.info, t.errT)
      case t: LocalVar => t.copy(typ = t.typ)(pos, t.info, t.errT)
      case t: viper.silver.ast.Result => t.copy(t.typ)(pos, t.info, t.errT)
      case t: FieldAccess => t.copy()(pos, t.info, t.errT)
      case t: PredicateAccess => t.copy()(pos, t.info, t.errT)
      case t: Unfolding => t.copy()(pos, t.info, t.errT)
      case t: Applying => t.copy()(pos, t.info, t.errT)
      case t: CondExp => t.copy()(pos, t.info, t.errT)
      case t: Let => t.copy()(pos, t.info, t.errT)
      case t: Exists => t.copy()(pos, t.info, t.errT)
      case t: Forall => t.copy()(pos, t.info, t.errT)
      case t: ForPerm => t.copy()(pos, t.info, t.errT)
      case t: InhaleExhaleExp => t.copy()(pos, t.info, t.errT)
      case t: WildcardPerm => t.copy()(pos, t.info, t.errT)
      case t: FullPerm => t.copy()(pos, t.info, t.errT)
      case t: NoPerm => t.copy()(pos, t.info, t.errT)
      case t: EpsilonPerm => t.copy()(pos, t.info, t.errT)
      case t: CurrentPerm => t.copy()(pos, t.info, t.errT)
      case t: FieldAccessPredicate => t.copy()(pos, t.info, t.errT)
      case t: PredicateAccessPredicate => t.copy()(pos, t.info, t.errT)

      //Binary operators
      case t: Add => t.copy()(pos, t.info, t.errT)
      case t: Sub => t.copy()(pos, t.info, t.errT)
      case t: Mul => t.copy()(pos, t.info, t.errT)
      case t: Div => t.copy()(pos, t.info, t.errT)
      case t: Mod => t.copy()(pos, t.info, t.errT)
      case t: LtCmp => t.copy()(pos, t.info, t.errT)
      case t: LeCmp => t.copy()(pos, t.info, t.errT)
      case t: GtCmp => t.copy()(pos, t.info, t.errT)
      case t: GeCmp => t.copy()(pos, t.info, t.errT)
      case t: EqCmp => t.copy()(pos, t.info, t.errT)
      case t: NeCmp => t.copy()(pos, t.info, t.errT)
      case t: Or => t.copy()(pos, t.info, t.errT)
      case t: And => t.copy()(pos, t.info, t.errT)
      case t: Implies => t.copy()(pos, t.info, t.errT)
      case t: MagicWand => t.copy()(pos, t.info, t.errT)
      case t: FractionalPerm => t.copy()(pos, t.info, t.errT)
      case t: PermDiv => t.copy()(pos, t.info, t.errT)
      case t: PermAdd => t.copy()(pos, t.info, t.errT)
      case t: PermSub => t.copy()(pos, t.info, t.errT)
      case t: PermMul => t.copy()(pos, t.info, t.errT)
      case t: IntPermMul => t.copy()(pos, t.info, t.errT)
      case t: PermLtCmp => t.copy()(pos, t.info, t.errT)
      case t: PermLeCmp => t.copy()(pos, t.info, t.errT)
      case t: PermGtCmp => t.copy()(pos, t.info, t.errT)
      case t: PermGeCmp => t.copy()(pos, t.info, t.errT)
      case t: AnySetUnion => t.copy()(pos, t.info, t.errT)
      case t: AnySetIntersection => t.copy()(pos, t.info, t.errT)
      case t: AnySetSubset => t.copy()(pos, t.info, t.errT)
      case t: AnySetMinus => t.copy()(pos, t.info, t.errT)
      case t: AnySetContains => t.copy()(pos, t.info, t.errT)

      //Unary operators
      case t: Minus => t.copy()(pos, t.info, t.errT)
      case t: Not => t.copy()(pos, t.info, t.errT)
      case t: PermMinus => t.copy()(pos, t.info, t.errT)
      case t: Old => t.copy()(pos, t.info, t.errT)
      case t: LabelledOld => t.copy()(pos, t.info, t.errT)
      case t: AnySetCardinality => t.copy()(pos, t.info, t.errT)
      case t: FuncApp => t.copy()(pos, t.info, t.typ, t.errT)
      case t: DomainFuncApp => t.copy()(pos, t.info, t.typ, t.domainName, t.errT)
      case t: EmptySeq => t.copy()(pos, t.info, t.errT)
      case t: ExplicitSeq => t.copy()(pos, t.info, t.errT)
      case t: RangeSeq => t.copy()(pos, t.info, t.errT)
      case t: SeqAppend => t.copy()(pos, t.info, t.errT)
      case t: SeqIndex => t.copy()(pos, t.info, t.errT)
      case t: SeqTake => t.copy()(pos, t.info, t.errT)
      case t: SeqDrop => t.copy()(pos, t.info, t.errT)
      case t: SeqContains => t.copy()(pos, t.info, t.errT)
      case t: SeqUpdate => t.copy()(pos, t.info, t.errT)
      case t: SeqLength => t.copy()(pos, t.info, t.errT)

      //others
      case t: EmptySet => t.copy()(pos, t.info, t.errT)
      case t: ExplicitSet => t.copy()(pos, t.info, t.errT)
      case t: EmptyMultiset => t.copy()(pos, t.info, t.errT)
      case t: ExplicitMultiset => t.copy()(pos, t.info, t.errT)
      case t =>
        _frontend.logger.warn("The location was not updated for the node " + t + ". Make sure to handle this type of node")
        t
    }
  }

  def stop(): Unit = _frontend.verifier.stop()
}

