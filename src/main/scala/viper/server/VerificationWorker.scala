/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import java.nio.file.Paths

import scala.collection.mutable.ListBuffer
import akka.actor.ActorRef
import ch.qos.logback.classic.Logger
import viper.carbon.CarbonFrontend
import viper.server.ViperServerRunner.ReporterActor
import viper.silicon.{SiliconFrontend, SymbExLogger}
import viper.silver.ast.{Position, _}
import viper.silver.frontend.{SilFrontend, TranslatorState}
import viper.silver.reporter
import viper.silver.reporter.{Reporter, _}
import viper.silver.verifier.errors._
import viper.silver.verifier.{AbstractVerificationError, _}

import scala.language.postfixOps

// Implementation of the Reporter interface used by the backend.
class ActorReporter(private val actor_ref: ActorRef, val tag: String)
  extends viper.silver.reporter.Reporter {

  val name = s"ViperServer_$tag"

  def report(msg: reporter.Message): Unit = {
    //println(s"ActorReporter reporting >>> ${msg}")
    actor_ref ! ReporterActor.ServerReport(msg)
  }
}

class ViperServerException extends Exception

case class ViperServerWrongTypeException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name`."
}

case class ViperServerBackendNotFoundException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be found."
}

class VerificationWorker(private val reporter: ActorRef,
                         private val logger: Logger,
                         private val command: List[String]) extends Runnable {

  private def resolveCustomBackend(clazzName: String, rep: Reporter): Option[SilFrontend] = {
    (try {
      val constructor = Class.forName(clazzName).getConstructor(
        classOf[viper.silver.reporter.Reporter],
        classOf[ch.qos.logback.classic.Logger])
      Some(constructor.newInstance(rep, logger))
    }
    catch {
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

  def run(): Unit = {
    try {
      command match {
        case "silicon" :: args =>
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend(new SiliconFrontend(new ActorReporter(reporter, "silicon"), logger))
          backend.execute(args)
        case "carbon" :: args =>
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend(new CarbonFrontend(new ActorReporter(reporter, "carbon"), logger))
          backend.execute(args)
        case custom :: args =>
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(resolveCustomBackend(custom, new ActorReporter(reporter, custom)).get)
          backend.execute(args)
        case args =>
          logger.error("invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
      }
    }
    catch {
      case _: InterruptedException =>
      case _: java.nio.channels.ClosedByInterruptException =>
      case e: Throwable =>
        reporter ! ReporterActor.ServerReport(ExceptionReport(e))
        logger.trace(s"Creation/Execution of the verification backend ${if (backend == null) "<null>" else backend.toString} resulted in exception.", e)
    }
    finally {
      try {
        backend.stop()
      }
      catch {
        case e: Throwable =>
          logger.trace(s"Stopping the verification backend ${if (backend == null) "<null>" else backend.toString} resulted in exception.", e)
      }
    }

    if (backend != null) {
      logger.info(s"The command `${command.mkString(" ")}` has been executed.")
      reporter ! ReporterActor.FinalServerReport(true)
    } else {
      logger.error(s"The command `${command.mkString(" ")}` did not result in initialization of verification backend.")
      reporter ! ReporterActor.FinalServerReport(false)
    }
  }
}

class ViperBackend(private val _frontend: SilFrontend) {

  override def toString: String = {
    if ( _frontend.verifier == null )
      s"ViperBackend( ${_frontend.getClass.getName} /with uninitialized verifier/ )"
    else
      s"ViperBackend( ${_frontend.verifier.name} )"
  }

  private def collectDefinitions(program: Program): List[Definition] = (program.members.collect {

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
            Definition(ax.name, "Axiom", ax.pos, Some(p)) +: (ax.pos match {
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

  private def countInstances(p: Program): Map[String, Int] = p.members.groupBy({
      case m: Method => "method"
      case fu: Function => "function"
      case p: Predicate => "predicate"
      case d: Domain => "domain"
      case fi: Field => "field"
      case _ => "other"
    }).mapValues(_.size)

  def execute(args: Seq[String]) {
    _frontend.setStartTime()

    // create the verifier
    _frontend.setVerifier( _frontend.createVerifier(args.mkString(" ")) )

    if (!_frontend.prepare(args)) return

    // initialize the translator
    _frontend.init( _frontend.verifier )

    // set the file we want to verify
    _frontend.reset( Paths.get(_frontend.config.file()) )

    // run the parser, typechecker, and verifier
    _frontend.parse()
    _frontend.typecheck()
    _frontend.translate()

    if (_frontend.errors.nonEmpty) {
      _frontend.setState( TranslatorState.Verified )

    } else {
      val prog: Program = _frontend.program.get
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

      if (_frontend.config.disableCaching()) {
        _frontend.doVerify()
      } else {
        println("start cached verification")
        doVerifyCached()
      }
    }

    _frontend.verifier.stop()

    // finish by reporting the overall outcome

    _frontend.result match {
      case Success =>
        _frontend.reporter report OverallSuccessMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime)
        // TODO: Think again about where to detect and trigger SymbExLogging
      case f@Failure(_) =>
        _frontend.reporter report OverallFailureMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime,
          // Cached errors will be reporter as soon as they are retrieved from the cache.
          Failure(f.errors.filter { e => !e.cached }))
    }

    if (SymbExLogger.enabled) {
      _frontend.reporter.report(PongMessage("<Stub SymbExLoggerReport>"))
    }
  }

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

  def doVerifyCached(): Unit = {

    // The entityHashes of the new AST are evaluated lazily.

    val (methodsToVerify, methodsToCache, cachedErrors) = consultCache()
    _frontend.logger.debug(
      s"Retrieved data from cache..." +
      s" methodsToCache: ${methodsToCache.map(_.name)};" +
      s" cachedErrors: ${cachedErrors.map(_.loggableMessage)};" +
      s" methodsToVerify: ${methodsToVerify.map(_.name)}.")

    val real_program = _frontend.program.get
    val prog: Program = Program(real_program.domains, real_program.fields, real_program.functions, real_program.predicates,
      methodsToVerify ++ methodsToCache) (real_program.pos, real_program.info, real_program.errT)

    _frontend.logger.trace(s"The cached program is equivalent to: \n${prog.toString()}")
    _frontend.setVerificationResult( _frontend.mapVerificationResult(_frontend.verifier.verify(prog)) )

    _frontend.setState( TranslatorState.Verified )

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
      _frontend.getVerificationResult.get match {
        case Failure(errorList) =>
          _frontend.setVerificationResult(Failure(errorList ++ cachedErrors))
        case Success =>
          _frontend.setVerificationResult(Failure(cachedErrors))
      }
    }
  }

  def backendName: String = _frontend.verifier.getClass.getName
  def file: String = _frontend.config.file()

  def consultCache(): (List[Method], List[Method], List[VerificationError]) = {
    val errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
    val methodsToVerify: collection.mutable.ListBuffer[Method] = ListBuffer()
    val methodsToCache: collection.mutable.ListBuffer[Method] = ListBuffer()

    val file: String = _frontend.config.file()

    //read errors from cache
    val prog: Program = _frontend.program.get
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
      case t: DomainAxiom => t.copy()(pos, t.info, t.domainName, t.errT)
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
      case t: Fresh => t.copy()(pos, t.info, t.errT)
      case t: Constraining => t.copy()(pos, t.info, t.errT)
      case t: LocalVarDeclStmt => t.copy()(pos, t.info, t.errT)

      case t: LocalVarDecl => t.copy()(pos, t.info, t.errT)

      //Expressions
      case t: FalseLit => t.copy()(pos, t.info, t.errT)
      case t: NullLit => t.copy()(pos, t.info, t.errT)
      case t: TrueLit => t.copy()(pos, t.info, t.errT)
      case t: IntLit => t.copy()(pos, t.info, t.errT)
      case t: LocalVar => t.copy()(t.typ, pos, t.info, t.errT)
      case t: viper.silver.ast.Result => t.copy()(t.typ, pos, t.info, t.errT)
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
      case t: FuncApp => t.copy()(pos, t.info, t.typ, t.formalArgs, t.errT)
      case t: DomainFuncApp => t.copy()(pos, t.info, t.typ, t.formalArgs, t.domainName, t.errT)
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

