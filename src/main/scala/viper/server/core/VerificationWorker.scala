// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import ch.qos.logback.classic.Logger
import viper.carbon.CarbonFrontend
import viper.server.ViperConfig
import viper.server.vsi.{Envelope, VerificationTask}
import viper.silicon.SiliconFrontend
import viper.silver.ast._
import viper.silver.frontend.{DefaultStates, SilFrontend}
import viper.silver.reporter.{Reporter, _}
import viper.silver.verifier.{AbstractVerificationError, VerificationResult, _}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.language.postfixOps

class ViperServerException extends Exception
case class ViperServerWrongTypeException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name`."
}
case class ViperServerBackendNotFoundException(name: String) extends ViperServerException {
  override def toString: String = s"Verification backend (<: SilFrontend) `$name` could not be found."
}

case class SilverEnvelope(m: Message) extends Envelope

class VerificationWorker(private val viper_config: ViperConfig,
                         private val logger: Logger,
                         private val command: List[String],
                         private val program: Program)(implicit val ec: ExecutionContext) extends VerificationTask {

  private var backend: ViperBackend = _

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

  // Implementation of the Reporter interface used by the backend.
  class ActorReporter(val tag: String) extends Reporter {
    val name = s"ViperServer_$tag"

    def report(msg: Message): Unit = {
      enqueueMessages(msg)
    }
  }

  def run(): Unit = {
    try {
      command match {
        case "silicon" :: args =>
          logger.info("Creating new Silicon verification backend.")
          backend = new ViperBackend(new SiliconFrontend(new ActorReporter("silicon"), logger), program)
          backend.execute(args)
        case "carbon" :: args =>
          logger.info("Creating new Carbon verification backend.")
          backend = new ViperBackend(new CarbonFrontend(new ActorReporter("carbon"), logger), program)
          backend.execute(args)
        case custom :: args =>
          logger.info(s"Creating new verification backend based on class $custom.")
          backend = new ViperBackend(resolveCustomBackend(custom, new ActorReporter(custom)).get, program)
          backend.execute(args)
        case args =>
          logger.error("invalid arguments: ${args.toString}",
            "You need to specify the verification backend, e.g., `silicon [args]`")
      }
    }catch {
      case _: InterruptedException =>
      case _: java.nio.channels.ClosedByInterruptException =>
      case e: Throwable =>
        enqueueMessages(ExceptionReport(e))
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
      registerTaskEnd(true)
    } else {
      logger.error(s"The command `${command.mkString(" ")}` did not result in initialization of verification backend.")
      registerTaskEnd(false)
    }
  }

  override type A = Message

  override def pack(m: A): Envelope = {
    SilverEnvelope(m)
  }
}

class ViperBackend(private val _frontend: SilFrontend, private val _ast: Program) {

  def backendName: String = _frontend.verifier.getClass.getName
  def file: String = _frontend.config.file()

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

  /** Run the backend verification functionality
    * */
  def execute(args: Seq[String]){
    _frontend.setStartTime()
    _frontend.setVerifier( _frontend.createVerifier(args.mkString(" ")) )

    if (!_frontend.prepare(args)) return
    _frontend.init( _frontend.verifier )

    reportProgramStats(_ast)

    val temp_result: Option[VerificationResult] = if (_frontend.config.disableCaching()) {
      _frontend.logger.info("Verification with caching disabled")
      Some(_frontend.verifier.verify(_ast))
    } else {
      _frontend.logger.info("Verification with caching enabled")
      doCachedVerification(_ast)
      _frontend.getVerificationResult
    }

    temp_result match {
      case Some(Success) =>
        _frontend.reporter report OverallSuccessMessage(_frontend.getVerifierName, System.currentTimeMillis() - _frontend.startTime)
      // TODO: Think again about where to detect and trigger SymbExLogging
      case Some(f@Failure(_)) =>
          // Cached errors will be reported as soon as they are retrieved from the cache.
        _frontend.reporter report OverallFailureMessage(_frontend.getVerifierName,
                                                        System.currentTimeMillis() - _frontend.startTime,
                                                        Failure(f.errors.filter { e => !e.cached }))
      case None =>
    }
    _frontend.verifier.stop()
  }

  private def doCachedVerification(real_program: Program) = {
    /** Top level branch is here for the same reason as in
      * {{{viper.silver.frontend.DefaultFrontend.verification()}}} */

    val file: String = _frontend.config.file()
    val (transformed_prog, cached_results) = ViperCache.applyCache(backendName, file, real_program)

    // collect and report errors
    val errors: collection.mutable.ListBuffer[VerificationError] = ListBuffer()
    cached_results.foreach (result => {
      val cached_errors = result.verification_errors
      if(cached_errors.isEmpty){
        _frontend.reporter.report(CachedEntityMessage(_frontend.getVerifierName,result.method, Success))
      } else {
        errors ++= cached_errors
        _frontend.reporter.report(CachedEntityMessage(_frontend.getVerifierName, result.method, Failure(errors)))
      }
    })

    _frontend.logger.debug(
      s"Retrieved data from cache..." +
        s" cachedErrors: ${errors.map(_.loggableMessage)};" +
        s" methodsToVerify: ${cached_results.map(_.method.name)}.")
    _frontend.logger.trace(s"The cached program is equivalent to: \n${transformed_prog.toString()}")

    _frontend.setVerificationResult(_frontend.verifier.verify(transformed_prog))
    _frontend.setState(DefaultStates.Verification)

    // update cache
    val methodsToVerify = transformed_prog.methods.filter(_.body.isDefined)
    methodsToVerify.foreach(m => {
      // Results come back irrespective of program Member.
      val cachable_errors =
        for {
          verRes <- _frontend.getVerificationResult
          cache_errs <- verRes match {
            case Failure(errs) => getMethodSpecificErrors(m, errs)
            case Success => Some(Nil)
          }
        } yield cache_errs

      if(cachable_errors.isDefined){
        ViperCache.update(backendName, file, m, transformed_prog, cachable_errors.get) match {
          case e :: es =>
            _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}): $e. Other entries for this method: ($es)")
          case Nil =>
            _frontend.logger.trace(s"Storing new entry in cache for method (${m.name}) FAILED.")
        }
      }
    })

    // combine errors:
    if (errors.nonEmpty) {
      _frontend.getVerificationResult.get match {
        case Failure(errorList) =>
          _frontend.setVerificationResult(Failure(errorList ++ errors))
        case Success =>
          _frontend.setVerificationResult(Failure(errors))
      }
    }
  }

  /**
    * Tries to determine which of the given errors are caused by the given method.
    * If an error's scope field is set, this information is used.
    * Otherwise, if both the given method and the error have line/column position information, we calculate if the error's
    * position is inside the method.
    * If at least one error has no scope set and no position, or the method's position is not set, we cannot determine
    * if the error belongs to the method and return None.
    */
  private def getMethodSpecificErrors(m: Method, errors: Seq[AbstractError]): Option[List[AbstractVerificationError]] = {
    val methodPos = m.pos match {
      case sp: SourcePosition => Some(sp.start.line, sp.end.get.line)
      case _ => {
        None
      }
    }
    val result = scala.collection.mutable.ListBuffer[AbstractVerificationError]()

    errors.foreach {
      case e: AbstractVerificationError if e.scope.isDefined =>
        if (e.scope.get == m)
          result += e
      case e: AbstractVerificationError =>
        e.pos match {
          case pos: HasLineColumn =>
            val errorPos = pos.line
            if (methodPos.isEmpty)
            {
              return None
            }
            // The position of the error is used to determine to which Method it belongs.
            if (errorPos >= methodPos.get._1 && errorPos <= methodPos.get._2) result += e
          case _ =>
            return None
        }
      case e =>
        throw new Exception("Error with unexpected type found: " + e)
    }
    Some(result.toList)
  }

  def stop(): Unit = _frontend.verifier.stop()
}