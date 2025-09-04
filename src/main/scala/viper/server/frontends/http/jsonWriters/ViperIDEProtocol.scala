// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.http.jsonWriters

import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import edu.mit.csail.sdg.translator.A4Solution
import spray.json.DefaultJsonProtocol
import viper.server.vsi.{AstJobId, VerJobId}
import viper.silicon.logger.{LogConfig, MemberSymbExLog}
import viper.silicon.logger.writer.{SymbExLogReportWriter, TermWriter}
import viper.silicon.state.terms.Term
import viper.silver.ast._
import viper.silver.reporter._
import viper.silver.verifier.{ValueEntry, _}

object ViperIDEProtocol extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  import spray.json._

  final case class VerificationRequestAccept(ast_id: AstJobId, ver_id: VerJobId)
  final case class VerificationRequestReject(msg: String)
  final case class ServerStopConfirmed(msg: String)
  final case class JobDiscardAccept(msg: String)
  final case class JobDiscardReject(msg: String)
  final case class CacheFlushAccept(msg: String)
  final case class CacheFlushReject(msg: String)
  final case class AlloyGenerationRequestReject(reason: String)
  final case class AlloyGenerationRequestComplete(solution: A4Solution)


  implicit val verReqAccept_writer: RootJsonFormat[VerificationRequestAccept] = lift(new RootJsonWriter[VerificationRequestAccept] {
    override def write(obj: VerificationRequestAccept): JsValue = JsObject(
      "ast_id" -> JsNumber(obj.ast_id.id),
      "id" -> JsNumber(obj.ver_id.id)
    )
  })
  implicit val verReqReject_format: RootJsonFormat[VerificationRequestReject] = jsonFormat1(VerificationRequestReject.apply)
  implicit val serverStopConfirmed_format: RootJsonFormat[ServerStopConfirmed] = jsonFormat1(ServerStopConfirmed.apply)
  implicit val jobDiscardAccept_format: RootJsonFormat[JobDiscardAccept] = jsonFormat1(JobDiscardAccept.apply)
  implicit val jobDiscardReject_format: RootJsonFormat[JobDiscardReject] = jsonFormat1(JobDiscardReject.apply)
  implicit val CacheFlushAccept_format: RootJsonFormat[CacheFlushAccept] = jsonFormat1(CacheFlushAccept.apply)
  implicit val CacheFlushReject_format: RootJsonFormat[CacheFlushReject] = jsonFormat1(CacheFlushReject.apply)
  implicit val alloyGenReqReject_format: RootJsonFormat[AlloyGenerationRequestReject] = jsonFormat1(AlloyGenerationRequestReject.apply)
  implicit val alloyGenReqComplete_format: RootJsonFormat[AlloyGenerationRequestComplete] = lift(
    new RootJsonWriter[AlloyGenerationRequestComplete] {
      override def write(obj: AlloyGenerationRequestComplete): JsValue = AlloySolutionWriter.toJSON(obj.solution)
    }
  )

  // Implicit conversions for reporter.Message.

  implicit val file_format: RootJsonFormat[File] = lift(new RootJsonWriter[File] {
    override def write(obj: File): JsValue = JsString(obj.toAbsolutePath.toString)
  })

  implicit val sourcePosition_writer: RootJsonFormat[AbstractSourcePosition] = lift(new RootJsonWriter[AbstractSourcePosition] {
    override def write(obj: AbstractSourcePosition): JsValue = JsObject(
      "file" -> (if (obj.file != null) {
        //FIXME This hack is needed due to the following bug in Silver: https://github.com/viperproject/silver/issues/253
        //TODO  The bug has been fixed. Review if this is still needed.
        obj.file.toJson
      } else {
        JsString("<undefined>")
      }),
      "start" -> JsString(s"${obj.line}:${obj.column}"),
      "end" -> (obj.end match {
        case Some(end_pos) =>
          JsString(s"${end_pos.line}:${end_pos.column}")
        case _ =>
          JsString(s"<undefined>")
      }))
  })

  implicit val position_writer: RootJsonFormat[Position] = lift(new RootJsonWriter[Position] {
    override def write(obj: Position): JsValue = obj match {
      case NoPosition => JsString(obj.toString)
      case sp: AbstractSourcePosition => sp.toJson
      case hlc: HasLineColumn => JsString(s"${hlc.line}:${hlc.column}")
    }
  })

  implicit val optionAny_writer: RootJsonFormat[Option[Any]] = lift(new RootJsonWriter[Option[Any]] {
    override def write(obj: Option[Any]): JsValue = obj match {
      case Some(stuff) => stuff.toString.toJson
      case _ => JsObject.empty
    }
  })

  implicit val entity_writer: RootJsonFormat[Entity] = lift(new RootJsonWriter[Entity] {
    override def write(obj: Entity): JsObject = {
      val entity_type = obj match {
        case _: viper.silver.ast.Method => "method"
        case _: viper.silver.ast.Function => "function"
        case _: viper.silver.ast.Predicate => "predicate"
        case _: viper.silver.ast.Domain => "domain"
        case _: viper.silver.ast.Field => "field"
        case _ => s"<not a Viper top-lever entity: ${obj.getClass.getCanonicalName}>"
      }
      JsObject(
        "type" -> JsString(entity_type),
        "name" -> JsString(obj.name),
        "position" -> obj.pos.toJson
      )
    }
  })

  implicit val constantEntry_writer: RootJsonFormat[ConstantEntry] = lift(new RootJsonWriter[ConstantEntry] {
    override def write(obj: ConstantEntry): JsValue = JsString(obj.value)
  })

  implicit val applicationEntry_writer: RootJsonFormat[ApplicationEntry] = lift(new RootJsonWriter[ApplicationEntry] {
    override def write(obj: ApplicationEntry): JsValue = JsObject(
      "name" -> JsString(obj.name),
      "args" -> JsArray(obj.arguments.map(_.toJson).toVector)
    )
  })

  implicit val modelValue_writer: RootJsonFormat[ValueEntry] = lift(new RootJsonWriter[ValueEntry] {
    override def write(obj: ValueEntry): JsValue = obj match {
      case c: ConstantEntry =>
        JsObject(
          "type" -> JsString("constant_entry"),
          "value" -> c.toJson
        )
      case a: ApplicationEntry =>
        JsObject(
          "type" -> JsString("application_entry"),
          "value" -> a.toJson
        )
    }
  })

  implicit val mapEntry_writer: RootJsonFormat[MapEntry] = lift(new RootJsonWriter[MapEntry] {
    override def write(obj: MapEntry): JsValue = JsObject(
      "type" -> JsString("map_entry"),
      "cases" -> JsArray(obj.options.map {
        case (args: Seq[ValueEntry], res: ValueEntry) =>
          JsObject("args" -> JsArray(args.map(_.toJson).toVector),
                   "value" -> res.toJson)
      }.toVector),
      "default" -> obj.default.toJson
    )
  })

  implicit val modelEntry_writer: RootJsonFormat[ModelEntry] = lift(new RootJsonWriter[ModelEntry] {
    override def write(obj: ModelEntry): JsValue = obj match {
      case ve: ValueEntry =>
        ve.toJson
      case me: MapEntry =>
        me.toJson
    }
  })

  implicit val model_writer: RootJsonFormat[Model] = lift(new RootJsonWriter[Model] {
    override def write(obj: Model): JsValue =
      JsObject(obj.entries.map { case (k: String, v: ModelEntry) => (k, v.toJson) })
  })

  implicit val counterexample_writer: RootJsonFormat[Counterexample] = lift(new RootJsonWriter[Counterexample] {
    override def write(obj: Counterexample): JsValue = JsObject("model" -> obj.model.toJson)
  })

  implicit val abstractError_writer: RootJsonFormat[AbstractError] = lift(new RootJsonWriter[AbstractError] {
    override def write(obj: AbstractError): JsValue = {
      obj match {
        case e: VerificationError =>
          val counterexamples: Seq[Counterexample] = e.failureContexts.flatMap(ctx => ctx.counterExample)
          JsObject(
            "tag" -> JsString(obj.fullId),
            "text" -> JsString(obj.readableMessage),
            "position" -> obj.pos.toJson,
            "cached" -> JsBoolean(obj.cached),
            "counterexamples" -> JsArray(counterexamples.map(_.toJson).toVector))
        case _ =>
          JsObject(
            "tag" -> JsString(obj.fullId),
            "text" -> JsString(obj.readableMessage),
            "position" -> obj.pos.toJson,
            "cached" -> JsBoolean(obj.cached))
      }
    }
  })

  implicit val failure_writer: RootJsonFormat[Failure] = lift(new RootJsonWriter[Failure] {
    override def write(obj: Failure): JsObject =
      JsObject(
        "type" -> JsString("error"),
        "errors" -> JsArray(obj.errors.map(_.toJson).toVector))
  })

  implicit val entitySuccessMessage_writer: RootJsonFormat[EntitySuccessMessage] = lift(new RootJsonWriter[EntitySuccessMessage] {
    override def write(obj: EntitySuccessMessage): JsObject = {
      JsObject(
        "entity" -> obj.concerning.toJson,
        "time" -> obj.verificationTime.toJson,
        "cached" -> obj.cached.toJson)
    }
  })

  implicit val entityFailureMessage_writer: RootJsonFormat[EntityFailureMessage] = lift(new RootJsonWriter[EntityFailureMessage] {
    override def write(obj: EntityFailureMessage): JsValue = JsObject(
      "entity" -> obj.concerning.toJson,
      "time" -> obj.verificationTime.toJson,
      "result" -> obj.result.toJson,
      "cached" -> obj.cached.toJson)
  })

  implicit val astConstructionMessage_writer: RootJsonFormat[AstConstructionResultMessage] = lift(new RootJsonWriter[AstConstructionResultMessage] {
    override def write(obj: AstConstructionResultMessage): JsValue = JsObject(
      "status" -> (obj match {
        case _: AstConstructionSuccessMessage =>
          JsString("success")
        case _: AstConstructionFailureMessage =>
          JsString("failure")
      }),
      "details" -> (obj match {
        case succ: AstConstructionSuccessMessage =>
          succ.toJson
        case fail: AstConstructionFailureMessage =>
          fail.toJson
      }))
  })

  implicit val astConstructionSuccess_writer: RootJsonFormat[AstConstructionSuccessMessage] = lift(new RootJsonWriter[AstConstructionSuccessMessage] {
    override def write(obj: AstConstructionSuccessMessage): JsValue = JsObject(
      "time" -> obj.astConstructionTime.toJson)
  })

  implicit val astConstructionFailure_writer: RootJsonFormat[AstConstructionFailureMessage] = lift(new RootJsonWriter[AstConstructionFailureMessage] {
    override def write(obj: AstConstructionFailureMessage): JsValue = JsObject(
      "time" -> obj.astConstructionTime.toJson,
      "result" -> obj.result.toJson)
  })

  implicit val overallSuccessMessage_writer: RootJsonFormat[OverallSuccessMessage] = lift(new RootJsonWriter[OverallSuccessMessage] {
    override def write(obj: OverallSuccessMessage): JsObject = JsObject(
      "time" -> obj.verificationTime.toJson)
  })

  implicit val overallFailureMessage_writer: RootJsonFormat[OverallFailureMessage] = lift(new RootJsonWriter[OverallFailureMessage] {
    override def write(obj: OverallFailureMessage): JsValue = JsObject(
      "time" -> obj.verificationTime.toJson,
      "result" -> obj.result.toJson
    )
  })

  implicit val verificationResult_writer: RootJsonFormat[VerificationResult] = lift(new RootJsonWriter[VerificationResult] {
    override def write(obj: VerificationResult): JsValue = obj match {
      case Success => JsObject("type" -> JsString("success"))
      case f@ Failure(_) => f.toJson
    }
  })

  implicit val verifResultMessage_writer: RootJsonFormat[VerificationResultMessage] = lift(new RootJsonWriter[VerificationResultMessage] {
    override def write(obj: VerificationResultMessage): JsValue = {
      val is_overall = obj match {
        case _: OverallFailureMessage => true
        case _: OverallSuccessMessage => true
        case _ => false
      }
      JsObject(
        "verifier" -> JsString(obj.verifier),
        "status" -> (obj.result match {
          case viper.silver.verifier.Success => JsString("success")
          case _ => JsString("failure")
        }),
        "kind" -> (if (is_overall) {
          JsString("overall")
        } else {
          JsString("for_entity")
        }),
        "details" -> (obj match {
          case a: OverallFailureMessage => a.toJson
          case b: OverallSuccessMessage => b.toJson
          case c: EntityFailureMessage => c.toJson
          case d: EntitySuccessMessage => d.toJson
        }))
    }
  })

  implicit val statisticsReport_writer: RootJsonFormat[StatisticsReport] = lift(new RootJsonWriter[StatisticsReport] {
    override def write(obj: StatisticsReport): JsObject = JsObject(
      "methods" -> JsNumber(obj.nOfMethods),
      "functions" -> JsNumber(obj.nOfFunctions),
      "predicates" -> JsNumber(obj.nOfPredicates),
      "domains" -> JsNumber(obj.nOfDomains),
      "fields" -> JsNumber(obj.nOfFields))
  })

  implicit val programOutlineReport_writer: RootJsonFormat[ProgramOutlineReport] = lift(new RootJsonWriter[ProgramOutlineReport] {
    override def write(obj: ProgramOutlineReport): JsObject = JsObject("members" -> JsArray(obj.members.map(_.toJson).toVector))
  })

  implicit val viperAtomicType_writer: RootJsonFormat[viper.silver.ast.AtomicType] = lift(new RootJsonWriter[AtomicType] {
    override def write(obj: AtomicType): JsValue = obj match {
      case viper.silver.ast.Int => JsString("Int")
      case viper.silver.ast.Bool => JsString("Bool")
      case viper.silver.ast.Perm => JsString("Perm")
      case viper.silver.ast.Ref => JsString("Ref")
      case viper.silver.ast.InternalType => JsString("Internal")
      case viper.silver.ast.Wand => JsString("Wand")
      case viper.silver.ast.BackendType(viperName, interpretations) =>
        JsObject("viperName" -> JsString(viperName), "interpretations" -> interpretations.toJson)
    }
  })

  implicit val viperGenericType_writer: RootJsonFormat[viper.silver.ast.GenericType] = lift(new RootJsonWriter[GenericType] {
    override def write(obj: GenericType): JsValue = if (obj.isConcrete) {
      // If this is a concrete type instantiation, we serialize it concretely
      obj match {
        case col: CollectionType =>
          JsObject("collection" -> JsString(col.genericName), "elements" -> col.elementType.toJson)
        case MapType(keyType, valueType) =>
          JsObject("collection" -> JsString("Map"), "keys" -> keyType.toJson, "values" -> valueType.toJson)
        case DomainType(domainName, partialTypVarsMap) =>
          JsObject("collection" -> JsString(domainName), "typeParams" -> JsArray(partialTypVarsMap.values.map(_.toJson).toVector))
      }
    } else {
      // If this is not a concrete type instantiation, we serialize it generically
      JsString(obj.genericName)
    }
  })

  implicit val viperType_writer: RootJsonFormat[ViperType] = lift(new RootJsonWriter[ViperType] {
    override def write(obj: ViperType): JsObject = obj match {
      case atomic: AtomicType =>
        JsObject("kind" -> JsString("atomic"), "typename" -> atomic.toJson)
      case generic: GenericType =>
        JsObject("kind" -> JsString("generic"), "typename" -> generic.toJson, "isConcrete" -> JsBoolean(generic.isConcrete))
      case ext: ExtensionType =>
        JsObject("kind" -> JsString("extension"), "typename" -> JsString(ext.toString()))
      case weird_type =>
        /** TODO: check why trait [[viper.silver.ast.Type]] is not sealed */
        JsObject("kind" -> JsString("weird_type"), "typename" -> JsString(weird_type.toString()))
    }
  })

  implicit val symbolType_writer: RootJsonFormat[SymbolKind] = lift(new RootJsonWriter[SymbolKind] {
    override def write(obj: SymbolKind): JsObject = obj match {
      case typed_symbol: TypedSymbol =>
        JsObject("name" -> JsString(typed_symbol.name), "viperType" -> typed_symbol.viperType.toJson)
      case untyped_symbol =>
        JsObject("name" -> JsString(untyped_symbol.name))
    }
  })

  /** Legacy marshaller format. Using three different position types in one case class is ugly, but a valid
    * workaround for handling all cases of AST construction. If you want to try to improve/refactor, see
    * [[viper.server.utility.ProgramDefinitionsProvider]] for the usage Definition.
    */
  implicit val definition_writer: RootJsonFormat[Definition] = lift(new RootJsonWriter[Definition] {
    override def write(obj: Definition): JsObject = JsObject(
      "name" -> JsString(obj.name),
      "type" -> obj.typ.toJson,
      "location" -> obj.location.toJson,
      "scopeStart" -> (obj.scope match {
        case Some(s) => JsString(s"${s.start.line}:${s.start.column}")
        case _ => JsString("global")
      }),
      "scopeEnd" -> (obj.scope match {
        case Some(s) if s.end.isDefined => JsString(s"${s.end.get.line}:${s.end.get.column}")
        case _ => JsString("global")
      }))
  })

  implicit val programDefinitionsReport_writer: RootJsonFormat[ProgramDefinitionsReport] = lift(new RootJsonWriter[ProgramDefinitionsReport] {
    override def write(obj: ProgramDefinitionsReport): JsObject = JsObject(
      "definitions" -> JsArray(obj.definitions.map(_.toJson).toVector))
  })

  implicit val symbExLogReport_writer: RootJsonFormat[ExecutionTraceReport] = lift(new RootJsonWriter[ExecutionTraceReport] {
    override def write(obj: ExecutionTraceReport): JsObject = obj match {
      case ExecutionTraceReport(members: Seq[MemberSymbExLog] @unchecked, axioms: List[Term] @unchecked, functionPostAxioms: List[Term] @unchecked) =>
        JsObject(
          "members" -> SymbExLogReportWriter.toJSON(members, LogConfig.default()),
          "axioms" -> JsArray(axioms.map(TermWriter.toJSON).toVector),
          "functionPostAxioms" -> JsArray(functionPostAxioms.map(TermWriter.toJSON).toVector),
          "macros" -> JsArray(members.flatMap(m => m.macros().map(m => {
            JsObject(
              "macro" -> TermWriter.toJSON(m._1),
              "body" -> TermWriter.toJSON(m._2)
            )
          })).toVector)
        )
    }
  })

  implicit val stackTraceElement_writer: RootJsonFormat[StackTraceElement] = lift(new RootJsonWriter[java.lang.StackTraceElement] {
    override def write(obj: java.lang.StackTraceElement): JsString = JsString(obj.toString)
  })

  implicit val exceptionReport_writer: RootJsonFormat[ExceptionReport] = lift(new RootJsonWriter[ExceptionReport] {
    override def write(obj: ExceptionReport): JsObject = JsObject(
      "message" -> JsString(obj.e.toString),
      "stacktrace" -> JsArray(obj.e.getStackTrace.map(_.toJson).toVector))
  })

  implicit val invalidArgumentsReport_writer: RootJsonFormat[InvalidArgumentsReport] = lift(new RootJsonWriter[InvalidArgumentsReport] {
    override def write(obj: InvalidArgumentsReport): JsObject = JsObject(
      "tool" -> JsString(obj.tool_signature),
      "errors" -> JsArray(obj.errors.map(_.toJson).toVector))
  })

  implicit val backendSubProcessReport_writer: RootJsonFormat[BackendSubProcessReport] = lift(new RootJsonWriter[BackendSubProcessReport] {
    override def write(obj: BackendSubProcessReport): JsObject = JsObject(
      "tool" -> JsString(obj.tool_signature),
      "process_exe" -> JsString(obj.process_exe),
      "phase" -> JsString(obj.phase.toString),
      "pid" -> (obj.pid_maybe match {
        case Some(pid) => JsNumber(pid)
        case None => JsNull
      }))
  })

  implicit val dependency_writer: RootJsonFormat[Dependency] = lift(new RootJsonWriter[Dependency] {
    override def write(obj: Dependency): JsObject = JsObject(
      "name" -> JsString(obj.name),
      "version" -> JsString(obj.version),
      "location" -> JsString(obj.location))
  })

  implicit val externalDependenciesReport_writer: RootJsonFormat[ExternalDependenciesReport] = lift(new RootJsonWriter[ExternalDependenciesReport] {
    override def write(obj: ExternalDependenciesReport): JsArray = JsArray(obj.deps.map(_.toJson).toVector)
  })

  implicit val warningsDuringParsing_writer: RootJsonFormat[WarningsDuringParsing] = lift(new RootJsonWriter[WarningsDuringParsing] {
    override def write(obj: WarningsDuringParsing): JsArray = JsArray(obj.warnings.map(_.asInstanceOf[AbstractError].toJson).toVector)
  })

  implicit val warningsDuringTypechecking_writer: RootJsonFormat[WarningsDuringTypechecking] = lift(new RootJsonWriter[WarningsDuringTypechecking] {
    override def write(obj: WarningsDuringTypechecking): JsArray = JsArray(obj.warnings.map(_.asInstanceOf[AbstractError].toJson).toVector)
  })

  implicit val warningsDuringVerification_writer: RootJsonFormat[WarningsDuringVerification] = lift(new RootJsonWriter[WarningsDuringVerification] {
    override def write(obj: WarningsDuringVerification): JsArray = JsArray(obj.warnings.map(_.asInstanceOf[AbstractError].toJson).toVector)
  })

  implicit val simpleMessage_writer: RootJsonFormat[SimpleMessage] = lift(new RootJsonWriter[SimpleMessage] {
    override def write(obj: SimpleMessage): JsObject = JsObject("text" -> JsString(obj.text))
  })

  implicit val pongMessage_writer: RootJsonFormat[PongMessage] = lift(new RootJsonWriter[PongMessage] {
    override def write(obj: PongMessage): JsObject = JsObject("msg" -> JsString(obj.text))
  })

  implicit val quantifierInstantiationsMessage_writer: RootJsonFormat[QuantifierInstantiationsMessage] = lift(new RootJsonWriter[QuantifierInstantiationsMessage] {
    override def write(obj: QuantifierInstantiationsMessage): JsObject = JsObject(
      "quantifier" -> JsString(obj.quantifier),
      "instantiations" -> JsNumber(obj.instantiations),
      "max_gen" -> JsNumber(obj.max_gen),
      "max_cost" -> JsNumber(obj.max_cost),
      )
  })

  implicit val quantifierChosenTriggersMessage_writer: RootJsonFormat[QuantifierChosenTriggersMessage] = lift(new RootJsonWriter[QuantifierChosenTriggersMessage] {
    override def write(obj: QuantifierChosenTriggersMessage): JsObject = JsObject(
      "quantifier_type" -> JsString(obj.quant_type),
      "quantifier" -> JsString(obj.quantifier.toString),
      "triggers" -> JsArray(obj.triggers.map((trigger) => JsArray(trigger.exps.map((exp) => JsString(exp.toString)).toVector)).toVector)
      )
  })

  implicit val verificationTerminationMessage_writer: RootJsonFormat[VerificationTerminationMessage] = lift(new RootJsonWriter[VerificationTerminationMessage] {
    override def write(obj: VerificationTerminationMessage): JsObject = JsObject(
      "msg" -> JsString(obj.name)
      )
  })

  implicit val message_writer: RootJsonFormat[Message] = lift(new RootJsonWriter[Message] {
    override def write(obj: Message): JsValue = JsObject(
      "msg_type" -> JsString(obj.name),
      "msg_body" -> (obj match {
        case p: AstConstructionResultMessage => p.toJson
        case a: VerificationResultMessage => a.toJson
        case s: StatisticsReport => s.toJson
        case o: ProgramOutlineReport => o.toJson
        case d: ProgramDefinitionsReport => d.toJson
        case e: ExecutionTraceReport => e.toJson
        case x: ExceptionReport => x.toJson
        case i: InvalidArgumentsReport => i.toJson
        case b: BackendSubProcessReport => b.toJson
        case r: ExternalDependenciesReport => r.toJson
        case f: WarningsDuringParsing => f.toJson
        case f: WarningsDuringTypechecking => f.toJson
        case m: SimpleMessage => m.toJson
        case q: QuantifierInstantiationsMessage => q.toJson
        case q: QuantifierChosenTriggersMessage => q.toJson
        case v: VerificationTerminationMessage => v.toJson
        case p: PProgramReport => p.semanticAnalysisSuccess.toJson
        case w: WarningsDuringVerification => w.toJson
      }))
  })

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = {
    val start = ByteString("")
    val between = ByteString("\n")
    val end = ByteString("")

    val compactArrayRendering: Flow[ByteString, ByteString, NotUsed] = Flow[ByteString].intersperse(start, between, end)
    // Method withFramingRendererFlow: Java DSL overrides Scala DSL. Who knows why? Use .asJava as a workaround.
    EntityStreamingSupport.json().withFramingRendererFlow( compactArrayRendering.asJava )
  }
}
