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
import viper.silicon.SymbLog
import viper.silicon.state.terms.Term
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

  implicit val position_writer: RootJsonFormat[Position] = lift(new RootJsonWriter[Position] {
    override def write(obj: Position): JsValue = JsObject(
      "file" -> (if (obj.file != null) {
        //FIXME this hack is needed due to the following bug in Silver: https://github.com/viperproject/silver/issues/253
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
        "position" -> (obj.pos match {
          case src_pos: Position => src_pos.toJson
          case no_pos => JsString(no_pos.toString)
        }))
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
        case e: VerificationError if e.counterexample.isDefined =>
          JsObject(
            "tag" -> JsString(obj.fullId),
            "text" -> JsString(obj.readableMessage),
            "position" -> (obj.pos match {
              case src_pos: Position => src_pos.toJson
              case no_pos => JsString(no_pos.toString)
            }),
            "cached" -> JsBoolean(obj.cached),
            "counterexample" -> e.counterexample.get.toJson)
        case _ =>
          JsObject(
            "tag" -> JsString(obj.fullId),
            "text" -> JsString(obj.readableMessage),
            "position" -> (obj.pos match {
              case src_pos: Position => src_pos.toJson
              case no_pos => JsString(no_pos.toString)
            }),
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

  implicit val astConstructionMessage_writer = lift(new RootJsonWriter[AstConstructionResultMessage] {
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
    override def write(obj: StatisticsReport) = JsObject(
      "methods" -> JsNumber(obj.nOfMethods),
      "functions" -> JsNumber(obj.nOfFunctions),
      "predicates" -> JsNumber(obj.nOfPredicates),
      "domains" -> JsNumber(obj.nOfDomains),
      "fields" -> JsNumber(obj.nOfFields))
  })

  implicit val programOutlineReport_writer: RootJsonFormat[ProgramOutlineReport] = lift(new RootJsonWriter[ProgramOutlineReport] {
    override def write(obj: ProgramOutlineReport) = JsObject("members" -> JsArray(obj.members.map(_.toJson).toVector))
  })

  /** Legacy marshaller format. Using three different position types in one case class is ugly, but a valid
    * workaround for handling all cases of AST construction. If you want to try to improve/refactor, see
    * [[ViperBackend.collectDefinitions]] for the usage Definition.
    */
  implicit val definition_writer: RootJsonFormat[Definition] = lift(new RootJsonWriter[Definition] {
    override def write(obj: Definition) = JsObject(
      "name" -> JsString(obj.name),
      "type" -> JsString(obj.typ),
      "location" -> (obj.location match {
        case p:Position => p.toJson
        case _ => JsString("<undefined>")
      }),
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
    override def write(obj: ProgramDefinitionsReport) = JsObject(
      "definitions" -> JsArray(obj.definitions.map(_.toJson).toVector))
  })

  implicit val symbExLogReport_writer: RootJsonFormat[ExecutionTraceReport] = lift(new RootJsonWriter[ExecutionTraceReport] {
    override def write(obj: ExecutionTraceReport) = obj match {
      case ExecutionTraceReport(members: List[SymbLog], axioms: List[Term], functionPostAxioms: List[Term]) =>
        JsObject(
          "members" -> JsArray(members.map(m => SymbExLogReportWriter.toJSON(m.main)).toVector),
          "axioms" -> JsArray(axioms.map(TermWriter.toJSON).toVector),
          "functionPostAxioms" -> JsArray(functionPostAxioms.map(TermWriter.toJSON).toVector),
          "macros" -> JsArray(members.flatMap(m => m.macros().map(m => {
            JsObject(
              "macro" -> TermWriter.toJSON(m._1),
              "body" -> TermWriter.toJSON(m._2)
            )
          })).toVector)
        )

      case ExecutionTraceReport(members, axioms, functionPostAxioms) =>
        JsObject(
          "members" -> JsArray(members.map(m => JsString(m.toString)).toVector),
          "axioms" -> JsArray(axioms.map(a => JsString(a.toString)).toVector),
          "functionPostAxioms" -> JsArray(functionPostAxioms.map(a => JsString(a.toString)).toVector)
        )
    }
  })

  implicit val stackTraceElement_writer: RootJsonFormat[StackTraceElement] = lift(new RootJsonWriter[java.lang.StackTraceElement] {
    override def write(obj: java.lang.StackTraceElement) = JsString(obj.toString)
  })

  implicit val exceptionReport_writer: RootJsonFormat[ExceptionReport] = lift(new RootJsonWriter[ExceptionReport] {
    override def write(obj: ExceptionReport) = JsObject(
      "message" -> JsString(obj.e.toString),
      "stacktrace" -> JsArray(obj.e.getStackTrace.map(_.toJson).toVector))
  })

  implicit val invalidArgumentsReport_writer: RootJsonFormat[InvalidArgumentsReport] = lift(new RootJsonWriter[InvalidArgumentsReport] {
    override def write(obj: InvalidArgumentsReport) = JsObject(
      "tool" -> JsString(obj.tool_signature),
      "errors" -> JsArray(obj.errors.map(_.toJson).toVector))
  })

  implicit val dependency_writer: RootJsonFormat[Dependency] = lift(new RootJsonWriter[Dependency] {
    override def write(obj: Dependency) = JsObject(
      "name" -> JsString(obj.name),
      "version" -> JsString(obj.version),
      "location" -> JsString(obj.location))
  })

  implicit val externalDependenciesReport_writer: RootJsonFormat[ExternalDependenciesReport] = lift(new RootJsonWriter[ExternalDependenciesReport] {
    override def write(obj: ExternalDependenciesReport) = JsArray(obj.deps.map(_.toJson).toVector)
  })

  implicit val warningsDuringParsing_writer: RootJsonFormat[WarningsDuringParsing] = lift(new RootJsonWriter[WarningsDuringParsing] {
    override def write(obj: WarningsDuringParsing) = JsArray(obj.warnings.map(_.asInstanceOf[AbstractError].toJson).toVector)
  })

  implicit val warningsDuringTypechecking_writer: RootJsonFormat[WarningsDuringTypechecking] = lift(new RootJsonWriter[WarningsDuringTypechecking] {
    override def write(obj: WarningsDuringTypechecking) = JsArray(obj.warnings.map(_.asInstanceOf[AbstractError].toJson).toVector)
  })

  implicit val simpleMessage_writer: RootJsonFormat[SimpleMessage] = lift(new RootJsonWriter[SimpleMessage] {
    override def write(obj: SimpleMessage) = JsObject("text" -> JsString(obj.text))
  })

  implicit val pongMessage_writer: RootJsonFormat[PongMessage] = lift(new RootJsonWriter[PongMessage] {
    override def write(obj: PongMessage) = JsObject("msg" -> JsString(obj.text))
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
        case r: ExternalDependenciesReport => r.toJson
        case f: WarningsDuringParsing => f.toJson
        case f: WarningsDuringTypechecking => f.toJson
        case m: SimpleMessage => m.toJson
      }))
  })

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = {
    val between = ByteString("\n")
    val compactArrayRendering: Flow[ByteString, ByteString, NotUsed] = Flow[ByteString].intersperse(between)
    // Method withFramingRendererFlow: Java DSL overrides Scala DSL. Who knows why? Use .asJava as a workaround.
    EntityStreamingSupport.json().withFramingRendererFlow( compactArrayRendering.asJava )
  }
}
