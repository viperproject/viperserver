/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.NotUsed
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import spray.json.DefaultJsonProtocol
import viper.silver.reporter._
import viper.silver.verifier.{AbstractError, Failure, Success, VerificationResult}


object ViperIDEProtocol extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  import spray.json._

  final case class VerificationRequestAccept(id: Int)
  final case class VerificationRequestReject(msg: String)
  final case class ServerStopConfirmed(msg: String)
  final case class JobDiscardAccept(msg: String)
  final case class JobDiscardReject(msg: String)


  implicit val verReqAccept_format = jsonFormat1(VerificationRequestAccept)
  implicit val verReqReject_format = jsonFormat1(VerificationRequestReject)
  implicit val serverStopConfirmed_format = jsonFormat1(ServerStopConfirmed)
  implicit val jobDiscardAccept_format = jsonFormat1(JobDiscardAccept)
  implicit val jobDiscardReject_format = jsonFormat1(JobDiscardReject)


  // Implicit conversions for reporter.Message.

  implicit object file_format extends RootJsonFormat[File] {
    override def write(obj: File): JsValue = JsString(obj.toAbsolutePath.toString)

    override def read(json: JsValue): File = try {
      java.nio.file.Paths.get(json.toString)
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Invalid file path: `$json`. Cannot convert json format to File.")
    }
  }

  implicit val position_writer = lift(new RootJsonWriter[Position] {
    override def write(obj: Position): JsValue = JsObject(
        "file" -> (if (obj.file != null) {
          //FIXME this hack is needed due to the following bug in Silver: https://bitbucket.org/viperproject/silver/issues/232
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

  implicit val optionAny_writer = lift(new RootJsonWriter[Option[Any]] {
    override def write(obj: Option[Any]): JsValue = obj match {
      case Some(stuff) => stuff.toString.toJson
      case _ => JsObject.empty
    }
  })

  implicit val entity_writer = lift(new RootJsonWriter[Entity] {
    override def write(obj: Entity) = {
      val entity_type = obj match {
        case m: viper.silver.ast.Method => "method"
        case fu: viper.silver.ast.Function => "function"
        case p: viper.silver.ast.Predicate => "predicate"
        case d: viper.silver.ast.Domain => "domain"
        case fi: viper.silver.ast.Field => "field"
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

  implicit val abstractError_writer = lift(new RootJsonWriter[AbstractError] {
    override def write(obj: AbstractError) = JsObject(
      "cached" -> JsBoolean(obj.cached),
      "tag" -> JsString(obj.fullId),
      "text" -> JsString(obj.readableMessage),
      "position" -> (obj.pos match {
        case src_pos: Position => src_pos.toJson
        case no_pos => JsString(no_pos.toString)
      }))
  })

  implicit val failure_writer = lift(new RootJsonWriter[Failure] {
    override def write(obj: Failure) =
      JsObject(
        "type" -> JsString("error"),
        "errors" -> JsArray(obj.errors.map(_.toJson).toVector))
  })

  implicit val entitySuccessMessage_writer = lift(new RootJsonWriter[EntitySuccessMessage] {
    override def write(obj: EntitySuccessMessage) = {
      JsObject(
        "entity" -> obj.concerning.toJson,
        "time" -> obj.verificationTime.toJson)
    }
  })

  implicit val entityFailureMessage_writer = lift(new RootJsonWriter[EntityFailureMessage] {
    override def write(obj: EntityFailureMessage): JsValue = JsObject(
      "entity" -> obj.concerning.toJson,
      "time" -> obj.verificationTime.toJson,
      "result" -> obj.result.toJson)
  })

  implicit val overallSuccessMessage_writer = lift(new RootJsonWriter[OverallSuccessMessage] {
    override def write(obj: OverallSuccessMessage) = {
      JsObject(
        "time" -> obj.verificationTime.toJson)
    }
  })

  implicit val overallFailureMessage_writer = lift(new RootJsonWriter[OverallFailureMessage] {
    override def write(obj: OverallFailureMessage): JsValue = JsObject(
      "time" -> obj.verificationTime.toJson,
      "result" -> obj.result.toJson
    )
  })

  implicit val verificationResult_writer = lift(new RootJsonWriter[VerificationResult] {
    override def write(obj: VerificationResult): JsValue = obj match {
      case Success => JsObject("type" -> JsString("success"))
      case f@ Failure(_) => f.toJson
    }
  })

  implicit val verifResultMessage_writer = lift(new RootJsonWriter[VerificationResultMessage] {
    override def write(obj: VerificationResultMessage): JsValue = {
      val is_overall = obj match {
        case o: OverallFailureMessage => true
        case o: OverallSuccessMessage => true
        case _ => false
      }
      JsObject(
        "verifier" -> JsString(obj.verifier),
        "status" -> (obj.result match {
          case viper.silver.verifier.Success => JsString("success")
          case _ => JsString("failure")
        }),
        "kind" -> (is_overall match {
          case true => JsString("overall")
          case false => JsString("for_entity")
        }),
        "details" -> (obj match {
          case a: OverallFailureMessage => a.toJson
          case b: OverallSuccessMessage => b.toJson
          case c: EntityFailureMessage => c.toJson
          case d: EntitySuccessMessage => d.toJson
        }))
    }
  })

  implicit val statisticsReport_writer = lift(new RootJsonWriter[StatisticsReport] {
    override def write(obj: StatisticsReport) = JsObject(
      "methods" -> JsNumber(obj.nOfMethods),
      "functions"  -> JsNumber(obj.nOfFunctions),
      "predicates"  -> JsNumber(obj.nOfPredicates),
      "domains" -> JsNumber(obj.nOfDomains),
      "fields"  -> JsNumber(obj.nOfFields))
  })

  implicit val programOutlineReport_writer = lift(new RootJsonWriter[ProgramOutlineReport] {
    override def write(obj: ProgramOutlineReport) = JsObject("members" -> JsArray(obj.members.map(_.toJson).toVector))
  })

  /** Legacy marshaller format. Using three different position types in one case class is ugly, but a valid
    * workaround for handling all cases of AST construction. If you want to try to improve/refactor, see
    * [[ViperFrontend.collectDefinitions]] for the usage Definition.
    */
  implicit val definition_writer = lift(new RootJsonWriter[Definition] {
    override def write(obj: Definition) = JsObject(
      "name" -> JsString(obj.name),
      "type" -> JsString(obj.typ),
      "location" -> (obj.location match {
        case p:Position => p.toJson
        case _ => JsString("<undefined>")
      }),
      "scopeStart" -> (obj.scope match {
        case Some(s) => JsString(s.start.line + ":" + s.start.column)
        case _ => JsString("global")
      }),
      "scopeEnd" -> (obj.scope match {
        case Some(s) if s.end.isDefined => JsString(s.end.get.line + ":" + s.end.get.column)
        case _ => JsString("global")
      }))
  })

  implicit val programDefinitionsReport_writer = lift(new RootJsonWriter[ProgramDefinitionsReport] {
    override def write(obj: ProgramDefinitionsReport) = JsObject("definitions" -> JsArray(obj.definitions.map(_.toJson).toVector))
  })

  implicit val symbExLogReport_writer = lift(new RootJsonWriter[SymbExLogReport] {
    override def write(obj: SymbExLogReport) = JsObject(
      "entity" -> obj.entity.toJson,
      "timestamp" -> obj.timestamp.toJson,
      obj.stuff match {
        case Some(stuff) =>
          "stuff" -> JsString(s"<json transformer not implemented for attachment ${stuff.toString}>")
        case _ =>
          "stuff" -> JsString(s"<empty attachment>")
      })
  })

  implicit val pongMessage_writer = lift(new RootJsonWriter[PongMessage] {
    override def write(obj: PongMessage) = JsObject("msg" -> JsString(obj.msg))
  })

  implicit val message_writer = lift(new RootJsonWriter[Message] {
    override def write(obj: Message): JsValue = JsObject(
      "msg_type" -> JsString(obj.name),
      "msg_body" -> (obj match {
        case a: VerificationResultMessage => a.toJson
        case s: StatisticsReport => s.toJson
        case o: ProgramOutlineReport => o.toJson
        case d: ProgramDefinitionsReport => d.toJson
        case e: SymbExLogReport => e.toJson
        case f: PongMessage => f.toJson
      }))
  })

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = {
    val start = ByteString("")
    val between = ByteString("\n")
    val end = ByteString("")

    val compactArrayRendering: Flow[ByteString, ByteString, NotUsed] = Flow[ByteString].intersperse(between)
    // Method withFramingRendererFlow: Java DSL overrides Scala DSL. Who knows why? Use .asJava as a workaround.
    EntityStreamingSupport.json().withFramingRendererFlow( compactArrayRendering.asJava )
  }
}
