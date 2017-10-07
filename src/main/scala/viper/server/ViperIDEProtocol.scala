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
import spray.json.{DefaultJsonProtocol, JsValue, JsonWriter, RootJsonFormat}
import viper.silver.reporter._
import viper.silver.verifier.{AbstractError, Failure, Success, VerificationResult}


object ViperIDEProtocol extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  import spray.json._

  final case class VerificationRequestAccept(id: Int)
  final case class VerificationRequestReject(msg: String)
  final case class ServerStopConfirmed(msg: String)


  implicit val verReqAccept_format: RootJsonFormat[VerificationRequestAccept] = jsonFormat1(VerificationRequestAccept)
  implicit val verReqReject_format = jsonFormat1(VerificationRequestReject)
  implicit val serverStopConfirmed_format = jsonFormat1(ServerStopConfirmed)


  // Implicit conversions for reporter.Message.

  implicit object file_format extends RootJsonFormat[File] {
    override def write(obj: File): JsValue = JsString(obj.getFileName.toString)

    override def read(json: JsValue): File = try {
      java.nio.file.Paths.get(json.toString)
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Invalid file path: `$json`. Cannot convert json format to File.")
    }
  }

  implicit val position_writer = lift(new RootJsonWriter[Position] {
    override def write(obj: Position): JsValue = JsObject(
        "file" -> obj.file.toJson,
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
    import viper.silver.ast._

    override def write(obj: Entity) = {
      val entity_type = obj match {
        case fi: Field => "field"
        case d: Domain => "domain"
        case p: Predicate => "predicate"
        case fu: Function => "function"
        case m: Method => "method"
        case _ => s"<not a Viper top-lever entity: ${obj.getClass.getCanonicalName}>"
      }
      JsObject(
        "type" -> JsString(entity_type),
        "name" -> JsString(obj.name))
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

  implicit val entityFailureMessage_format = lift(new RootJsonWriter[EntityFailureMessage] {
    override def write(obj: EntityFailureMessage): JsValue = JsObject(
      "entity" -> obj.entity.toJson,
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

  implicit val pongMessage_format = jsonFormat1(PongMessage.apply)

  implicit val message_writer = lift(new RootJsonWriter[Message] {
    override def write(obj: Message): JsValue = JsObject(
      "msg_type" -> JsString(obj.toString),
      "msg_body" -> (obj match {
        case a: VerificationResultMessage => a.toJson
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
