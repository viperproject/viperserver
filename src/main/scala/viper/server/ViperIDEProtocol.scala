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
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}
import viper.silver.reporter._
import viper.silver.verifier.{AbstractError, Failure}


object ViperIDEProtocol extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  import spray.json._

  final case class VerificationRequestAccept(id: Int)
  final case class VerificationRequestReject(msg: String)
  final case class ServerStopConfirmed(msg: String)


  implicit val verReqAccept_format: RootJsonFormat[VerificationRequestAccept] = jsonFormat1(VerificationRequestAccept)
  implicit val verReqReject_format = jsonFormat1(VerificationRequestReject)
  implicit val serverStopConfirmed_format = jsonFormat1(ServerStopConfirmed)


  // Implicit conversions for reporter.Message.

  implicit object file_format extends JsonFormat[File] {
    override def write(obj: File): JsValue = obj.getFileName.toJson

    override def read(json: JsValue): File = try {
      java.nio.file.Paths.get(json.toString)
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Invalid file path: `$json`. Cannot convert json format to File.")
    }
  }

  implicit val position_format = lift(new JsonWriter[Position] {
    override def write(obj: Position): JsValue =
      JsObject(
        "file" -> obj.file.toJson,
        "start" -> JsString(s"${obj.line}:${obj.column}"),
        obj.end match {
          case Some(end_pos) =>
            "end" -> JsString(s"${end_pos.line}:${end_pos.column}")
        })
  })

  implicit val optionAny_format = lift(new JsonWriter[Option[Any]] {
    override def write(obj: Option[Any]): JsValue = obj match {
      case Some(stuff) => stuff.toString.toJson
      case _ => JsObject.empty
    }
  })

  implicit val abstractError_format = lift(new JsonWriter[AbstractError] {
    override def write(obj: AbstractError) = {
      val obj_1 = JsObject(
        "cached" -> JsBoolean(obj.cached),
        "tag" -> JsString(obj.fullId),
        "message" -> JsString(obj.readableMessage)
      )
      JsObject(
        obj_1.fields +
          (obj match {
            case hlc: HasLineColumn =>
              "start" -> JsString(s"${hlc.line}:${hlc.column}")
          }) +
          (obj match {
            case abs: AbstractSourcePosition =>
              abs.end match {
                case Some(end_pos) =>
                  "end" -> JsString(s"${end_pos.line}:${end_pos.column}")
              }
          }) +
          (obj match {
            case abs: AbstractSourcePosition =>
              "file" -> JsString(abs.file.toAbsolutePath.toString)
          })
      )
    }
  })

  implicit val entity_format = lift(new JsonWriter[Entity] {
    override def write(obj: Entity) = JsObject(
      "type" -> JsString(obj.getClass.toString),
      "name" -> JsString(obj.name))
  })

  implicit val failure_format = lift(new JsonWriter[Failure] {
    override def write(obj: Failure) =
      JsObject(
        "type" -> JsString("Error"),
        "errors" -> JsArray(obj.errors.map(_.toJson).toVector))
  })

  //implicit val successMessage_format = jsonFormat2(SuccessMessage.apply)
  implicit val successMessage_format = lift(new JsonWriter[SuccessMessage] {
    override def write(obj: SuccessMessage) = {
      JsObject(
        "entity" -> obj.entity.toJson,
        "time" -> obj.verificationTime.toJson
      )
    }
  })

  implicit val failureMessage_format = jsonFormat3(FailureMessage.apply)
  implicit val symbExLogReport_format = lift(new JsonWriter[SymbExLogReport] {
    override def write(obj: SymbExLogReport) = {
      val obj_1 = JsObject(
        "entity" -> obj.entity.toJson,
        "timestamp" -> obj.timestamp.toJson
      )
      JsObject(
        obj_1.fields +
          (obj.stuff match {
            case Some(stuff) =>
              "stuff" -> JsString("<json transformer not implemented>")
          })
      )
    }
  })
  implicit val pongMessage_format = jsonFormat1(PongMessage.apply)

  implicit val message_format = lift(new JsonWriter[Message] {
    override def write(obj: Message): JsValue = obj match {
      case a: SuccessMessage  => a.toJson
      case b: FailureMessage  => b.toJson
      case c: SymbExLogReport => c.toJson
      case d: PongMessage     => d.toJson
    }
  })

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = {
    //val start = ByteString("[")
    val between = ByteString("\n")
    //val end = ByteString("]")

    val compactArrayRendering: Flow[ByteString, ByteString, NotUsed] = Flow[ByteString].intersperse(between)
    // Method withFramingRendererFlow: Java DSL overrides Scala DSL. Who knows why? Use .asJava as a workaround.
    EntityStreamingSupport.json().withFramingRendererFlow( compactArrayRendering.asJava )
  }
}
