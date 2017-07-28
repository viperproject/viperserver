/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import akka.http.scaladsl.common.EntityStreamingSupport
import spray.json.{DefaultJsonProtocol, JsValue, JsonWriter}
import viper.silver.ast.{AbstractSourcePosition, HasLineColumn}
import viper.silver.reporter._


object ViperIDEProtocol extends akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport with DefaultJsonProtocol {

  import spray.json._

  final case class VerificationRequestAccept(id: Int)
  final case class VerificationRequestReject(msg: String)
  final case class ServerStopConfirmed(msg: String)


  implicit val verReqAccept_format = jsonFormat1(VerificationRequestAccept)
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

  implicit val entity_format = lift(new JsonWriter[Entity] {
    override def write(obj: Entity): JsValue = obj.name.toJson
  })

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

  implicit val successMessage_format = jsonFormat3(SuccessMessage.apply)
  implicit val failureMessage_format = jsonFormat4(FailureMessage.apply)
  implicit val symbExLogReport_format = jsonFormat4(SymbExLogReport.apply)
  implicit val pongMessage_format = jsonFormat1(PongMessage.apply)

  implicit val message_format = lift(new JsonWriter[Message] {
    override def write(obj: Message): JsValue = obj match {
      case a: SuccessMessage  => a.toJson
      case b: FailureMessage  => b.toJson
      case c: SymbExLogReport => c.toJson
      case d: PongMessage     => d.toJson
    }
  })

  implicit val jsonStreamingSupport = EntityStreamingSupport.json()
}
