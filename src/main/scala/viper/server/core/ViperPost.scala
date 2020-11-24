package viper.server.core

import viper.server.vsi.{Envelope, Post}
import viper.silver.reporter.Message

trait ViperPost extends Post {
  override type A = Message

  override def unpack(e: Envelope): Message = {
    e match {
      case ViperEnvelope(m) => m
    }
  }

  override def pack(m: Message): Envelope = ViperEnvelope(m)
}
