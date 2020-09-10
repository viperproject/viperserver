package viper.server.vsi

import viper.silver.reporter.Message

trait Envelope{
}

trait Letter {
  type A
  var message: A = _

  def pack(m: A) = {
    message = m
  }

  def unpack(): A = {
    message
  }
}

//class Letter[A](m: A) {
//  val message: A = m
//
//  def unpack(): A = {
//    message
//  }
//}

class SLetter() extends Letter {
 type A = Message
}