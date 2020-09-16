package viper.server.vsi

trait Envelope {}

trait Packer {
  type A

  def pack(m: A): Envelope
}

trait Unpacker {
  type A

  def unpack(m: Envelope): A
}