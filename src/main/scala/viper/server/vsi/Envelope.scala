package viper.server.vsi

trait Envelope{
}


trait Letter {
  type M

  def unpack(): M
}


