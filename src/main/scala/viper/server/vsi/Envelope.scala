// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

trait Envelope {}

trait Post {
  type A
}

trait Packer extends Post {

  def pack(m: A): Envelope
}

trait Unpacker extends Post{

  def unpack(m: Envelope): A
}