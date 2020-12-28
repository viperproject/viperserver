// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

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
