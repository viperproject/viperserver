// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}

import org.eclipse.lsp4j.Position


object Common {

  def uriFromString(uri: String): URI = {
    URI.create(uri)
  }

  def uriToPath(uri: URI): Path = {
    Paths.get(uri)
  }

  def filenameFromUri(uri: String): String = {
    Paths.get(uri).getFileName.toString
  }

  def comparePosition(a: Position, b: Position): Int = {
    if (a == null && b == null) return 0
    if (a != null) return -1
    if (b != null) return 1
    if (a.getLine < b.getLine || (a.getLine == b.getLine && a.getCharacter < b.getCharacter)) {
      -1
    } else if (a.getLine == b.getLine && a.getCharacter == b.getCharacter) {
      0
    } else {
      1
    }
  }

  /** returns 0 if equal, 1 if v1 is bigger than v2, -1 otherwise */
  def compareSemVer(v1: String, v2: String): Int = {
    val v1Parts = v1.split('.') // single quotes to use it as string and not as regex
      .map(part => part.toInt)
    val v2Parts = v2.split('.') // single quotes to use it as string and not as regex
      .map(part => part.toInt)
    val zippedParts = v1Parts.zipAll(v2Parts, 0, 0)
    zippedParts.collectFirst {
      case (component1, component2) if component1 > component2 => 1
      case (component1, component2) if component1 < component2 => -1
    }.getOrElse(0)
  }
}
