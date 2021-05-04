// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.frontends.lsp

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.Position


object Common {
  var viperFileEndings: Array[String] = Array("vpr", "sil")

  def uriFromString(uri: String): URI = {
    URI.create(uri)
  }

  def uriToPath(uri: URI): Path = {
    Paths.get(uri)
  }

  def filenameFromUri(uri: String): String = {
    Paths.get(uri).getFileName.toString
  }

  def refreshEndings(): CompletableFuture[Void] = {
    Coordinator.client.requestVprFileEndings().thenAccept((s: Array[String]) => {
      viperFileEndings = s
    }).exceptionally(e => {
      Log.debug(s"GetViperFileEndings request was rejected by the client: $e")
      null
    })
  }

  def isViperSourceFile(uri: String): CompletableFuture[Boolean] = {
    def areEndingsDefined: Boolean = viperFileEndings != null && viperFileEndings.nonEmpty
    if (areEndingsDefined) {
      val endingMatches = viperFileEndings.exists(ending => uri.endsWith(ending))
      CompletableFuture.completedFuture(endingMatches)
    } else { // need to refresh endings and then compare
      Log.debug("Refreshing the viper file endings.")
      refreshEndings().thenApply(_ => {
        if (areEndingsDefined) {
          println("Endings are defined!")
          viperFileEndings.foreach(s => s.drop(1))
          viperFileEndings.exists(ending => uri.endsWith(ending))
        } else {
          println("Endings not are defined!")
          false
        }
      })
    }
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
}