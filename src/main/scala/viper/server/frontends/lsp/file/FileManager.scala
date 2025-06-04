// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2023 ETH Zurich.

package viper.server.frontends.lsp.file

import java.net.URI
import java.nio.file.{Path, Paths}
import viper.server.core.VerificationExecutionContext
import viper.server.frontends.lsp
import viper.silver.ast
import scala.concurrent.Future

case class PathInfo(file_uri: String) {
  val uri: URI = URI.create(file_uri)
  val path: Path = Paths.get(uri)
  val filename: String = path.getFileName.toString
}

trait ManagesLeaf {
  /** If this file is a root, this is the relevant lsp data for the file. If
   * this file is a leaf, this `LeafManager` is disabled and the lsp data is
   * handled by a duplicated `LeafManager` owned by the root of the project. */
  val root: LeafManager

  def file: PathInfo = root.file
  def content: lsp.file.FileContent = root.content
  def coordinator: lsp.ClientCoordinator = root.coordinator

  def file_uri: String = file.file_uri
  // The `file_uri` we have from VS Code has the `:` after the windows drive
  // letter encoded as `%3A`. Viper doesn't do this, so hack here to allow for
  // string comparisons.
  def unescape(uri: String): String = uri.replaceFirst("%3A", ":")
  def filename: String = file.filename
  def path: Path = file.path

  def resetDiagnostics(first: Boolean): Unit
  def resetContainers(first: Boolean): Unit
  def addDiagnostic(first: Boolean)(vs: Seq[Diagnostic]): Unit

  def getInFuture[T](f: => T): Future[T]
}

case class FileManager(root: LeafManager)(implicit executor: VerificationExecutionContext) extends MessageHandler {
  override val ec: VerificationExecutionContext = executor
  var isOpen: Boolean = true

  def close(): Unit = {
    teardownProject()
    stopRunningVerification()
  }
}

object FileManager {
  def apply(file_uri: String, coordinator: lsp.ClientCoordinator, c: Option[String])(implicit executor: VerificationExecutionContext): FileManager = {
    val file = PathInfo(file_uri)
    // In some cases the IDE doesn't send the content to use, then load it ourselves.
    // Note that the file may have been deleted in the meantime!
    val fileContent = c.getOrElse(ast.utility.DiskLoader.loadContent(file.path).getOrElse(""))
    val content = lsp.file.FileContent(file.path, fileContent)
    new FileManager(LeafManager(file, coordinator, content))
  }
}
