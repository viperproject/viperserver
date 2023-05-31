package viper.server.frontends.lsp.file

import viper.server.frontends.lsp
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import viper.silver.ast
import viper.silver.ast.utility.lsp.SignatureHelp
import org.eclipse.lsp4j
import viper.server.frontends.lsp.file.utility.{SignatureHelpTranslator, LspContainer}

trait SignatureHelpManager[A <: SignatureHelpManager[A]] extends CommonFileInfo[A] { this: A =>
  var signatureHelpStart: Option[lsp4j.Position] = None

  // type SignatureHelpContainer = LspContainer[SignatureHelp, lsp4j.SignatureHelp, SignatureHelpTranslator.type]
  // val signatureHelp: SignatureHelpContainer = {
  //   val c: SignatureHelpContainer = LspContainer(SignatureHelpTranslator)
  //   containers.addOne(c)
  //   c
  // }
  // def signatureHelpInFile(uri: String): SignatureHelpContainer = {
  //   if (uri == file_uri) signatureHelp else coordinator.getFileManager(uri).signatureHelp
  // }
}