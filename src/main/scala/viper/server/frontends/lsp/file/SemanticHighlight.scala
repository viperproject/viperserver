package viper.server.frontends.lsp.file

import viper.server.frontends.lsp
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import viper.silver.ast
import viper.silver.ast.utility.lsp.SemanticHighlight
import org.eclipse.lsp4j.Range
import viper.server.frontends.lsp.file.utility.{StageArrayContainer, SemanticHighlightTranslator, LspContainer}

trait SemanticHighlightManager[A <: SemanticHighlightManager[A]] extends CommonFileInfo[A] { this: A =>
  type SemanticHighlightContainer = StageArrayContainer.ArrayContainer[SemanticHighlight, lsp.Lsp4jSemanticHighlight]
  val semanticToken: SemanticHighlightContainer =
    LspContainer(SemanticHighlightTranslator, coordinator.client.refreshSemanticTokens)

  containers.addOne(semanticToken)

//   def getSemanticHighlights: HashMap[String, ArrayBuffer[lsp.SemanticToken]] = HashMap()

//   def handleChangeST(uri: String, range: Range, text: String): Unit = {
//     val lines = text.split("\n", -1)
//     val deltaLines = lines.length - 1 + range.getStart.getLine - range.getEnd.getLine
//     val startCharacter = if (lines.length == 1) range.getStart.getCharacter else 0
//     val deltaChars = startCharacter + lines.last.length - range.getEnd.getCharacter
//     // If the change cannot ruin the meaning of a semantic token at the start,
//     // adjust the range start to avoid overlaps with adjacent tokens
//     if (text.isEmpty || !text.head.isLetterOrDigit) {
//       range.getStart.setCharacter(range.getStart.getCharacter + 1)
//     }
//     // If the change cannot ruin the meaning of a semantic token at the end,
//     // adjust the range end to avoid overlaps with adjacent tokens
//     if (text.isEmpty || !text.last.isLetterOrDigit) {
//       range.getEnd.setCharacter(range.getEnd.getCharacter - 1)
//     }
//     // Remove overlapping semantic tokens and update positions of those after change
//     getSemanticHighlights.get(uri).map(tokens => {
//       tokens.filterInPlace(token => {
//         val cmp = token.compare(range)
//         if (cmp == 1) {
//           // Token after change so may have moved
//           if (token.start.getLine == range.getEnd.getLine) {
//             token.start.setCharacter(token.start.getCharacter + deltaChars)
//           }
//           token.start.setLine(token.start.getLine + deltaLines)
//         }
//         // Remove overlaps
//         cmp != 0
//       })
//     })
//   }

//   def receiveSemanticTokens(tokens: Seq[SemanticHighlight]) = {
//     coordinator.logger.trace(s"got semantic tokens for $filename (${tokens.length})")
//     getSemanticHighlights.clear()
//     for (t@SemanticHighlight(pos, typ, modifiers) <- tokens) {
//       if (pos.start.line != pos._end.line) {
//         coordinator.logger.error(s"Multiline semantic tokens are not supported: ${t.toString}.")
//       } else {
//         val fileStr = pos.file.toUri().toString()
//         val tokenArray = getSemanticHighlights.getOrElseUpdate(fileStr, ArrayBuffer())

//         val mod = modifiers.foldLeft(0)((acc, m) => acc | (1 << m.id))
//         val startPos = lsp.Common.toPosition(pos.start)
//         val semToken = lsp.SemanticToken(startPos, pos._end.column - pos.start.column, typ.id, mod)
//         tokenArray += semToken
//       }
//     }
//     getSemanticHighlights.values.foreach(_.sortInPlaceBy(t => (t.start.getLine, t.start.getCharacter)))
//     getSemanticHighlights.values.foreach(tokenArray => {
//       var (prevLine, prevColumn) = (0, 0)
//       tokenArray.filterInPlace(token => {
//         val remove = token.start.getLine <= prevLine && token.start.getCharacter < prevColumn
//         prevLine = token.start.getLine
//         prevColumn = token.start.getCharacter + token.len
//         if (remove) {
//           coordinator.logger.error(s"Overlapping semantic tokens are not supported: ${token.toString}.")
//         }
//         !remove
//       })
//     })
//     coordinator.client.refreshSemanticTokens()
//   }
}