// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2024 ETH Zurich.

package viper.server.core

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import viper.server.utility.ReformatterAstGenerator
import viper.silver.ast.utility.DiskLoader
import viper.silver.logger.ViperStdOutLogger
import viper.silver.parser.ReformatPrettyPrinter

import java.nio.file.Path


class ReformatterTest extends AnyWordSpec with Matchers with ScalatestRouteTest {
  private val snippet = "src/test/resources/viper/reformat_snippet.vpr"
  private val snippet_expected = "src/test/resources/viper/reformat_snippet_expected.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")

  "ReformatterAstGenerator" should {
    var ast_gen: ReformatterAstGenerator = null
    s"should be instantiated without errors" in {
      ast_gen = new ReformatterAstGenerator(console_logger.get)
    }
    
    def check_inner(): Unit = {
      val ast = ast_gen.generateViperParseAst(snippet).get
      val reformatted = ReformatPrettyPrinter.showProgram(ast);
      val actual = DiskLoader.loadContent(Path.of(snippet_expected)).get
      assert(reformatted == actual)
    }

    s"should be able to reformat a file correctly" in {
      check_inner()
    }
  }
}
