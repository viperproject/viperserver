package viper.server.core

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import viper.server.utility.{AstGenerator, ReformatterAstGenerator}
import viper.silver.ast.Program
import viper.silver.ast.utility.DiskLoader
import viper.silver.logger.ViperStdOutLogger
import viper.silver.parser.{PProgram, ReformatPrettyPrinter}

import java.nio.file.{NoSuchFileException, Path}


class ReformatterTest extends AnyWordSpec with Matchers with ScalatestRouteTest {

  private val expressions = "src/test/resources/viper/reformatter/expressions.vpr"
  private val expressions_expected = "src/test/resources/viper/reformatter/expressions_expected.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")

  "ReformatterAstGenerator" should {
    var ast_gen: ReformatterAstGenerator = null
    s"should be instantiated without errors" in {
      ast_gen = new ReformatterAstGenerator(console_logger.get)
    }
    
    def check_inner(name: String): Unit = {
      val prefix = "src/test/resources/viper/reformatter/"
      val input_path = prefix + name + ".vpr"
      val expected_path = prefix + name + "_expected.vpr"

      val ast = ast_gen.generateViperParseAst(input_path).get
      val reformatted = ReformatPrettyPrinter.reformatProgram(ast);
      val actual = DiskLoader.loadContent(Path.of(expected_path)).get
      assert(reformatted == actual)
    }

    s"adts" in {
      check_inner("adts")
    }

    s"domains" in {
      check_inner("domains")
    }
    
    s"expressions" in {
      check_inner("expressions")
    }

    s"fields" in {
      check_inner("fields")
    }

    s"functions" in {
      check_inner("functions")
    }

    s"macros" in {
      check_inner("macros")
    }

    s"methods" in {
      check_inner("methods")
    }

    s"predicates" in {
      check_inner("predicates")
    }

    s"trailing_comment" in {
      check_inner("trailing_comment")
    }

    s"not_working" in {
      check_inner("not_working")
    }

  }
}
