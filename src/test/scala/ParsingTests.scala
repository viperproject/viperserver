// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

import java.nio.file.NoSuchFileException

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import viper.server.utility.AstGenerator
import viper.silver.ast.Program
import viper.silver.logger.ViperStdOutLogger


class ParsingTests extends WordSpec with Matchers with ScalatestRouteTest {

  private val verifiableFile = "src/test/resources/viper/let.vpr"
  private val emptyFile = "src/test/resources/viper/empty.vpr"
  private val sumFile = "src/test/resources/viper/sum_method.vpr"
  private val typeErrorFile = "src/test/resources/viper/type_error.vpr"
  private val parseErrorFile = "src/test/resources/viper/parse_error.vpr"
  private val nonExistingFile = "src/test/resources/viper/kajldksfnk.vpr"

  private val console_logger = ViperStdOutLogger("parsingTest logger", "ALL")

  "AstGenerator" should {
    var ast_gen: AstGenerator = null
    s"should be instantiated without errors" in {
      ast_gen = new AstGenerator(console_logger.get)
    }

    var test_ast: Option[Program] = null
    s"have 'generateViperAst()' return an defined option for the file 'sum_method.vpr'" in {
      test_ast = ast_gen.generateViperAst(sumFile)
      assert(test_ast.isDefined)
    }

    s"have 'generateViperAst()' return an defined option for the file 'let.vpr'" in {
      test_ast = ast_gen.generateViperAst(verifiableFile)
      assert(test_ast.isDefined)
    }

    s"have 'generateViperAst()' return an defined option for the file 'empty.vpr'" in {
      test_ast = ast_gen.generateViperAst(emptyFile)
      assert(test_ast.isDefined)
    }

    s"have 'generateViperAst()' return an empty option for the file 'type_error.vpr'" in {
      test_ast = ast_gen.generateViperAst(typeErrorFile)
      assert(!test_ast.isDefined)
    }

    s"have 'generateViperAst()' return an empty option for the file 'parse_error.vpr'" in {
      test_ast = ast_gen.generateViperAst(parseErrorFile)
      assert(!test_ast.isDefined)
    }

    s"have 'generateViperAst()' throw an exception for a non-existing file." in {
      assertThrows[NoSuchFileException] { // Result type: Assertion
        test_ast = ast_gen.generateViperAst(nonExistingFile)
      }
    }
  }
}
