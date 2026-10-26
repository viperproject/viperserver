// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.core

import org.scalatest.wordspec.AnyWordSpec
import viper.server.frontends.lsp.debug.dap.ConsoleCommand
import viper.server.frontends.lsp.debug.dap.ConsoleCommand._

class ConsoleCommandSpec extends AnyWordSpec {

  "Debug Console commands" should {

    "be parsed with their arguments" in {
      assert(parse(":prove") == Prove)
      assert(parse("  :RESET ") == Reset)
      assert(parse(":assert x.f > 5") == Assume("x.f > 5", free = false))
      assert(parse(":assume i == 4") == Assume("i == 4", free = true))
      assert(parse(":remove 3, [5] 7") == Remove(Seq(3, 5, 7)))
      assert(parse(":solver cvc5 --foo bar") == Solver("cvc5", Some("--foo bar")))
      assert(parse(":solver z3") == Solver("z3", None))
      assert(parse(":timeout 500") == Timeout(Some(500)))
      assert(parse(":timeout 0") == Timeout(None))
      assert(parse(":set oldheaps on") == ConsoleCommand.Set(PrintSetting.OldHeaps, Left(true)))
      assert(parse(":set depth 3") == ConsoleCommand.Set(PrintSetting.Depth, Right(3)))
      assert(parse(":unfold list(x)") == Fold("list(x)", unfold = true))
      assert(parse(":fold acc(list(x), 1/2)") == Fold("acc(list(x), 1/2)", unfold = false))
      assert(parse(":case 2") == Case(2))
      assert(parse(":recheck") == Recheck)
    }

    "treat input without a colon as an expression" in {
      assert(parse("x.f > 5") == Expression("x.f > 5"))
    }

    "reject malformed input" in {
      Seq(":assert", ":unfold", ":case 0", ":case x", ":remove a", ":timeout -1", ":set depth deep", ":set colour on", ":frobnicate").foreach { input =>
        assert(parse(input).isInstanceOf[Invalid], input)
      }
    }
  }
}
