// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2022 ETH Zurich.

package viper.server.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import viper.server.frontends.lsp.Common

class SemVerComparisonTests extends AnyFlatSpec with Matchers {

  "SemVerComparison" should "produce correct results" in {
    Common.compareSemVer("1.0", "1.0") should be (0)
    Common.compareSemVer("1.0", "1.0") should be (0)
    Common.compareSemVer("1", "1.0") should be (0)
    Common.compareSemVer("1.0", "1") should be (0)
    Common.compareSemVer("2.0", "1") should be (1)
    Common.compareSemVer("2.0", "2.1") should be (-1)
    Common.compareSemVer("2.0", "2.0.1") should be (-1)
    Common.compareSemVer("2.0.1", "2.0.2") should be (-1)
    Common.compareSemVer("2.0.2.0", "2.0.2") should be (0)
    Common.compareSemVer("2.0.2.1", "2.0.2") should be (1)
    Common.compareSemVer("10.0", "1.0.0") should be (1)
  }
}
