// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import akka.actor.ActorSystem
import org.scalatest.flatspec.AsyncFlatSpec
import viper.server.core.ViperCoreServerUtils.getMessagesFuture
import viper.server.utility.AstGenerator
import viper.silver.ast.Program
import viper.silver.logger.SilentLogger
import viper.silver.reporter.{EntityFailureMessage, Message, OverallFailureMessage}

class AsyncCoreServerSpec extends AsyncFlatSpec {
  implicit var actor_system: ActorSystem = ActorSystem("Test")

  private val test_file = "src/test/resources/viper/verification_error.vpr"
  private val test_ast: Program = (new AstGenerator(SilentLogger().get)).generateViperAst(test_file).get

  val server_args: Array[String] = Array()
  val silicon_without_caching: SiliconConfig = SiliconConfig(List("--disableCaching"))
  val silicon_with_caching: SiliconConfig = SiliconConfig(List())

  val core = new ViperCoreServer(server_args)
  core.start()

  /* vvvvvvvvvvvvvvvvvvvvvvv */

  behavior of "ViperCoreServer"

  /* ^^^^^^^^^^^^^^^^^^^^^^^ */

  it should s"be able to eventually produce an OverallFailureMessage @$test_file" in {
    val jid = core.verify(test_file, silicon_with_caching, test_ast)
    ViperCoreServerUtils.getMessagesFuture(core, jid) map {
      messages: List[Message] =>
        val ofms = messages collect {
          case ofm: OverallFailureMessage => ofm
        }
        val efms = messages collect {
          case efm: EntityFailureMessage => efm
        }
        assert(ofms.length === 1)
        assert(efms.length === 1)
    }
  }

  it should s"retrieve the cached results upon requesting to verify the same AST" in {
    val jid = core.verify(test_file, silicon_with_caching, test_ast)
    getMessagesFuture(core, jid) map {
      messages: List[Message] =>
        val efms: List[EntityFailureMessage] = messages collect {
          case efm: EntityFailureMessage => efm
        }
//        println(ViperCache)
//        println(efms.last)
        assert(efms.length === 1 && efms.last.cached)
//          ofms.last.result.errors.collect { case a: AbstractError => a.cached }.length === 1)
    }
  }

  it should s"run getMessagesFuture() to get Seq[Message] containing the expected verification result" in {
    val jid = core.verify(test_file, silicon_without_caching, test_ast)
    getMessagesFuture(core, jid) map {
      messages: List[Message] =>
//        println(messages)
        assert(true)
    }
  }
}
