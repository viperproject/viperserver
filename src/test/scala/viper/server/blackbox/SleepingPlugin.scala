// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2026 ETH Zurich.

package viper.server.blackbox

import viper.silver.plugin.SilverPlugin

/** A plugin that makes a parse slow by construction: before a file is parsed, it sleeps for as
  * many milliseconds as the file asks for in a comment of the form `// sleep 3000`. A file
  * without such a comment is not delayed. It is loaded like any plugin, with `--plugin` and its
  * class name.
  *
  * Like the parser itself, the sleep takes no notice of an interrupt: a parse that is given up
  * while it sleeps still lasts as long as the file asks for.
  */
class SleepingPlugin extends SilverPlugin {
  override def beforeParse(input: String, isImported: Boolean): String = {
    SleepingPlugin.request.findFirstMatchIn(input).foreach(m => SleepingPlugin.sleepThrough(m.group(1).toLong))
    input
  }
}

object SleepingPlugin {
  val className: String = classOf[SleepingPlugin].getName
  private val request = """//\s*sleep\s+(\d+)""".r

  private def sleepThrough(ms: Long): Unit = {
    val end = System.currentTimeMillis() + ms
    var interrupted = false
    while (System.currentTimeMillis() < end) {
      try Thread.sleep(math.max(1, end - System.currentTimeMillis())) catch { case _: InterruptedException => interrupted = true }
    }
    if (interrupted) Thread.currentThread().interrupt()
  }
}
