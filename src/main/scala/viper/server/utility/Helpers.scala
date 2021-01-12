// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.utility

import java.io.File

object Helpers {
  def getArgListFromArgString(arg_str: String): List[String] = {
    val possibly_quoted_string = raw"""[^\s"']+|"[^"]*"|'[^']*'""".r
    val quoted_string = """^["'](.*)["']$""".r
    possibly_quoted_string.findAllIn(arg_str).toList.map {
      case quoted_string(noqt_a) => noqt_a
      case a => a
    }
  }

  def canonizedFile(some_file_path: String): File = {
    val f = new File(some_file_path)
    if (f.isAbsolute) {
      f
    } else {
      java.nio.file.Paths.get(System.getProperty("user.dir"), some_file_path).toFile
    }
  }

  def validatePath(path: String): Boolean = {
    val f = canonizedFile(path)
    (f.isFile || f.isDirectory) && f.canWrite || f.getParentFile.canWrite
  }

  def validateViperFile(path: String): Boolean = {
    val f = canonizedFile(path)
    f.isFile && f.canWrite
  }

}
