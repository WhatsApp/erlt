/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.corq.test

import com.whatsapp.corq.ast.DB
import com.whatsapp.corq.test.util._

object Main {
  sealed trait Cmd
  case object Check extends Cmd
  case object Debug extends Cmd

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      help()
      return
    }
    val cmd: Cmd = args(0) match {
      case "check" => Check
      case "debug" => Debug
      case _       => help(); return
    }

    val module = args(1)
    DB.beamLocation(module) match {
      case None =>
        Console.err.println(s"Cannot locate beam file for module $module")
      case Some(beamFile) =>
        Console.println(s"Loading forms from $beamFile")

        val feedback = cmd match {
          case Check => TcDiagnosticsText.checkForms(beamFile).mkString("", "\n", "\n")
          case Debug => WIPDiagnosticsText.loadForms(beamFile).mkString("", "\n", "\n")
        }

        Console.println(feedback)
    }
  }

  def help(): Unit = {
    Console.println("com.whatsapp.corq.test.util.TypeCheckModule")
    Console.println("usage:")
    Console.println("  check <module_name>")
    Console.println("  debug <module_name>")
  }
}
