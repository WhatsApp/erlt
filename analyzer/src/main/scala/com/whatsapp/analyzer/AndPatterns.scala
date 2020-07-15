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

package com.whatsapp.analyzer

import scala.util.Using

object AndPatterns {
  private val indentation = "    "

  case class AndPattern(module: String, line: Int, isRenameOnly: Boolean)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    val renameOnly = data.filter(_.isRenameOnly)
    val complexPattern = data.filter(!_.isRenameOnly)

    Console.println(s"Compound patterns: ${data.size}")
    Console.println(s"${indentation}rename only:     ${renameOnly.size}")
    Console.println(s"${indentation}complex pattern: ${complexPattern.size}")

    Console.println()

    Console.println("Complex and patterns:")
    complexPattern.foreach(print)
  }

  private def print(p: AndPattern): Unit =
    Console.println(s"$indentation${p.module}:${p.line}")

  private def loadData(rpc: RPC): List[AndPattern] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[AndPattern] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam")).sorted
    beamFiles flatMap { f => rpc.getAndPatterns(s"$dir/$f") }
  }
}
