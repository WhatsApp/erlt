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

object PatternMatches {
  case class PatternMatches(module: String, matches: Int, clauses: Int)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    Console.println(s"Total number of pattern matching constructs: ${data.map(_.matches).sum}")
    Console.println(s"Total number of clauses: ${data.map(_.clauses).sum}")

    Console.println()

    data.foreach { module =>
      Console.println(s"${module.module} matches: ${module.matches}, clauses: ${module.clauses}")
    }
  }

  private def loadData(rpc: RPC): List[PatternMatches] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[PatternMatches] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam")).sorted
    beamFiles map { f => rpc.getPatternMatches(s"$dir/$f") }
  }
}
