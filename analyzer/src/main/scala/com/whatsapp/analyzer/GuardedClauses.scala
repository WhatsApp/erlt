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

object GuardedClauses {
  private val indentation = "    "

  case class GuardedClause(module: String, line: Int, isCovered: Boolean, isGuardTypeTestOnly: Boolean)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData).sortBy(c => (c.module, c.line))

    val covered = data.filter(_.isCovered)
    val uncovered = data.filter(!_.isCovered)
    val uncoveredTypeTestOnly = uncovered.filter(_.isGuardTypeTestOnly)
    val uncoveredRealLogic = uncovered.filter(!_.isGuardTypeTestOnly)

    Console.println(s"Clauses with guards: ${data.size}")
    Console.println(s"${indentation}those covered by later clauses:   ${covered.size}")
    Console.println(s"${indentation}those uncovered by later clauses: ${uncovered.size}")
    Console.println(s"$indentation${indentation}with only type test: ${uncoveredTypeTestOnly.size}")
    Console.println(s"$indentation${indentation}with real logic: ${uncoveredRealLogic.size}")

    Console.println()

    Console.println("Covered guarded clauses:")
    covered.foreach(print)

    Console.println()

    Console.println("Uncovered guarded clauses with only type tests:")
    uncoveredTypeTestOnly.foreach(print)

    Console.println()

    Console.println("Uncovered guarded clauses with real logic:")
    uncoveredRealLogic.foreach(print)
  }

  private def print(clause: GuardedClause): Unit =
    Console.println(s"$indentation${clause.module}:${clause.line}")

  private def loadData(rpc: RPC): List[GuardedClause] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[GuardedClause] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam")).sorted
    beamFiles flatMap { f => rpc.getGuardedClauses(s"$dir/$f") }
  }
}
