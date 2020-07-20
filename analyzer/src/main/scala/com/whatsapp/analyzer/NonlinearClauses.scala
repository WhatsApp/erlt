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

object NonlinearClauses {
  private val indentation = "    "

  case class NonlinearClause(module: String, line: Int, isCovered: Boolean)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    val covered = data.filter(_.isCovered)
    val uncovered = data.filter(!_.isCovered)

    Console.println(s"Nonlinear clauses: ${data.size}")
    Console.println(s"${indentation}those covered by later clauses:   ${covered.size}")
    Console.println(s"${indentation}those uncovered by later clauses: ${uncovered.size}")

    Console.println()

    Console.println("Covered nonlinear clauses:")
    covered.foreach(print)

    Console.println()

    Console.println("Uncovered nonlinear clauses:")
    uncovered.foreach(print)
  }

  private def print(clause: NonlinearClause): Unit =
    Console.println(s"$indentation${clause.module}:${clause.line}")

  private def loadData(rpc: RPC): List[NonlinearClause] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[NonlinearClause] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam")).sorted
    beamFiles flatMap { f => rpc.getNonlinearClauses(s"$dir/$f") }
  }
}
