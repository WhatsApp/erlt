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

object NonlinearPatterns {
  private val indentation = "    "

  case class FunctionWithNonlinearClause(
      module: String,
      function: String,
      arity: Int,
      hasWildcardCatchAll: Boolean,
      hasCatchAll: Boolean,
  )

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    val withWildcardCatchAll = data.filter(_.hasWildcardCatchAll)
    val withComplexCatchAll = data.filter(c => c.hasCatchAll && !c.hasWildcardCatchAll)
    val withoutCatchAll = data.filter(!_.hasCatchAll)

    Console.println(s"Functions with nonlinear clauses: ${data.size}")
    Console.println(s"${indentation}those with a wildcard catch all: ${withWildcardCatchAll.size}")
    Console.println(s"${indentation}those with a complex catch all:  ${withComplexCatchAll.size}")
    Console.println(s"${indentation}those without a catch all:       ${withoutCatchAll.size}")

    Console.println()

    def printFunction(f: FunctionWithNonlinearClause): Unit =
      Console.println(s"$indentation${f.module}:${f.function}/${f.arity}")

    Console.println("Functions with nonlinear clauses (with a wildcard catch all):")
    withWildcardCatchAll.foreach(printFunction)

    Console.println()

    Console.println("Functions with nonlinear clauses (with a complex catch all):")
    withComplexCatchAll.foreach(printFunction)

    Console.println()

    Console.println("Functions with nonlinear clauses (without a catch all):")
    withoutCatchAll.foreach(printFunction)
  }

  private def loadData(rpc: RPC): List[FunctionWithNonlinearClause] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[FunctionWithNonlinearClause] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam")).sorted
    beamFiles flatMap { f => rpc.getFunctionsWithNonlinearClauses(s"$dir/$f") }
  }
}
