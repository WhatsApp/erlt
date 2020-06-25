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

object DynamicCalls {

  case class Usage(module: String, calls: List[String], generated: Boolean)

  def main(args: Array[String]): Unit = {
    val usages = Using.resource(RPC.connect())(loadData)

    val allUsages = usages.filter(_.calls.nonEmpty).sortBy(_.calls.length).reverse
    val allCount = allUsages.map(_.calls.length).sum

    val nonGenUsages = allUsages.filterNot(_.generated)
    val nonGenCount = nonGenUsages.map(_.calls.length).sum

    Console.println(s"all dynamic calls: $allCount")
    Console.println(s"non-gen dynamic calls: $nonGenCount")

    Console.println(s"all modules with dynamic calls: ${allUsages.size}")
    Console.println(s"non-gen modules with dynamic calls: ${nonGenUsages.size}")

    for (usage <- nonGenUsages) {
      Console.println(s"${usage.module}: ${usage.calls.mkString(",")}")
    }
  }

  private def loadData(rpc: RPC): List[Usage] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[Usage] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)

    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }

    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames map { m =>
      Usage(m, rpc.getDynamicCalls(s"$dir/${m}.beam"), CodeDirs.isGenerated(libName, m))
    }
  }
}
