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

object Primitives {

  case class Usage(module: String, primitives: List[String])

  def main(args: Array[String]): Unit = {
    val primCategory = args(0)
    val rpc = RPC.connect()
    val usages =
      try {
        loadData(rpc, primCategory)
      } finally {
        rpc.close()
      }

    val usages1 = usages.filter(_.primitives.nonEmpty).sortBy(_.primitives.length).reverse
    val totalCount = usages1.map(_.primitives.length).sum

    Console.println(s"all primitives: $totalCount")
    val allPrimitives: List[(String, Int)] =
      usages1.flatMap(_.primitives).groupBy(identity).toList.map { case (p, ps) => (p, ps.size) }.sortBy(_._2).reverse

    Console.println(s"modules with primitives: ${usages1.size}")

    Console.println()
    Console.println("Primitives:")
    for ((primitive, count) <- allPrimitives) {
      Console.println(s"$primitive: $count")
    }

    Console.println()
    Console.println("Primitives per module:")
    for (usage <- usages1) {
      Console.println(s"${usage.module}: ${usage.primitives.mkString(",")}")
    }
  }

  private def loadData(rpc: RPC, primCategory: String): List[Usage] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(_, rpc, primCategory))
  }

  private def indexProjectDir(dir: String, rpc: RPC, primCategory: String): List[Usage] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)

    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }

    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames map { m => Usage(m, rpc.getPrimitives(s"$dir/${m}.beam", primCategory)) }
  }
}
