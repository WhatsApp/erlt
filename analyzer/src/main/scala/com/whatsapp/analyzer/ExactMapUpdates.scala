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

object ExactMapUpdates {
  type Line = Int

  case class Info(module: String, lines: List[Line])

  def main(args: Array[String]): Unit = {
    val infos = Using.resource(RPC.connect())(loadData)
    var count: Int = 0
    for (info <- infos) {
      if (info.lines.nonEmpty) {
        count += info.lines.size
        println(s"${info.module}: ${info.lines.mkString(", ")}")
      }
    }
    println(s"Total: $count")
  }

  private def loadData(rpc: RPC): List[Info] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(rpc))
  }

  private def indexProjectDir(rpc: RPC)(dir: String): List[Info] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames map { m => Info(m, rpc.getExactMapUpdates(s"$dir/${m}.beam")) }
  }
}