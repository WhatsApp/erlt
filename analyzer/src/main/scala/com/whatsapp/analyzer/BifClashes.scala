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

object BifClashes {

  case class Clash(module: String, name: String, arity: Int)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    Console.println(s"Clashes: ${data.size}")
    for (clash <- data) {
      Console.println(s"${clash.module}:${clash.name}/${clash.arity}")
    }
  }

  private def loadData(rpc: RPC): List[Clash] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[Clash] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)

    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }

    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames flatMap { mn =>
      rpc.getBifClashes(s"$dir/${mn}.beam") map { case (m, n, a) => Clash(m, n, a) }
    }
  }
}
