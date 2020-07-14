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

  case class NonlinearPattern(module: String, function: String, arity: Int)

  def main(args: Array[String]): Unit = {
    val data = Using.resource(RPC.connect())(loadData)

    Console.println(s"Nonlinear Patterns: ${data.size}")
    for (pattern <- data) {
      Console.println(s"${pattern.module}:${pattern.function}/${pattern.arity}")
    }
  }

  private def loadData(rpc: RPC): List[NonlinearPattern] = {
    CodeDirs.firstPartyEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[NonlinearPattern] = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s =>  s.substring(0, s.length - ".beam".length)).sorted
    moduleNames flatMap { m =>
      rpc.getNonlinearPatterns(s"$dir/$m.beam") map { case (f, a) => NonlinearPattern(m, f, a) }
    }
  }
}
