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

package com.whatsapp.corq.ast

import com.whatsapp.corq.{CodeDirs, RPC, config}
import erlang.CErl
import erlang.Data._

import scala.util.Using

object DB {
  case class Info(module: String, count: Int)

  def main(args: Array[String]): Unit = {
    println(s"Here ${args(0)}")
    // TODO: relative path
    val beamFilePath = "/Users/mheiber/erlt/corq/" + args(0)
    Using.resource(RPC.connect())(loadData(beamFilePath))
  }

  private def loadData(beamFilePath: String)(rpc: RPC): Option[CErl.CModule] = {
        val maybeModule = rpc.loadCoreForms(beamFilePath)
        pprint.pprintln(maybeModule, height = 1000)
        maybeModule
        // Info(m, maybeModule.map(_.defs.size).getOrElse(0))
  }
}