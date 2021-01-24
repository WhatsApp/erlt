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

import com.whatsapp.analyzer.util.TypeVars
import erlang.forms.AbstractForm.{AF_FunctionSpec, IdWithArity}

import scala.util.Using

object ParameterizedSpecs {

  case class Info(module: String, parameterizedSpecs: List[IdWithArity])

  def main(args: Array[String]): Unit = {
    val infos = Using.resource(RPC.connect())(loadData)
    var count: Int = 0
    for (info <- infos) {
      if (info.parameterizedSpecs.nonEmpty) {
        count += info.parameterizedSpecs.length
        println(s"${info.module}: ${info.parameterizedSpecs.map(showId).mkString(", ")}")
      }
    }
    println(s"Total: $count parameterized types")
  }

  private def showId(id: IdWithArity): String =
    s"${id._1}/${id._2}"

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
    moduleNames map { m => Info(m, paramSpecs(rpc.getSpecs(s"$dir/${m}.beam"))) }
  }

  def paramSpecs(specs: List[AF_FunctionSpec]): List[IdWithArity] =
    for {
      spec <- specs
      freeVars = TypeVars.freeVars(spec)
      if (freeVars - "_").nonEmpty
    } yield spec.id
}
