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

object ErrorHandling {
  case class ModuleInfo(name: String, catches: Int, tries: Int, generated: Boolean)
  case class AppInfo(name: String, modules: List[ModuleInfo])
  case class Total(catches: Int, tries: Int)

  def main(args: Array[String]): Unit = {
    val appInfos = Using.resource(RPC.connect())(loadData)

    var totalCatches = 0
    var totalCatchesNonGen = 0
    var totalTries = 0
    var totalTriesNonGen = 0

    val allModules = appInfos.flatMap(_.modules)
    // some generated stuff is duplicated (or not cleaned up?)
    val generated: Set[String] =
      allModules.filter(_.generated).map(_.name).toSet

    for {
      AppInfo(_, modules) <- appInfos
      moduleInfo <- modules
    } {
      totalCatches = totalCatches + moduleInfo.catches
      totalTries = totalTries + moduleInfo.tries
      if (!moduleInfo.generated) {
        totalCatchesNonGen = totalCatches + moduleInfo.catches
        totalTriesNonGen = totalTriesNonGen + moduleInfo.tries
      }
    }

    Console.println(s"Catches: $totalCatches")
    Console.println(s"Tries:   $totalTries")

    Console.println(s"Catches (NonGen): $totalCatchesNonGen")
    Console.println(s"Tries (NonGen):   $totalTriesNonGen")

    val interestingModules = allModules.filterNot(mi => generated(mi.name) || mi.name.endsWith("_tests"))
    val top10 = interestingModules.sortBy { mi => mi.tries + mi.catches }.reverse.take(10)
    for (moduleInfo <- top10) {
      Console.println(s"${moduleInfo.name} catches:${moduleInfo.catches} tries:${moduleInfo.tries}")
    }
  }

  private def loadData(rpc: RPC): List[AppInfo] = {
    val appInfos = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    appInfos.filterNot(ai => CodeDirs.thirdParty.contains(ai.name))
  }

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    println(s"Starting $dir")
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    val moduleInfos: List[ModuleInfo] =
      for (mName <- moduleNames) yield {
        val path = s"$dir/${mName}.beam"
        val Some((catches, tries)) = rpc.getErrorHandling(path)
        ModuleInfo(mName, catches, tries, CodeDirs.isGenerated(libName, mName))
      }

    AppInfo(libName, moduleInfos)
  }

}
