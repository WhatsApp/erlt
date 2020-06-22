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

object HighLevelStats {

  case class AppInfo(name: String, modules: List[ModuleInfo])
  case class ModuleInfo(name: String, funCount: Int, generated: Boolean)
  case class AppInfoStat(name: String, allMods: Int, genMods: Int, allFuns: Int, genFuns: Int)

  def main(args: Array[String]): Unit = {
    val rpc = RPC.connect()
    val data =
      try {
        loadData(rpc)
      } finally {
        rpc.close()
      }

    val allModules = data.flatMap(_.modules)
    val nonGenModules = allModules.filterNot(_.generated)
    val allModuleCount = allModules.size
    val nonGenModuleCount = nonGenModules.size

    val allFunCount = allModules.map(_.funCount).sum
    val nonGenFunCount = nonGenModules.map(_.funCount).sum

    println(s"All modules: $allModuleCount")
    println(s"NonGen modules: $nonGenModuleCount")

    println(s"All funs: $allFunCount")
    println(s"NonGen funs: $nonGenFunCount")

    val stats: List[AppInfoStat] = for (appInfo <- data) yield {
      val allMs = appInfo.modules.size
      val genMs = appInfo.modules.count(_.generated)
      val allFuns = appInfo.modules.map(_.funCount).sum
      val genFuns = appInfo.modules.filter(_.generated).map(_.funCount).sum
      AppInfoStat(appInfo.name, allMods = allMs, genMods = genMs, allFuns = allFuns, genFuns = genFuns)
    }

    val stats1 = stats.sortBy(_.name)

    for (stat <- stats1) {
      println(stat.name)
      println(s"  Mods: ${stat.allMods}")
      println(s"  Gen Mods: ${stat.genMods}")
      println(s"  Funs: ${stat.allFuns}")
      println(s"  Gen Funs: ${stat.genFuns}")
    }
  }

  private def loadData(rpc: RPC): List[AppInfo] = {
    val infos = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    val fInfos = infos.filterNot(info => CodeDirs.thirdParty.contains(info.name))
    fInfos
  }

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    val modules: List[ModuleInfo] = for (mName <- moduleNames) yield {
      val path = s"$dir/${mName}.beam"
      val count = rpc.getFunctions(path)
      ModuleInfo(mName, count, CodeDirs.isGenerated(libName, mName))
    }
    AppInfo(libName, modules)
  }
}
