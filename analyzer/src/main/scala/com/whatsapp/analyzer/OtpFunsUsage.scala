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

object OtpFunsUsage {

  case class OtpApp(name: String, modules: List[OtpModuleInfo])
  case class OtpModuleInfo(name: String, exported: Int)

  case class Fun(module: String, fun: String, arity: Int)

  case class ProjectApp(name: String, infos: List[ModuleInfo])
  case class ModuleInfo(name: String, usedFuns: List[Fun])

  def main(args: Array[String]): Unit = {
    Using.resource(RPC.connect()) { rpc =>
      val apps = loadOtpApps(rpc)
      analyze(rpc, apps)
    }
  }

  private def analyze(rpc: RPC, apps: List[OtpApp]): Unit = {
    var module2App = Map.empty[String, String]
    var module2OtpModuleInfo = Map.empty[String, OtpModuleInfo]
    var app2OtpApp = Map.empty[String, OtpApp]
    for (app <- apps) {
      for (module <- app.modules) {
        module2App = module2App + (module.name -> app.name)
        app2OtpApp = app2OtpApp + (app.name -> app)
        module2OtpModuleInfo = module2OtpModuleInfo + (module.name -> module)
      }
    }

    val libs = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    val waLibs = libs.filterNot(CodeDirs.thirdParty.contains)
    var usedOtpModules = Set.empty[String]
    var usedOtpFuns = Set.empty[Fun]

    for (lib <- waLibs) {
      for (info <- lib.infos) {
        for (fun <- info.usedFuns) {
          if (module2App.contains(fun.module)) {
            usedOtpModules = usedOtpModules + fun.module
            usedOtpFuns = usedOtpFuns + fun
          }
        }
      }
    }

    println(s"Used mods: ${usedOtpModules.size}")
    println(s"Used funs: ${usedOtpFuns.size}")

    val answer: Map[String, Set[String]] =
      usedOtpModules.groupBy(module2App)
    val usedByModule = usedOtpFuns.groupBy(_.module)

    val otpApps = answer.keys.toList.sorted
    for (otpAppName <- otpApps) {
      val usedModules = answer(otpAppName).toList.sorted
      val otpApp = app2OtpApp(otpAppName)
      val moduleNum = otpApp.modules.size
      val usedModuleNum = usedModules.size
      val funNum = otpApp.modules.map(_.exported).sum
      val usedFunNum = usedModules.flatMap(usedByModule).toSet.size

      Console.println(s"$otpAppName (${usedModuleNum}/$moduleNum mods) (${usedFunNum}/${funNum} funs)")
      for (module <- usedModules) {
        val usedModFuns = usedByModule(module).toList.map(f => s"${f.fun}/${f.arity}").sorted
        val modFuns = module2OtpModuleInfo(module).exported
        Console.println(s"    $module (${usedModFuns.size}/$modFuns funs)")
        for (mf <- usedModFuns) {
          Console.println(s"        $mf")
        }
      }
    }
  }

  private def loadOtpApps(rpc: RPC): List[OtpApp] = {
    val otpEbinDirs = rpc.getOtpEbinDirs()
    otpEbinDirs.map(loadOtpApp(_, rpc))
  }

  private def loadOtpApp(dir: String, rpc: RPC): OtpApp = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('-')
    val libName = dir.substring(start + 5, end)
    val beamFiles = new java.io.File(dir).list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    val infos = moduleNames.map { mName =>
      val path = s"$dir/${mName}.beam"
      val Some(deps) = rpc.getExports(path)
      OtpModuleInfo(mName, deps.size)
    }
    OtpApp(libName, infos)
  }

  private def indexProjectDir(dir: String, rpc: RPC): ProjectApp = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    val infos = moduleNames.map { mName =>
      val path = s"$dir/${mName}.beam"
      val Some(deps) = rpc.getUsedFuns(path)
      ModuleInfo(mName, deps.map(d => Fun(d._1, d._2, d._3)))
    }
    ProjectApp(libName, infos)
  }
}
