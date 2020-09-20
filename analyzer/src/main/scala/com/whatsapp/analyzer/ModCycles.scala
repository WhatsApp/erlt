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

import com.whatsapp.analyzer.util.SCC

import scala.util.Using

object ModCycles {

  case class Fun(module: String, fun: String, arity: Int)

  case class ProjectApp(name: String, infos: List[ModuleInfo])
  case class ModuleInfo(name: String, usedFuns: List[Fun])

  def main(args: Array[String]): Unit = {
    Using.resource(RPC.connect()) { rpc =>
      analyze(rpc)
    }
  }

  private def analyze(rpc: RPC): Unit = {
    val libs = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    val waLibs = libs.filterNot(CodeDirs.thirdParty.contains)

    var modules =
      Set.empty[String]
    var usedStuff =
      Map.empty[String, Set[String]]

    for (lib <- waLibs) {
      for (info <- lib.infos) {
        val thisMod = info.name
        val usedMods = info.usedFuns.map(_.module).toSet - thisMod
        modules += thisMod
        usedStuff += thisMod -> usedMods
      }
    }

    val vertices: List[SCC.Vertex] =
      modules.toList.sorted.map(SCC.Vertex)
    val rawEdges: List[(String, String)] = {
      // we have to filter modules outside of known modules out
      // (SCC analysis implies that edges lead to existing vertices)
      usedStuff.toList.flatMap(kv => kv._2.toList.filter(modules).map(kv._1 -> _))
    }
    val edges: List[SCC.Edge] =
      rawEdges.map(kv => SCC.Edge(SCC.Vertex(kv._1), SCC.Vertex(kv._2)))

    val g = SCC.G(vertices, edges)
    val components = SCC.components(g).map(_.map(_.label))

    components.foreach { comp =>
      if (comp.size > 1) {
        println(comp.mkString(", "))
      }
    }
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