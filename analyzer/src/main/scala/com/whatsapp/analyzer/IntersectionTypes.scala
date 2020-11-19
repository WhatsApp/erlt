package com.whatsapp.analyzer

import scala.util.Using

object IntersectionTypes {
  type Id = (String, Int)
  case class ModuleInfo(module: String, multiSpecs: List[Id])
  case class AppInfo(app: String, moduleInfos: List[ModuleInfo])

  def main(args: Array[String]): Unit = {
    val infos = Using.resource(RPC.connect())(loadData)
    var count = 0
    for {
      info <- infos
      ModuleInfo(module, multiSpecs) <- info.moduleInfos
    } {
      if (multiSpecs.nonEmpty) {
        println(s"$module $multiSpecs")
        count = count + multiSpecs.size
      }
    }

    println(s"COUNT: $count")

  }

  private def loadData(rpc: RPC): List[AppInfo] =
    CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    val infos = moduleNames.map { mName =>
      val path = s"$dir/${mName}.beam"
      val Some(specs) = rpc.getMultiSpecs(path)
      ModuleInfo(mName, specs)
    }
    AppInfo(libName, infos)
  }

}
