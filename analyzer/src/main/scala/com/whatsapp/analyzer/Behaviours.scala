package com.whatsapp.analyzer

object Behaviours {

  case class AppInfo(name: String, modules: Int, modulesWithBehaviours: Int, usage: Map[String, Int])
  case class InfoAcc(modules: Int, modulesWithBehaviours: Int, usage: Map[String, Int])

  def main(args: Array[String]): Unit = {
    val rpc = RPC.connect()
    val data =
      try {
        loadData(rpc)
      } finally {
        rpc.close()
      }

    val behaviours = data.usage.keys.toList.sorted
    Console.println("ALL")
    for (behaviour <- behaviours) {
      Console.println(s"$behaviour ${data.usage(behaviour)}")
    }

    Console.println(s"Modules:    ${data.modules}")
    Console.println(s"Behaviours: ${data.modulesWithBehaviours}")
    Console.println("Top10")
    val top10 = data.usage.toList.sortBy(_._2).reverse.take(10)
    for ((behaviour, count) <- top10) {
      Console.println(s"$behaviour $count")
    }
  }

  private def loadData(rpc: RPC): InfoAcc = {
    var usageAcc = Map.empty[String, Int].withDefaultValue(0)
    var modulesAcc = 0
    var modulesWithBehavioursAcc = 0
    val projectUsages = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    for (AppInfo(libName, modules, modulesWithBehaviours, usage) <- projectUsages if !CodeDirs.thirdParty.contains(libName)) {
      modulesAcc = modulesAcc + modules
      modulesWithBehavioursAcc = modulesWithBehavioursAcc + modulesWithBehaviours
      for ((behaviour, count) <- usage)
        usageAcc = usageAcc.updated(behaviour, usageAcc(behaviour) + count)
    }
    InfoAcc(modulesAcc, modulesWithBehavioursAcc, usageAcc)
  }

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    var usage = Map.empty[String, Int].withDefaultValue(0)
    var modules: Int = 0
    var modulesWithBehaviours: Int = 0
    for (mName <- moduleNames) {
      val path = s"$dir/${mName}.beam"
      val Some(behaviours) = rpc.getBehaviours(path)
      for (behaviour <- behaviours) {
        usage = usage.updated(behaviour, usage(behaviour) + 1)
      }
      modules = modules + 1
      if (behaviours.nonEmpty) {
        modulesWithBehaviours = modulesWithBehaviours + 1
      }
    }
    AppInfo(libName, modules, modulesWithBehaviours, usage)
  }
}
