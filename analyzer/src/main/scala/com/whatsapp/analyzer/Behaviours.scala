package com.whatsapp.analyzer

object Behaviours {

  def main(args: Array[String]): Unit = {
    val rpc = RPC.connect()
    val data =
      try {
        loadData(rpc)
      } finally {
        rpc.close()
      }

    val behaviours = data.keys.toList.sorted
    Console.println("ALL")
    for (behaviour <- behaviours) {
      Console.println(s"$behaviour ${data(behaviour)}")
    }
    Console.println("Top10")
    val top10 = data.toList.sortBy(_._2).reverse.take(10)
    for ((behaviour, count) <- top10) {
      Console.println(s"$behaviour $count")
    }
  }

  private def loadData(rpc: RPC): Map[String, Int] = {
    var usage = Map.empty[String, Int].withDefaultValue(0)
    val projectUsages = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    for {
      (libName, x) <- projectUsages if !CodeDirs.thirdParty.contains(libName)
      (behaviour, count) <- x
    } usage = usage.updated(behaviour, usage(behaviour) + count)
    usage
  }

  private def indexProjectDir(dir: String, rpc: RPC): (String, Map[String, Int]) = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    var usage = Map.empty[String, Int].withDefaultValue(0)
    for (mName <- moduleNames) {
      val path = s"$dir/${mName}.beam"
      val Some(behaviours) = rpc.getBehaviours(path)
      for (behaviour <- behaviours) {
        usage = usage.updated(behaviour, usage(behaviour) + 1)
      }
    }
    (libName, usage)
  }
}
