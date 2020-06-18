package com.whatsapp.analyzer

object Receives {

  case class Receives(module: String, count: Int)

  def main(args: Array[String]): Unit = {
    val rpc = RPC.connect()
    val data =
      try {
        loadData(rpc)
      } finally {
        rpc.close()
      }

    val data1 = data.filter(_.count > 0).sortBy(_.count).reverse
    val totalCount = data1.map(_.count).sum
    Console.println(s"all receives: $totalCount")
    Console.println(s"modules with receives: ${data1.size}")

    for (receives <- data1) {
      Console.println(s"${receives.module}: ${receives.count}")
    }
  }

  private def loadData(rpc: RPC): List[Receives] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[Receives] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)

    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }

    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames map { m => Receives(m, rpc.getReceives(s"$dir/${m}.beam")) }
  }
}
