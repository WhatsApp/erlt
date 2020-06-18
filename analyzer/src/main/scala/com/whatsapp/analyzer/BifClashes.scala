package com.whatsapp.analyzer

object BifClashes {

  case class Clash(module: String, name: String, arity: Int)

  def main(args: Array[String]): Unit = {
    val rpc = RPC.connect()
    val data =
      try {
        loadData(rpc)
      } finally {
        rpc.close()
      }

    Console.println(s"Clashes: ${data.size}")
    for (clash <- data) {
      Console.println(s"${clash.module}:${clash.name}/${clash.arity}")
    }
  }

  private def loadData(rpc: RPC): List[Clash] = {
    CodeDirs.projectEbinDirs.flatMap(indexProjectDir(_, rpc))
  }

  private def indexProjectDir(dir: String, rpc: RPC): List[Clash] = {
    val dirFile = new java.io.File(dir)
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)

    if (CodeDirs.thirdParty.contains(libName)) {
      return List.empty
    }

    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted
    moduleNames flatMap  { mn =>
      rpc.getBifClashes(s"$dir/${mn}.beam") map {case (m, n, a) => Clash(m, n, a)}
    }
  }
}
