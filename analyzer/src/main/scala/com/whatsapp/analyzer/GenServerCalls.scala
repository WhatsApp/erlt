package com.whatsapp.analyzer

import scala.util.Using

object GenServerCalls {
  type Id = (String, Int)
  case class Calls(module: String, total: Int, tagged: Int, others: Int, moduleRefs: Int)
  case class AppInfo(app: String, calls: List[Calls])

  def main(args: Array[String]): Unit = {
    val infos = Using.resource(RPC.connect())(loadData)
    var totalCount, taggedCount, othersCount, moduleRefsCount = 0
    var exceptions = List[Calls]()
    for {
      info <- infos
      Calls(module, total, tagged, others, moduleRefs) <- info.calls
    } {
      totalCount = totalCount + total
      taggedCount = taggedCount + tagged
      othersCount = othersCount + others
      moduleRefsCount = moduleRefsCount + moduleRefs
      if (others > 0) {
        exceptions = Calls(module, total, tagged, others, moduleRefs) :: exceptions
      }
    }

    println("Results")
    println(s"  Total calls: $totalCount")
    println(s"    of which, tagged: $taggedCount")
    println(s"    of which, others: $othersCount")
    println(s"  ModuleName=Ref: $moduleRefsCount")

    println("Definitions")
    println("  Total  - The count of all gen_server:calls")
    println("  Tagged - The count of all gen_server:calls where the message is a literal atom\n" +
            "           or a tuple whose first argument is a literal atom, e.g.:\n" +
            "             `gen_server:call(Pid, cancel_last_order)`\n" +
            "             `gen_server:call(Pid, {order_pizza, Diameter, Base, Topping})`")
    println("  Others - The count of all gen_server:calls where the message is of another\n" +
            "           syntactic structure, such as a variable bound elsewhere, e.g.:\n" +
            "             `gen_server:call(Pid, Message)`")
    println("  ModuleName=Ref - The count of times where the module name is clearly used as\n" +
            "                   the ServerRef, which implies the `handle_call` implementation\n" +
            "                   used will be the one in the same file as the call, e.g.:\n" +
            "                     `gen_server:call(?MODULE, {register_job, JobId})`")

    for (e <- exceptions) {
      println(e)
    }
  }

  private def loadData(rpc: RPC): List[AppInfo] = {
    val infos = CodeDirs.projectEbinDirs.map(indexProjectDir(_, rpc))
    infos.filterNot { info => CodeDirs.thirdParty.contains(info.app) }
  }

  private def indexProjectDir(dir: String, rpc: RPC): AppInfo = {
    val start = dir.lastIndexOf("/lib/")
    val end = dir.lastIndexOf('/')
    val libName = dir.substring(start + 5, end)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    val moduleNames = beamFiles.map(s => s.substring(0, s.length - 5)).sorted

    val infos = moduleNames.map { mName =>
      val path = s"$dir/${mName}.beam"
      val Some((total, tagged, others, moduleRefs)) = rpc.getGenServerCalls(path)
      Calls(mName, total, tagged, others, moduleRefs)
    }
    AppInfo(libName, infos)
  }

}
