package com.whatsapp.analyzer.test

import java.io.{PrintWriter, StringWriter}

import com.whatsapp.analyzer.{CodeDirs, RPC}
import erlang.Data.EList
import erlang.forms.AbstractFormConvert

import scala.util.Using

object LoadAllForms {

  def main(args: Array[String]): Unit =
    Using.resource(RPC.connect())(loadAllForms)

  private def loadAllForms(rpc: RPC): Unit =
    CodeDirs.projectEbinDirs.foreach(loadFormsFromDir(rpc))

  private def loadFormsFromDir(rpc: RPC)(dir: String): Unit = {
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    for (beamFile <- beamFiles) {
      val beamFilePath = s"$dir/$beamFile"
      val rawForms = rpc.getForms(beamFilePath)
      val absForms = rawForms match {
        case EList(elems, None) =>
          try {
            elems.map(AbstractFormConvert.convertForm)
          } catch {
            case e: Throwable =>
              Console.err.println(s"$beamFilePath: ERROR")
              Console.err.println(shortenedStackTrace(e, 20))
              List()
          }
        case _ =>
          sys.error(s"$beamFilePath: wrong forms")
      }
    }
  }

  def shortenedStackTrace(e: Throwable, maxLines: Int): String = {
    val writer = new StringWriter
    e.printStackTrace(new PrintWriter(writer))
    val lines = writer.toString.split("\n")
    val sb = new StringBuilder
    for (i <- 0 until Math.min(lines.length, maxLines))
      sb.append(lines(i)).append("\n")
    sb.toString
  }
}
