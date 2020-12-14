package com.whatsapp.analyzer.test

import com.whatsapp.analyzer.CodeDirs
import erlang.Beam
import erlang.Data.EList
import erlang.forms.AbstractFormConvert

import java.io.{PrintWriter, StringWriter}

object LoadAllForms {

  val debug = false

  def main(args: Array[String]): Unit =
    loadAllForms()

  private def loadAllForms(): Unit =
    CodeDirs.projectEbinDirs.foreach(loadFormsFromDir)

  private def loadFormsFromDir(dir: String): Unit = {
    println(dir)
    val dirFile = new java.io.File(dir)
    val beamFiles = dirFile.list().toList.filter(_.endsWith(".beam"))
    for (beamFile <- beamFiles) {
      val beamFilePath = s"$dir/$beamFile"
      val rawForms = Beam.loadAbstractForms(beamFilePath).get
      val absForms = rawForms match {
        case EList(elems, None) =>
          try {
            elems.map(AbstractFormConvert.convertForm(_, lite = false))
          } catch {
            case e: Throwable =>
              if (debug) {
                Console.err.println(s"$beamFilePath: ERROR")
                Console.err.println(shortenedStackTrace(e, 20))
              } else {
                Console.print("E")
              }
              List()
          }
        case _ =>
          sys.error(s"$beamFilePath: wrong forms")
      }
      Console.out.print(".")
    }
    Console.out.println()
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
