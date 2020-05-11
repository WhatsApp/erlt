package com.whatsapp.sterlang.test.it

import com.whatsapp.sterlang.TypePrinter2
import com.whatsapp.sterlang.TypePrinter2.{TypeSchemes, Types}

abstract class DirSpec extends org.scalatest.FunSpec {
  import java.io.File

  def testDir(dir: String): Unit = {
    describe(dir) {
      val file = new File(dir)
      val files = file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl"))
      val paths: Array[String] = files.map(_.getPath).sorted
      paths.foreach { p =>
        it(p) {
          testFile(p)
          testFileVerbose(p)
        }
      }
    }
  }

  def testFile(f: String): Unit = {
    SterlangTestUtil.processFile(f, TypeSchemes, "_ty", "ty")

    val myOutput = fileContent(f + "._ty")
    val expectedOut = fileContent(f + ".ty")
    assert(myOutput == expectedOut)

    new File(f + "._ty").delete()
  }

  def testFileVerbose(f: String): Unit = {
    SterlangTestUtil.processFile(f, Types, "_vt", "vt")

    val myOutput = fileContent(f + "._vt")
    val expectedOut = fileContent(f + ".vt")
    assert(myOutput == expectedOut)

    new File(f + "._vt").delete()
  }

  def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }
}
