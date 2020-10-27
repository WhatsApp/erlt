package com.whatsapp.sterlang.test.it

import java.io.File
import java.nio.file.Files

import com.whatsapp.sterlang.etf

class ParserSpec extends org.scalatest.funspec.AnyFunSpec {

  testDir("examples/pos/src")
  testDir("examples/elm_core/src")
  testDir("examples/dev/src")
  testDir("examples/dir/src")
  testDir("examples/pattern/src")
  testDir("examples/pattern_error/src")

  def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt")).map(_.getName).map(_.dropRight(5)).sorted

      moduleNames.foreach { m =>
        val erltPath = s"$iDirPath/$m.erlt"
        it(erltPath) {
          testFile(iDirPath, m)
        }
      }
    }
  }

  def testFile(iDirPath: String, module: String): Unit = {
    val inputFile = s"$iDirPath/$module.erlt"
    val devProgram = etf.programFromFileDev(inputFile)
    val erltProgram = etf.programFromFileErlt(inputFile)
    assert(devProgram === erltProgram)
  }
}
