package com.whatsapp.sterlang.test.it

import java.io.File
import java.nio.file.{Files, Path}

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
      val sterlangOutDir = Files.createTempDirectory("sterlang")
      val erltBuildDir = Files.createTempDirectory("erlt_build")
      val erltEBinDir = Files.createTempDirectory("erlt_ebin")

      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt")).map(_.getName).map(_.dropRight(5)).sorted
      val moduleArgs = moduleNames.map(_ ++ ".erlt").mkString(" ")

      s"./parser -idir $iDirPath -odir $sterlangOutDir".!!
      s"./erlt --build compile --src-dir $iDirPath --build-dir $erltBuildDir -o $erltEBinDir $moduleArgs".!!

      moduleNames.foreach { m =>
        val erltPath = s"$iDirPath/$m.erlt"
        it(erltPath) {
          testModule(m, sterlangOutDir, erltBuildDir)
          testModuleApi(m, sterlangOutDir, erltBuildDir)
        }
      }
    }
  }

  def testModule(module: String, devDir: Path, erltDir: Path): Unit = {
    val devProgram = etf.programFromFileDev(s"$devDir/$module.etf")
    val erltProgram = etf.programFromFileErlt(s"$erltDir/$module.etf")
    assert(devProgram === erltProgram)
  }

  def testModuleApi(module: String, devDir: Path, erltDir: Path): Unit = {
    val devModuleApi = etf.moduleApiFromFileDev(s"$devDir/$module.etf")
    val erltModuleApi = etf.moduleApiFromFileErlt(s"$erltDir/$module.defs.etf")
    assert(devModuleApi === erltModuleApi)
  }
}
