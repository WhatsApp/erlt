package com.whatsapp.sterlang.test.it

import java.io.File
import java.nio.file.{Files, Path}

import com.whatsapp.sterlang.dev.EtfDev
import com.whatsapp.sterlang.{EtfErltc, Etf}

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
      val erltcBuildDir = Files.createTempDirectory("erltc_build")
      val erltcEBinDir = Files.createTempDirectory("erltc_ebin")

      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt")).map(_.getName).map(_.dropRight(5)).sorted
      val moduleArgs = moduleNames.map(_ ++ ".erlt").mkString(" ")

      s"./parser -idir $iDirPath -odir $sterlangOutDir".!!
      s"./erltc --build compile --src-dir $iDirPath --build-dir $erltcBuildDir -o $erltcEBinDir --skip-type-checking $moduleArgs".!!

      moduleNames.foreach { m =>
        val erltPath = s"$iDirPath/$m.erlt"
        it(erltPath) {
          testModule(m, sterlangOutDir, erltcBuildDir)
          testModuleApi(m, sterlangOutDir, erltcBuildDir)
        }
      }
    }
  }

  def testModule(module: String, devDir: Path, erltcDir: Path): Unit = {
    val devProgram = EtfDev.programFromFile(s"$devDir/$module.etf")
    val erltProgram = EtfErltc.programFromFile(s"$erltcDir/$module.etf")
    assert(devProgram === erltProgram)
  }

  def testModuleApi(module: String, devDir: Path, erltcDir: Path): Unit = {
    val devModuleApi = EtfDev.moduleApiFromFile(s"$devDir/$module.etf")
    val erltModuleApi = EtfErltc.moduleApiFromFile(s"$erltcDir/$module.defs.etf")
    assert(devModuleApi === erltModuleApi)
  }
}
