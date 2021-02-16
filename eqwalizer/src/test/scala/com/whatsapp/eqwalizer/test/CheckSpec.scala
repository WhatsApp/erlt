package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.test.util.TcDiagnosticsText

import java.nio.file.{Files, Paths}

class CheckSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  testDir(srcDir = "test_projects/check/src", ebinDir = "test_projects/_build/default/lib/check/ebin")

  def testDir(srcDir: String, ebinDir: String): Unit = {
    describe(ebinDir) {
      val files = Paths.get(ebinDir).toFile.listFiles().sorted
      val beamFiles = files.filter(f => f.isFile && f.getPath.endsWith(".beam"))
      val modules = beamFiles.map(_.getName).map(_.dropRight(".beam".length))
      modules.foreach(testBeam(srcDir, ebinDir, _))
    }
  }

  def testBeam(srdDir: String, ebinDir: String, module: String): Unit =
    it(s"$srdDir/$module.erl") {
      val beamFile = s"$ebinDir/$module.beam"
      val diag = TcDiagnosticsText.checkFile(beamFile).mkString("", "\n", "\n")
      val expPath = Paths.get(s"$srdDir/$module.erl.check")
      if (generateOut) Files.write(expPath, diag.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp == diag)
    }
}
