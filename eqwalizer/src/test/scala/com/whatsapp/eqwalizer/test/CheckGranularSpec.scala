package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.test.util.TcDiagnosticsText

import java.nio.file.{Files, Paths}

class CheckGranularSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  val srcDir = "test_projects/check/src"
  val ebinDir = "test_projects/_build/default/lib/check/ebin"

  describe("Type checking single funs") {
    testFun("misc", Id("test80_neg", 1))
    testFun("misc", Id("test39_pos", 1))
  }

  def testFun(module: String, id: Id): Unit =
    it(s"$srcDir/$module.erl $id") {
      val beamFile = s"$ebinDir/$module.beam"
      val diag = TcDiagnosticsText.checkFun(beamFile, id).mkString("", "\n", "\n")
      val expPath = Paths.get(s"$srcDir/$module.erl.${id.name}.check")
      if (generateOut) Files.write(expPath, diag.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp == diag)
    }
}
