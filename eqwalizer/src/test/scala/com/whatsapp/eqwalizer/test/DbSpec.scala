package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.DB

import java.nio.file.Paths

class DbSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("module DB") {
    it("should locate beam files from test projects") {
      val relPath = "test_projects/_build/default/lib/check/ebin/misc.beam"
      val absPath = Paths.get(relPath).toAbsolutePath.toString
      assert(DB.beamLocation("misc") === Some(absPath))
    }

    it("should not locate unknown modules") {
      assert(DB.beamLocation("unknown_module") === None)
    }
  }
}
