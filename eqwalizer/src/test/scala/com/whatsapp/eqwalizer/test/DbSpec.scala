package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.DB

class DbSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("module DB") {
    it("should locate beam files from test projects") {
      assert(DB.beamLocation("misc") === Some("test_projects/_build/default/lib/check/ebin/misc.beam"))
    }

    it("should not locate unknown modules") {
      assert(DB.beamLocation("unknown_module") === None)
    }
  }
}
