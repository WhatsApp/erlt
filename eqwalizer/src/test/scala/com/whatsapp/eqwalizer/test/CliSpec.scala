package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.Main

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Paths}

class CliSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  describe("eqwalizer Main") {
    it("prints help by default") {
      checkAction(Main.main(Array()), "help.txt")
    }

    it("prints help for wrong command") {
      checkAction(Main.main(Array("do_it", "misc")), "help.txt")
    }

    it("type checks a single fun") {
      checkAction(Main.main(Array("check", "misc:test39_pos/1")), "check01.txt")
    }

    it("type checks a single module") {
      checkAction(Main.main(Array("check", "as_pat")), "check02.txt")
    }

    it("debugs a single fun") {
      checkAction(Main.main(Array("debug", "wip_maps:bad_mixed_update4/1")), "debug01.txt")
    }

    it("debugs a single module") {
      checkAction(Main.main(Array("debug", "wip_maps")), "debug02.txt")
    }

    it("reports a missing beam") {
      checkErrAction(Main.main(Array("check", "missing")), "missing.txt")
    }
  }

  private def checkAction(action: => Unit, expOutPath: String): Unit = {
    val out = new ByteArrayOutputStream
    Console.withOut(out)(action)
    val outText = out.toString

    val expPath = Paths.get(s"test_projects/_cli/$expOutPath")
    if (generateOut) Files.write(expPath, outText.getBytes)
    val expText = new String(Files.readAllBytes(expPath))

    assert(outText === expText)
  }

  private def checkErrAction(action: => Unit, expOutPath: String): Unit = {
    val out = new ByteArrayOutputStream
    Console.withErr(out)(action)
    val outText = out.toString

    val expPath = Paths.get(s"test_projects/_cli/$expOutPath")
    if (generateOut) Files.write(expPath, outText.getBytes)
    val expText = new String(Files.readAllBytes(expPath))

    assert(outText === expText)
  }
}
