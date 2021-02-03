/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.corq.test

import com.whatsapp.corq.test.util.TcDiagnosticsText
import java.nio.file.{Files, Paths}

import com.whatsapp.corq.ast.DB

class CheckSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  testDir(
    srcDir = "test_projects/check/src",
    ebinDir = "test_projects/_build/default/lib/check/ebin"
  )

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
      val diag = TcDiagnosticsText.checkForms(beamFile).mkString("", "\n", "\n")
      println(s"diagnostics: $diag")
      val expPath = Paths.get(s"$srdDir/$module.erl.check")
      if (generateOut) Files.write(expPath, diag.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp == diag)
    }
}
