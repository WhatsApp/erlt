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

package com.whatsapp.coralizer.test

import com.whatsapp.coralizer.test.util.TcDiagnosticsText
import java.nio.file.{Files, Path, Paths}

import com.whatsapp.coralizer.ast.DB
import com.whatsapp.coralizer.test.util.TcDiagnosticsText.Checked

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

  def testBeam(srcDir: String, ebinDir: String, module: String): Unit =
    it(s"$srcDir/$module.erl") {
      val beamFile = s"$ebinDir/$module.beam"
      val Checked(erl, core) = TcDiagnosticsText.check(beamFile)
      expTest(erl, Paths.get(s"$srcDir/$module.erl.check"))
      expTest(core, Paths.get(s"$srcDir/$module.core.check"))
    }

  private def expTest(diagnostics: String, expPath: Path): Unit = {
    if (generateOut) {
      Files.write(expPath, diagnostics.getBytes)
    }
    val exp = new String(Files.readAllBytes(expPath))
    assert(exp == diagnostics)
  }

}
