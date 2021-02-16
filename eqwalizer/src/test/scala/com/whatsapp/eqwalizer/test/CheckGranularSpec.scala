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

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.test.util.TcDiagnosticsText

import java.nio.file.{Files, Paths}

class CheckGranularSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = true

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
