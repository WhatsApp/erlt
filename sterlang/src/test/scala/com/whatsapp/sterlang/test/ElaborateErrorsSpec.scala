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

package com.whatsapp.sterlang.test

import java.io.StringWriter

import com.whatsapp.sterlang.Doc.ZRange
import com.whatsapp.sterlang._

class ElaborateErrorsSpec extends org.scalatest.funspec.AnyFunSpec {

  def testSpecError(input: String, expected: SpecError): Unit = {
    val prog = etf.programFromString(input)
    val sw = new StringWriter()
    try {
      val vars = new Vars()
      val context = Context(prog.enumDefs, prog.specs, prog.typeAliases, Set.empty, Map.empty)
      val elaborate = new Elaborate(vars, context, prog)
      elaborate.elaborateFuns(prog.funs)
      val actualOutput = sw.toString
      fail(actualOutput)
    } catch {
      case se: SpecError =>
        assert(se.fName === expected.fName)
        assert(se.elabType === expected.elabType)
        assert(se.specType === expected.specType)
    }
  }

  describe("Spec mismatch") {
    it("example1") {
      val input =
        """
          |-lang(st).
          |-module(test).
          |-spec call_id(A) -> B.
          |call_id(X) -> id(X).
          |
          |id(X) -> X.
          |""".stripMargin
      val expected =
        new SpecError(ZRange, "call_id/1", "fun((A) -> B)", "fun((A) -> A)")
      testSpecError(input, expected)
    }
  }
}
