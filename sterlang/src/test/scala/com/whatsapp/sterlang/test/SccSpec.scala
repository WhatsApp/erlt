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

import com.whatsapp.sterlang.{Parser, SyntaxUtil}

class SccSpec extends org.scalatest.FunSpec {
  def testScc(input: String, expSccs: List[List[String]]): Unit = {
    it(expSccs.toString()) {
      val res = Parser.programFromString(input)
      if (!res.successful) {
        fail(res.toString)
      }
      val rawProgram = res.get
      val sccs = SyntaxUtil.buildSCC(rawProgram.funs, rawProgram.module)
      assert(sccs === expSccs)
    }
  }

  describe("SCCs") {
    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |""".stripMargin,
      List(),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |even() -> odd().
        |odd() -> even().
        |""".stripMargin,
      List(List("even/0", "odd/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |odd() -> even().
        |even() -> odd().
        |""".stripMargin,
      List(List("odd/0", "even/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |odd1() -> even1().
        |odd2() -> even2().
        |even1() -> odd1().
        |even2() -> odd2().
        |""".stripMargin,
      List(List("odd1/0", "even1/0"), List("odd2/0", "even2/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |odd2() -> even2().
        |odd1() -> even1().
        |even1() -> odd1().
        |even2() -> odd2().
        |""".stripMargin,
      List(List("odd2/0", "even2/0"), List("odd1/0", "even1/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |main1() -> even1().
        |odd2() -> even2().
        |odd1() -> even1().
        |even1() -> odd1().
        |even2() -> odd2().
        |main2() -> even2().
        |""".stripMargin,
      List(List("odd2/0", "even2/0"), List("odd1/0", "even1/0"), List("main1/0"), List("main2/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |main2() -> even2().
        |odd2() -> even2().
        |odd1() -> even1().
        |even1() -> odd1().
        |even2() -> odd2().
        |main1() -> even1().
        |""".stripMargin,
      List(List("odd2/0", "even2/0"), List("main2/0"), List("odd1/0", "even1/0"), List("main1/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |main() -> test:foo1().
        |foo1() -> 1.
        |""".stripMargin,
      List(List("foo1/0"), List("main/0")),
    )

    testScc(
      """
        |-lang([erl2, st]).
        |-module(test).
        |main() -> fun test:foo2/0.
        |foo2() -> 1.
        |""".stripMargin,
      List(List("foo2/0"), List("main/0")),
    )
  }
}
