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

import com.whatsapp.sterlang.{AstUtil, etf}

class SccSpec extends org.scalatest.funspec.AnyFunSpec {
  def testScc(input: String, expSccs: List[List[String]]): Unit = {
    it(expSccs.toString()) {
      val prog = etf.programFromString(input)
      val sccs = AstUtil.buildSCC(prog.funs, prog.module)
      assert(sccs === expSccs)
    }
  }

  describe("SCCs") {
    testScc(
      """
        |-module(test).
        |""".stripMargin,
      List(),
    )

    testScc(
      """
        |-module(test).
        |even() -> odd().
        |odd() -> even().
        |""".stripMargin,
      List(List("even/0", "odd/0")),
    )

    testScc(
      """
        |-module(test).
        |odd() -> even().
        |even() -> odd().
        |""".stripMargin,
      List(List("odd/0", "even/0")),
    )

    testScc(
      """
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
        |-module(test).
        |main() -> test:foo1().
        |foo1() -> 1.
        |""".stripMargin,
      List(List("foo1/0"), List("main/0")),
    )

    testScc(
      """
        |-module(test).
        |main() -> fun test:foo2/0.
        |foo2() -> 1.
        |""".stripMargin,
      List(List("foo2/0"), List("main/0")),
    )
  }
}
