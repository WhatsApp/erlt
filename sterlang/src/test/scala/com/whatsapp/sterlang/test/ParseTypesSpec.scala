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

import com.whatsapp.sterlang.Ast._
import com.whatsapp.sterlang.Parser
import com.whatsapp.sterlang.Pos.NP

class ParseTypesSpec extends org.scalatest.FunSpec {

  def testType(input: String, tp: Type): Unit = {
    it(input) {
      val res = Parser.typeFromString(input)
      if (!res.successful) {
        fail(res.toString)
      }
      assert(res.get === tp)
    }
  }

  def testSpecType(input: String, spec: Spec): Unit = {
    it(input) {
      val res = Parser.specTypeFromString(input)
      if (!res.successful) {
        fail(res.toString)
      }
      assert(res.get === spec)
    }
  }

  def testTypeDefs(input: String, typeDefs: List[EnumDef]): Unit = {
    it(input) {
      val res = Parser.enumDefsFromString(input)
      if (!res.successful) {
        fail(res.toString)
      }
      assert(res.get === typeDefs)
    }
  }

  describe("Types") {
    testType(
      "fun(() -> A)",
      FunType(List(), TypeVar("A")(NP))(NP),
    )

    testType(
      "fun(() -> {})",
      FunType(List(), TupleType(List())(NP))(NP),
    )

    testType(
      "fun((option(A)) -> A)",
      FunType(List(UserType(LocalName("option"), List(TypeVar("A")(NP)))(NP)), TypeVar("A")(NP))(NP),
    )

    testType(
      "fun((option(A), option(B), fun((A) -> B)) -> option(B))",
      FunType(
        List(
          UserType(LocalName("option"), List(TypeVar("A")(NP)))(NP),
          UserType(LocalName("option"), List(TypeVar("B")(NP)))(NP),
          FunType(List(TypeVar("A")(NP)), TypeVar("B")(NP))(NP),
        ),
        UserType(LocalName("option"), List(TypeVar("B")(NP)))(NP),
      )(NP),
    )

    testType(
      "[A]",
      ListType(TypeVar("A")(NP))(NP),
    )

    testType(
      "fun(([A]) -> [A])",
      FunType(List(ListType(TypeVar("A")(NP))(NP)), ListType(TypeVar("A")(NP))(NP))(NP),
    )

    testSpecType(
      "foo([A]) -> [A]",
      Spec(
        new LocalFunName("foo", 1),
        FunType(List(ListType(TypeVar("A")(NP))(NP)), ListType(TypeVar("A")(NP))(NP))(NP),
      )(NP),
    )
  }

  describe("Type definitions") {
    testTypeDefs("-enum void() :: .", List(EnumDef("void", List(), List())(NP)))

    testTypeDefs("-enum void(A) :: .", List(EnumDef("void", List(TypeVar("A")(NP)), List())(NP)))

    testTypeDefs(
      "-enum box(A) :: box{A}.",
      List(EnumDef("box", List(TypeVar("A")(NP)), List(EnumCon("box", List(TypeVar("A")(NP)))(NP)))(NP)),
    )

    testTypeDefs(
      "-enum box(A) :: box{(A)}.",
      List(EnumDef("box", List(TypeVar("A")(NP)), List(EnumCon("box", List(TypeVar("A")(NP)))(NP)))(NP)),
    )

    testTypeDefs("-enum box() :: box{}.", List(EnumDef("box", List(), List(EnumCon("box", List())(NP)))(NP)))

    testTypeDefs(
      "-enum box2(A) :: box2{A, A}.",
      List(
        EnumDef("box2", List(TypeVar("A")(NP)), List(EnumCon("box2", List(TypeVar("A")(NP), TypeVar("A")(NP)))(NP)))(NP)
      ),
    )

    testTypeDefs(
      "-enum box2(A, B) :: box2{A, B}.",
      List(
        EnumDef(
          "box2",
          List(TypeVar("A")(NP), TypeVar("B")(NP)),
          List(EnumCon("box2", List(TypeVar("A")(NP), TypeVar("B")(NP)))(NP)),
        )(NP)
      ),
    )

    testTypeDefs(
      "-enum box2(A, B) :: box2{A, B} | box1{A}.",
      List(
        EnumDef(
          "box2",
          List(TypeVar("A")(NP), TypeVar("B")(NP)),
          List(
            EnumCon("box2", List(TypeVar("A")(NP), TypeVar("B")(NP)))(NP),
            EnumCon("box1", List(TypeVar("A")(NP)))(NP),
          ),
        )(NP)
      ),
    )

    testTypeDefs(
      "-enum box() :: box{}. -enum box2(A, B) :: box2{A, B} | box1{A}.",
      List(
        EnumDef("box", List(), List(EnumCon("box", List())(NP)))(NP),
        EnumDef(
          "box2",
          List(TypeVar("A")(NP), TypeVar("B")(NP)),
          List(
            EnumCon("box2", List(TypeVar("A")(NP), TypeVar("B")(NP)))(NP),
            EnumCon("box1", List(TypeVar("A")(NP)))(NP),
          ),
        )(NP),
      ),
    )

    testTypeDefs(
      """-enum box1(A) :: box1{A}.
        |-enum box2(A) :: box2{box1(A)}.
        |""".stripMargin,
      List(
        EnumDef("box1", List(TypeVar("A")(NP)), List(EnumCon("box1", List(TypeVar("A")(NP)))(NP)))(NP),
        EnumDef(
          "box2",
          List(TypeVar("A")(NP)),
          List(EnumCon("box2", List(UserType(LocalName("box1"), List(TypeVar("A")(NP)))(NP)))(NP)),
        )(NP),
      ),
    )
  }

}
