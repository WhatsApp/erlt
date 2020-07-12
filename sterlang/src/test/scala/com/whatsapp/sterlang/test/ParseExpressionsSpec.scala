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

class ParseExpressionsSpec extends org.scalatest.FunSpec {

  def testPat(input: String, pat: Pat): Unit = {
    it(input) {
      val res = Parser.patFromString(input)
      assert(res.get === pat)
    }
  }

  def testExp(input: String, exp: Exp): Unit = {
    it(input) {
      val res = Parser.expFromString(input)
      if (!res.successful) {
        fail(res.toString)
      }
      assert(res.get === exp)
    }
  }

  describe("Patterns") {
    testPat("_", WildPat()(NP))

    testPat("X", VarPat("X")(NP))

    testPat("{}", TuplePat(List())(NP))

    testPat("({})", TuplePat(List())(NP))

    testPat("(X)", VarPat("X")(NP))

    testPat("{X, Y}", TuplePat(List(VarPat("X")(NP), VarPat("Y")(NP)))(NP))

    testPat("#{}", RecordPat(List(), false)(NP))

    testPat("##{}", RecordPat(List(), true)(NP))

    testPat("#{x := XVal}", RecordPat(List(Field("x", VarPat("XVal")(NP))), false)(NP))

    testPat(
      "#{x := XVal, y := YVal}",
      RecordPat(
        List(
          Field("x", VarPat("XVal")(NP)),
          Field("y", VarPat("YVal")(NP)),
        ),
        false,
      )(NP),
    )

    testPat("##{x := XVal}", RecordPat(List(Field("x", VarPat("XVal")(NP))), true)(NP))

    testPat(
      "##{x := XVal} = MyRec",
      AndPat(RecordPat(List(Field("x", VarPat("XVal")(NP))), true)(NP), VarPat("MyRec")(NP))(NP),
    )
  }

  describe("Expressions") {
    testExp("true", BoolExp(true)(NP))

    testExp("false", BoolExp(false)(NP))

    testExp("42", NumberExp(42)(NP))

    testExp("""  "string"  """, StringExp("string")(NP))

    testExp("X", VarExp(new LocalVarName("X"))(NP))

    testExp("(X)", VarExp(new LocalVarName("X"))(NP))

    testExp("X.y", SelExp(VarExp(new LocalVarName("X"))(NP), "y")(NP))

    testExp("(X.y).z", SelExp(SelExp(VarExp(new LocalVarName("X"))(NP), "y")(NP), "z")(NP))

    testExp("X.y.z", SelExp(SelExp(VarExp(new LocalVarName("X"))(NP), "y")(NP), "z")(NP))

    testExp("X(Y)", AppExp(VarExp(new LocalVarName("X"))(NP), List(VarExp(new LocalVarName("Y"))(NP)))(NP))

    testExp(
      "(X(Y))(Z)",
      AppExp(
        AppExp(VarExp(new LocalVarName("X"))(NP), List(VarExp(new LocalVarName("Y"))(NP)))(NP),
        List(VarExp(new LocalVarName("Z"))(NP)),
      )(NP),
    )

    testExp(
      "X(Y)(Z)",
      AppExp(
        AppExp(VarExp(new LocalVarName("X"))(NP), List(VarExp(new LocalVarName("Y"))(NP)))(NP),
        List(VarExp(new LocalVarName("Z"))(NP)),
      )(NP),
    )

//  TODO: this is not parsed naively
//    testExp(
//      "x(y).z",
//      SelExp(
//        AppExp(VarExp(LocalName("x")), List(VarExp(LocalName("y")))),
//        "z"))

    testExp(
      "X(Y.z)",
      AppExp(VarExp(new LocalVarName("X"))(NP), List(SelExp(VarExp(new LocalVarName("Y"))(NP), "z")(NP)))(NP),
    )

    testExp("#{}", RecordExp(List())(NP))

    testExp("#{x => X}", RecordExp(List(Field("x", VarExp(new LocalVarName("X"))(NP))))(NP))

    testExp("#{x => 1, y => 2}", RecordExp(List(Field("x", NumberExp(1)(NP)), Field("y", NumberExp(2)(NP))))(NP))

    testExp(
      "R #{x := 1}",
      RecordUpdateExp(VarExp(new LocalVarName("R"))(NP), RecordExp(List(Field("x", NumberExp(1)(NP))))(NP))(NP),
    )

  }

  describe("Operations") {
    testExp(
      "X orelse Y",
      BinOpExp(BoolConnOp(Or), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X andalso Y",
      BinOpExp(BoolConnOp(And), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X == Y",
      BinOpExp(Cmp(Eq), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X <= Y",
      BinOpExp(Cmp(LtEq), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X < Y",
      BinOpExp(Cmp(Lt), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X >= Y",
      BinOpExp(Cmp(GtEq), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X > Y",
      BinOpExp(Cmp(Gt), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X <> Y",
      BinOpExp(Cmp(NEq), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X + Y",
      BinOpExp(Arith(Plus), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X - Y",
      BinOpExp(Arith(Minus), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X * Y",
      BinOpExp(Arith(Times), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "X / Y",
      BinOpExp(Arith(Div), VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "- Y",
      UOpExp(UMinus, VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "not Y",
      UOpExp(UNot, VarExp(new LocalVarName("Y"))(NP))(NP),
    )
  }

  describe("Lists") {
    testExp(
      "[X | Y]",
      ConsExp(VarExp(new LocalVarName("X"))(NP), VarExp(new LocalVarName("Y"))(NP))(NP),
    )

    testExp(
      "[X | [Y | Z]]",
      ConsExp(
        VarExp(new LocalVarName("X"))(NP),
        ConsExp(VarExp(new LocalVarName("Y"))(NP), VarExp(new LocalVarName("Z"))(NP))(NP),
      )(NP),
    )

    testExp(
      "[]",
      ListExp(List())(NP),
    )

    testExp(
      "[A]",
      ListExp(List(VarExp(new LocalVarName("A"))(NP)))(NP),
    )
  }

  describe("Enums") {
    testExp(
      "option.none{}",
      EnumConExp(LocalName("option"), "none", List())(NP),
    )
    testExp(
      "option.some{A}",
      EnumConExp(LocalName("option"), "some", List(VarExp(new LocalVarName("A"))(NP)))(NP),
    )
    testExp(
      "pair.pair2{A, A}",
      EnumConExp(
        LocalName("pair"),
        "pair2",
        List(VarExp(new LocalVarName("A"))(NP), VarExp(new LocalVarName("A"))(NP)),
      )(NP),
    )
  }

}
