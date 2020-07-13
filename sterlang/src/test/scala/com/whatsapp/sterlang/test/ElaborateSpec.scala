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

import com.whatsapp.sterlang.{Ast, _}

class ElaborateSpec extends org.scalatest.FunSpec {
  val S = Ast

  def testTyping(input: String, expOutput: String): Unit = {
    val prog = etf.programFromString(input)
    val sw = new StringWriter()
    val vars = new Vars()
    val context = Context(prog.enumDefs, prog.specs, prog.typeAliases, Set.empty, Map.empty)
    val elaborate = new Elaborate(vars, context, prog)
    val (annDefs, env) = elaborate.elaborateFuns(prog.funs)
    TypePrinter2(vars, Some(sw)).printFunsTypeSchemes(annDefs, env)
    val actualOutput = sw.toString
    assert(actualOutput == expOutput)
  }

  def testTypeError(input: String): Unit = {
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
      case te: TypeError =>
        info("caught error:" + te.toString)
      case te: TypeMismatchError =>
        info("caught error:" + te.toString)
    }
  }

  describe("Enums") {
    it("box()") {
      val input =
        """
          |-lang([erl2, st]).
          |-module(test).
          |-enum box() :: box{}.
          |b() -> box.box{}.
          |""".stripMargin
      val output =
        """-spec b() -> box().
          |""".stripMargin
      testTyping(input, output)
    }

    it("box(A)") {
      val input =
        """
          |-lang([erl2, st]).
          |-module(test).
          |-enum box(A) :: box{A}.
          |box1(X) -> box.box{X}.
          |box2(X) -> box.box{{X, X}}.
          |""".stripMargin
      val output =
        """-spec box1(A) -> box(A).
          |-spec box2(A) -> box({A, A}).
          |""".stripMargin
      testTyping(input, output)
    }

    it("Option(A)") {
      val input =
        """
          |-lang([erl2, st]).
          |-module(test).
          |-enum option(A) :: none{} | some{A}.
          |mkNone(A) -> option.none{}.
          |mkSome(A) -> option.some{A}.
          |none() -> option.none{}.
          |someInt() -> mkSome(0).
          |someStr() -> mkSome("").
          |optInts() -> [none(), someInt()].
          |optStrs() -> [none(), someStr()].
          |""".stripMargin
      val output =
        """-spec mkNone(A) -> option(B).
          |-spec mkSome(A) -> option(A).
          |-spec none() -> option(A).
          |-spec someInt() -> option(integer()).
          |-spec someStr() -> option(string()).
          |-spec optInts() -> list(option(integer())).
          |-spec optStrs() -> list(option(string())).
          |""".stripMargin
      testTyping(input, output)
    }

    it("Catching error") {
      val input =
        """
          |-lang([erl2, st]).
          |-module(test).
          |-enum option(A) :: none{} | some{A}.
          |mkSome(A) -> option.some{A}.
          |someInt() -> mkSome(0).
          |someStr() -> mkSome("").
          |mix() -> [someInt(), someStr()].
          |""".stripMargin
      testTypeError(input)
    }
  }

  describe("Enum case expressions") {
    it("some case expressions") {
      val input =
        """
          |-lang([erl2, st]).
          |-module(test).
          |-enum option(A) :: none{} | some{A}.
          |-enum num() :: z{} | s{num()}.
          |-enum my_list(A) ::  nil{} | cons{A, my_list(A)}.
          |getOpt(Opt, DefVal) ->
          |  case Opt of
          |      option.some{A} -> A;
          |      option.none{} -> DefVal
          |  end.
          |next(X) -> num.s{X}.
          |prev(X) ->
          |  case X of
          |      num.s{X1} -> X1;
          |      num.z{} -> num.z{}
          |  end.
          |append(Xs, Ys) ->
          |  case Xs of
          |      my_list.nil{} -> Ys;
          |      my_list.cons{X1, Xs1} -> my_list.cons{X1, append(Xs1, Ys)}
          |  end.
          |map(Xs, F) ->
          |  case Xs of
          |      my_list.nil{} -> my_list.nil{};
          |      my_list.cons{X1, Xs1} -> my_list.cons{F(X1), map(Xs1, F)}
          |  end.
          |""".stripMargin
      val output =
        """-spec getOpt(option(A), A) -> A.
          |-spec next(num()) -> num().
          |-spec prev(num()) -> num().
          |-spec append(my_list(A), my_list(A)) -> my_list(A).
          |-spec map(my_list(A), fun((A) -> B)) -> my_list(B).
          |""".stripMargin
      testTyping(input, output)
    }
  }
}
