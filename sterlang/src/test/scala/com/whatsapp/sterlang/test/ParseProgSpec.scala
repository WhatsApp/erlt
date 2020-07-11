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

class ParseProgSpec extends org.scalatest.FunSpec {
  def testProg(input: String, prog: Program): Unit = {
    val res = Parser.programFromString(input)
    if (!res.successful) {
      fail(res.toString)
    }
    assert(res.get === prog)
  }

  describe("Programs with all elements") {
    it("Should be parsed correctly") {
      testProg(
        """
          |-lang([erl2, st]).
          |-module(test).
          |-depends_on([mod1]).
          |-depends_on([mod2]).
          |-enum box(A) :: box{A}.
          |-type boxAlias(A) :: box(A).
          |-enum box2(A) :: box2{A}.
          |-type boxAlias2(A) :: box2(A).
          |-spec box_id(boxAlias(A)) -> boxAlias(A).
          |box_id(X) -> X.
          |""".stripMargin,
        Program(
          require = Require(List("mod1", "mod2")),
          enumDefs = List(
            EnumDef("box", List(TypeVar("A")(NP)), List(EnumCon("box", List(TypeVar("A")(NP)))(NP)))(NP),
            EnumDef("box2", List(TypeVar("A")(NP)), List(EnumCon("box2", List(TypeVar("A")(NP)))(NP)))(NP),
          ),
          typeAliases = List(
            TypeAlias("boxAlias", List(TypeVar("A")(NP)), UserType(LocalName("box"), List(TypeVar("A")(NP)))(NP))(NP),
            TypeAlias("boxAlias2", List(TypeVar("A")(NP)), UserType(LocalName("box2"), List(TypeVar("A")(NP)))(NP))(NP),
          ),
          specs = List(
            Spec(
              new LocalFunName("box_id", 1),
              FunType(
                List(UserType(LocalName("boxAlias"), List(TypeVar("A")(NP)))(NP)),
                UserType(LocalName("boxAlias"), List(TypeVar("A")(NP)))(NP),
              )(NP),
            )(NP)
          ),
          funs = List(
            Fun(
              new LocalFunName("box_id", 1),
              List(
                Clause(
                  List(VarPat("X")(NP)),
                  List(),
                  Body(List(), ValDef(WildPat()(NP), VarExp(new LocalVarName("X"))(NP))),
                )
              ),
            )(NP)
          ),
          opaques = List.empty,
          exports = Set.empty,
          exportTypes = Set.empty,
          module = "test",
          lang = ST,
          imports = Map.empty,
          importTypes = Map.empty,
        ),
      )
    }
  }
}
