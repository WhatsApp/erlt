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
import com.whatsapp.sterlang.Doc.ZRange
import com.whatsapp.sterlang.etf

class ParseProgSpec extends org.scalatest.funspec.AnyFunSpec {
  def testProg(input: String, expProg: Program): Unit = {
    val prog = etf.programFromString(input)
    assert(prog === expProg)
  }

  describe("Programs with all elements") {
    it("Should be parsed correctly") {
      testProg(
        """
          |-lang(st).
          |-module(test).
          |-enum box(A) :: box{A}.
          |-type boxAlias(A) :: box(A).
          |-enum box2(A) :: box2{A}.
          |-type boxAlias2(A) :: box2(A).
          |-spec box_id(boxAlias(A)) -> boxAlias(A).
          |box_id(X) -> X.
          |test(X) -> if X -> true; true -> false end.
          |""".stripMargin,
        Program(
          enumDefs = List(
            EnumDef("box", List(TypeVar("A")(ZRange)), List(EnumCon("box", List(TypeVar("A")(ZRange)))(ZRange)))(
              ZRange
            ),
            EnumDef("box2", List(TypeVar("A")(ZRange)), List(EnumCon("box2", List(TypeVar("A")(ZRange)))(ZRange)))(
              ZRange
            ),
          ),
          structDefs = List.empty,
          typeAliases = List(
            TypeAlias(
              "boxAlias",
              List(TypeVar("A")(ZRange)),
              UserType(LocalName("box"), List(TypeVar("A")(ZRange)))(ZRange),
            )(ZRange),
            TypeAlias(
              "boxAlias2",
              List(TypeVar("A")(ZRange)),
              UserType(LocalName("box2"), List(TypeVar("A")(ZRange)))(ZRange),
            )(ZRange),
          ),
          specs = List(
            Spec(
              new LocalFunName("box_id", 1),
              FunType(
                List(UserType(LocalName("boxAlias"), List(TypeVar("A")(ZRange)))(ZRange)),
                UserType(LocalName("boxAlias"), List(TypeVar("A")(ZRange)))(ZRange),
              )(ZRange),
            )(ZRange)
          ),
          funs = List(
            Fun(
              new LocalFunName("box_id", 1),
              List(
                Clause(
                  List(VarPat("X")(ZRange)),
                  List(),
                  Body(List(), ValDef(WildPat()(ZRange), VarExp(new LocalVarName("X"))(ZRange))),
                )
              ),
            )(ZRange),
            Fun(
              new LocalFunName("test", 1),
              List(
                Clause(
                  List(VarPat("X")(ZRange)),
                  List(),
                  Body(
                    List(),
                    ValDef(
                      WildPat()(ZRange),
                      IfExp(
                        List(
                          IfClause(
                            List(Guard(List(VarExp(new LocalVarName("X"))(ZRange)))),
                            Body(List(), ValDef(WildPat()(ZRange), BoolExp(true)(ZRange))),
                          ),
                          IfClause(
                            List(Guard(List(BoolExp(true)(ZRange)))),
                            Body(List(), ValDef(WildPat()(ZRange), BoolExp(false)(ZRange))),
                          ),
                        )
                      )(ZRange),
                    ),
                  ),
                )
              ),
            )(ZRange),
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
