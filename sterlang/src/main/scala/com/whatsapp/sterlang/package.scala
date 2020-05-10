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

package com.whatsapp

package object sterlang {
  type Env = Map[String, STypes.TypeSchema]
  @inline
  val S = Ast

  case class Context(enumDefs: List[S.EnumDef],
                     specs: List[S.Spec],
                     aliases: List[S.TypeAlias],
                     opaques: Set[S.TypeId],
                     env: Env) {
    def extend(program: S.Program): Context = {
      val opaqueAliases = program.opaques.map { opaque =>
        S.TypeAlias(opaque.name, opaque.params, opaque.body)(opaque.p)
      }
      Context(
        enumDefs ++ program.enumDefs,
        specs ++ program.specs,
        aliases ++ program.typeAliases ++ opaqueAliases,
        opaques,
        env,
      )
    }
  }

  case class ModuleApi(enumDefs: List[S.EnumDef],
                       aliases: List[S.TypeAlias],
                       specs: List[S.Spec],
                       opaques: List[S.TypeId])

  val nativeOpaques: Set[S.TypeId] =
    Set(
      S.TypeId(S.LocalName("any"), 0),
      S.TypeId(S.LocalName("atom"), 0),
      S.TypeId(S.LocalName("binary"), 0),
      S.TypeId(S.LocalName("bitstring"), 0),
      S.TypeId(S.LocalName("byte"), 0),
      S.TypeId(S.LocalName("identifier"), 0),
      S.TypeId(S.LocalName("iodata"), 0),
      S.TypeId(S.LocalName("iolist"), 0),
      S.TypeId(S.LocalName("map"), 2),
      S.TypeId(S.LocalName("neg_integer"), 0),
      S.TypeId(S.LocalName("none"), 0),
      S.TypeId(S.LocalName("non_neg_integer"), 0),
      S.TypeId(S.LocalName("number"), 0),
      S.TypeId(S.LocalName("pid"), 0),
      S.TypeId(S.LocalName("port"), 0),
      S.TypeId(S.LocalName("pos_integer"), 0),
      S.TypeId(S.LocalName("reference"), 0),
      S.TypeId(S.LocalName("term"), 0),
      S.TypeId(S.LocalName("timeout"), 0),
      S.TypeId(S.LocalName("node"), 0),
      S.TypeId(S.LocalName("no_return"), 0),

      S.TypeId(S.LocalName("integer"), 0),
      S.TypeId(S.LocalName("char"), 0),
      S.TypeId(S.LocalName("float"), 0),
      S.TypeId(S.LocalName("boolean"), 0),
      S.TypeId(S.LocalName("list"), 1),
      S.TypeId(S.LocalName("string"), 0),
    )

  val nativeAliases: List[S.TypeAlias] =
    List(
      S.TypeAlias("node", List.empty, S.UserType(S.LocalName("atom"), List.empty)(Pos.NP))(Pos.NP),
      S.TypeAlias("no_return", List.empty, S.UserType(S.LocalName("none"), List.empty)(Pos.NP))(Pos.NP),
    )
}
