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

  case class Context(
      enumDefs: List[Ast.EnumDef],
      structDefs: List[Ast.StructDef],
      specs: List[Ast.Spec],
      aliases: List[Ast.TypeAlias],
      opaques: Set[Ast.TypeId],
      env: Env,
  ) {
    def extend(program: Ast.Program): Context = {
      val opaqueAliases = program.opaques.map { opaque =>
        Ast.TypeAlias(opaque.name, opaque.params, opaque.body)(opaque.r)
      }
      Context(
        enumDefs ++ program.enumDefs,
        structDefs ++ program.structDefs,
        specs ++ program.specs,
        aliases ++ program.typeAliases ++ opaqueAliases,
        opaques,
        env,
      )
    }
  }

  case class ModuleApi(
      enumDefs: List[Ast.EnumDef],
      structDefs: List[Ast.StructDef],
      aliases: List[Ast.TypeAlias],
      specs: List[Ast.Spec],
      opaques: List[Ast.TypeId],
  )

  val nativeOpaques: Set[Ast.TypeId] =
    Set(
      Ast.TypeId(Ast.LocalName("any"), 0),
      Ast.TypeId(Ast.LocalName("atom"), 0),
      Ast.TypeId(Ast.LocalName("binary"), 0),
      Ast.TypeId(Ast.LocalName("bitstring"), 0),
      Ast.TypeId(Ast.LocalName("byte"), 0),
      Ast.TypeId(Ast.LocalName("exception"), 0),
      Ast.TypeId(Ast.LocalName("identifier"), 0),
      Ast.TypeId(Ast.LocalName("iodata"), 0),
      Ast.TypeId(Ast.LocalName("iolist"), 0),
      Ast.TypeId(Ast.LocalName("map"), 2),
      Ast.TypeId(Ast.LocalName("message"), 0),
      Ast.TypeId(Ast.LocalName("none"), 0),
      Ast.TypeId(Ast.LocalName("number"), 0),
      Ast.TypeId(Ast.LocalName("pid"), 0),
      Ast.TypeId(Ast.LocalName("port"), 0),
      Ast.TypeId(Ast.LocalName("reference"), 0),
      Ast.TypeId(Ast.LocalName("term"), 0),
      Ast.TypeId(Ast.LocalName("timeout"), 0),
      Ast.TypeId(Ast.LocalName("node"), 0),
      Ast.TypeId(Ast.LocalName("no_return"), 0),
      Ast.TypeId(Ast.LocalName("char"), 0),
      Ast.TypeId(Ast.LocalName("boolean"), 0),
      Ast.TypeId(Ast.LocalName("list"), 1),
      Ast.TypeId(Ast.LocalName("string"), 0),
    )

  val nativeAliases: List[Ast.TypeAlias] =
    List(
      Ast.TypeAlias("float", List.empty, Ast.UserType(Ast.LocalName("number"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("integer", List.empty, Ast.UserType(Ast.LocalName("number"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("neg_integer", List.empty, Ast.UserType(Ast.LocalName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("non_neg_integer", List.empty, Ast.UserType(Ast.LocalName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("pos_integer", List.empty, Ast.UserType(Ast.LocalName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("node", List.empty, Ast.UserType(Ast.LocalName("atom"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("no_return", List.empty, Ast.UserType(Ast.LocalName("none"), List.empty)(Doc.ZRange))(Doc.ZRange),
    )
}
