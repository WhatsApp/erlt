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
  type Env = Map[String, STypes.TypeScheme]

  case class Context(
      enumDefs: List[Ast.EnumDef],
      structDefs: List[Ast.StructDef],
      aliases: List[Ast.TypeAlias],
      opaques: Set[Ast.TypeId],
      env: Env,
  ) {
    def extend(program: Ast.Program): Context = {
      val opaqueAliases = program.opaques.map { opaque =>
        Ast.TypeAlias(opaque.name, opaque.params, opaque.body)(opaque.r)
      }
      val localOpaques = program.uncheckedOpaques.map {
        case Ast.UncheckedOpaque(name, _) =>
          Ast.TypeId(Ast.LocalName(name))
      }
      Context(
        enumDefs ++ program.enumDefs,
        structDefs ++ program.structDefs,
        aliases ++ program.typeAliases ++ opaqueAliases,
        opaques ++ localOpaques,
        env,
      )
    }
  }

  case class ModuleApi(
      enumDefs: List[Ast.EnumDef],
      structDefs: List[Ast.StructDef],
      aliases: List[Ast.TypeAlias],
      specs: List[Ast.Spec],
      // includes both:
      // -opaque
      // -export_type + [opaque unchecked]
      opaques: List[Ast.TypeId],
  )

  val nativeOpaques: Set[Ast.TypeId] =
    Set(
      Ast.TypeId(Ast.LocalName("any")),
      Ast.TypeId(Ast.LocalName("atom")),
      Ast.TypeId(Ast.LocalName("binary")),
      Ast.TypeId(Ast.LocalName("bitstring")),
      Ast.TypeId(Ast.LocalName("byte")),
      Ast.TypeId(Ast.LocalName("exception")),
      Ast.TypeId(Ast.LocalName("identifier")),
      Ast.TypeId(Ast.LocalName("iodata")),
      Ast.TypeId(Ast.LocalName("iolist")),
      Ast.TypeId(Ast.LocalName("message")),
      Ast.TypeId(Ast.LocalName("none")),
      Ast.TypeId(Ast.LocalName("number")),
      Ast.TypeId(Ast.LocalName("pid")),
      Ast.TypeId(Ast.LocalName("port")),
      Ast.TypeId(Ast.LocalName("reference")),
      Ast.TypeId(Ast.LocalName("term")),
      Ast.TypeId(Ast.LocalName("timeout")),
      Ast.TypeId(Ast.LocalName("char")),
      Ast.TypeId(Ast.LocalName("boolean")),
      Ast.TypeId(Ast.LocalName("string")),
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
