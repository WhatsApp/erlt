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
          Ast.TypeId(Ast.UName(name))
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
      Ast.TypeId(Ast.UName("any")),
      Ast.TypeId(Ast.UName("atom")),
      Ast.TypeId(Ast.UName("binary")),
      Ast.TypeId(Ast.UName("bitstring")),
      Ast.TypeId(Ast.UName("byte")),
      Ast.TypeId(Ast.UName("exception")),
      Ast.TypeId(Ast.UName("identifier")),
      Ast.TypeId(Ast.UName("iodata")),
      Ast.TypeId(Ast.UName("iolist")),
      Ast.TypeId(Ast.UName("message")),
      Ast.TypeId(Ast.UName("none")),
      Ast.TypeId(Ast.UName("number")),
      Ast.TypeId(Ast.UName("pid")),
      Ast.TypeId(Ast.UName("port")),
      Ast.TypeId(Ast.UName("reference")),
      Ast.TypeId(Ast.UName("term")),
      Ast.TypeId(Ast.UName("timeout")),
      Ast.TypeId(Ast.UName("char")),
      Ast.TypeId(Ast.UName("boolean")),
      Ast.TypeId(Ast.UName("string")),
    )

  val nativeAliases: List[Ast.TypeAlias] =
    List(
      Ast.TypeAlias("float", List.empty, Ast.UserType(Ast.UName("number"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("integer", List.empty, Ast.UserType(Ast.UName("number"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("neg_integer", List.empty, Ast.UserType(Ast.UName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("non_neg_integer", List.empty, Ast.UserType(Ast.UName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("pos_integer", List.empty, Ast.UserType(Ast.UName("number"), List.empty)(Doc.ZRange))(
        Doc.ZRange
      ),
      Ast.TypeAlias("node", List.empty, Ast.UserType(Ast.UName("atom"), List.empty)(Doc.ZRange))(Doc.ZRange),
      Ast.TypeAlias("no_return", List.empty, Ast.UserType(Ast.UName("none"), List.empty)(Doc.ZRange))(Doc.ZRange),
    )
}
