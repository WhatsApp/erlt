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

package com.whatsapp.sterlang.etf

import com.whatsapp.sterlang.{Ast, Pos}
import com.whatsapp.sterlang.forms.Forms
import com.whatsapp.sterlang.forms.Types

object Convert {
  def convert(form: Forms.Form): Option[Ast.ProgramElem] =
    form match {
      case Forms.Lang(mods) =>
        Some(Ast.LangElem(mods))
      case Forms.Module(name) =>
        Some(Ast.ModuleElem(name))
      case Forms.Require(modules) =>
        Some(Ast.RequireElem(modules))
      case Forms.Export(ids) =>
        Some(Ast.ExportElem(ids))
      case Forms.Import(module, ids) =>
        Some(Ast.ImportElem(module, ids.map { case (name, arity) => new Ast.LocalFunName(name, arity) }))
      case Forms.ExportType(ids) =>
        Some(Ast.ExportTypeElem(ids))
      case Forms.ImportType(module, ids) =>
        Some(Ast.ImportTypeElem(module, ids.map { case (name, arity) => new Ast.LocalFunName(name, arity) }))
      case Forms.TypeDecl(typeAttr, typeName, params, body) =>
        val typeParams = params.map(Ast.TypeVar(_)(Pos.NP))
        typeAttr match {
          case Forms.Enum =>
            val enumCons =
              body match {
                case Types.UnionType(elems) =>
                  elems.map(enumCon)
                case single =>
                  List(enumCon(single))
              }
            val enumDef = Ast.EnumDef(typeName, typeParams, enumCons)(Pos.NP)
            Some(Ast.EnumElem(enumDef))
          case Forms.Type =>
            val typeAlias = Ast.TypeAlias(typeName, typeParams, convertType(body))(Pos.NP)
            Some(Ast.TypeAliasElem(typeAlias))
          case Forms.Opaque =>
            val opaque = Ast.Opaque(typeName, typeParams, convertType(body))(Pos.NP)
            Some(Ast.OpaqueElem(opaque))
        }
      case Forms.FunctionSpec(Forms.Spec, (name, arity), types) =>
        types match {
          case List(Types.FunctionType(params, res)) =>
            val funType = Ast.FunType(params.map(convertType), convertType(res))(Pos.NP)
            val funName = new Ast.LocalFunName(name, arity)
            val spec = Ast.Spec(funName, funType)(Pos.NP)
            Some(Ast.SpecElem(spec))
          case other =>
            sys.error(s"Unexpected spec: $form")
        }
      case Forms.FunctionDecl(name, arity, clauses) =>
        // TODO
        None
      case Forms.Behaviour(_) | Forms.Compile(_) | Forms.EOF | Forms.File(_) | Forms.RecordDecl(_, _) |
          Forms.FunctionSpec(Forms.Callback, _, _) =>
        None
    }

  private def convertType(tp: Types.Type): Ast.Type =
    tp match {
      case Types.AnnotatedType(_, tp) =>
        convertType(tp)
      case Types.BitstringType(List()) =>
        Ast.UserType(Ast.LocalName("binary"), List())(Pos.NP)
      case Types.BitstringType(_) =>
        sys.error(s"Not supported (yet) binary type: $tp")
      case Types.FunctionType(args, result) =>
        Ast.FunType(args.map(convertType), convertType(result))(Pos.NP)
      case Types.AssocMap(List()) =>
        sys.error(s"erroneous type: $tp")
      case Types.AssocMap(assocs) =>
        convertAssocs(assocs)
      case Types.PredefinedType("list", List(elemType)) =>
        Ast.ListType(convertType(elemType))(Pos.NP)
      case Types.PredefinedType(name, params) =>
        Ast.UserType(Ast.LocalName(name), params.map(convertType))(Pos.NP)
      case Types.RemoteType(module, typeName, params) =>
        Ast.UserType(Ast.RemoteName(module, typeName), params.map(convertType))(Pos.NP)
      case Types.TupleTypeTyped(elems) =>
        Ast.TupleType(elems.map(convertType))(Pos.NP)
      case Types.TypeVariable("_") =>
        Ast.WildTypeVar()(Pos.NP)
      case Types.TypeVariable(v) =>
        Ast.TypeVar(v)(Pos.NP)
      case Types.UserType(name, params) =>
        Ast.UserType(Ast.LocalName(name), params.map(convertType))(Pos.NP)
      case Types.AtomType(_) | Types.EmptyListType | Types.EnumCtr(_, _) | Types.FunTypeAny | Types.FunTypeAnyArgs(_) |
          Types.IntegerRangeType(_, _) | Types.AnyMap | Types.RecordType(_, _) | Types.TupleTypeAny |
          Types.UnionType(_) | (_: Types.SingletonIntegerType) =>
        sys.error(s"erroneous type: $tp")
    }

  private def enumCon(tp: Types.Type): Ast.EnumCon =
    tp match {
      case Types.EnumCtr(name, params) =>
        Ast.EnumCon(name, params.map(convertType))(Pos.NP)
      case other =>
        sys.error(s"Expected an enum ctr but got: $other")
    }

  private def convertAssocs(assocs: List[Types.AssocType]): Ast.Type =
    assocs match {
      case List() =>
        sys.error(s"unexpected assocs: $assocs")
      case List(Types.MapFieldOpt(List(k, v))) =>
        Ast.UserType(Ast.LocalName("map"), List(convertType(k), convertType(v)))(Pos.NP)
      case _ =>
        assocs.last match {
          case Types.MapFieldExact(List(Types.TypeVariable("_"), Types.TypeVariable("_"))) =>
            Ast.OpenRecordType(assocs.init.map(convertAssoc), Ast.WildTypeVar()(Pos.NP))(Pos.NP)
          case _ =>
            Ast.RecordType(assocs.map(convertAssoc))(Pos.NP)
        }
    }

  private def convertAssoc(assoc: Types.AssocType): Ast.Field[Ast.Type] =
    assoc match {
      case Types.MapFieldExact(List(Types.AtomType(field), v)) =>
        Ast.Field(field, convertType(v))
      case other =>
        sys.error(s"unexpected assocs: $other")
    }
}
