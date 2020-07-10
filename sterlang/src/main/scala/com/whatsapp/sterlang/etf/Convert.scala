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
import com.whatsapp.sterlang.forms.{Forms, Exprs, Types, Patterns}

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
        val funName = new Ast.LocalFunName(name, arity)
        val fun = Ast.Fun(funName, clauses.map(convertClause))(Pos.NP)
        Some(Ast.FunElem(fun))
      case Forms.Behaviour(_) | Forms.Compile(_) | Forms.EOF | Forms.File(_) | Forms.RecordDecl(_, _) |
          Forms.FunctionSpec(Forms.Callback, _, _) =>
        None
    }

  private def convertClause(clause: Exprs.Clause): Ast.Clause =
    Ast.Clause(clause.pats.map(convertPattern), List(), convertBody(clause.body))

  private def convertBody(exprs: List[Exprs.Expr]): Ast.Body =
    Ast.Body(exprs.init.map(convertValDef), convertValDef(exprs.last))

  private def convertValDef(expr: Exprs.Expr): Ast.ValDef =
    expr match {
      case Exprs.Match(p, e) =>
        Ast.ValDef(convertPattern(p), convertExpr(e))
      case e =>
        Ast.ValDef(Ast.WildPat()(Pos.NP), convertExpr(e))
    }

  private def convertPattern(p: Patterns.Pattern): Ast.Pat =
    p match {
      case Patterns.VariablePattern("_") =>
        Ast.WildPat()(Pos.NP)
      case Patterns.VariablePattern(name) =>
        Ast.VarPat(name)(Pos.NP)
      case Patterns.LiteralPattern(literal) =>
        literal match {
          case Exprs.AtomLiteral("true") =>
            Ast.BoolPat(true)(Pos.NP)
          case Exprs.AtomLiteral("false") =>
            Ast.BoolPat(false)(Pos.NP)
          case Exprs.AtomLiteral(_) =>
            sys.error(s"atoms are not supported in ST: $p")
          case Exprs.CharLiteral(ch) =>
            sys.error("chars in patterns are not supported yet")
          case Exprs.FloatLiteral(fl) =>
            Ast.NumberPat(fl.intValue())(Pos.NP)
          case Exprs.IntLiteral(i) =>
            Ast.NumberPat(i)(Pos.NP)
          case Exprs.StringLiteral(str) =>
            Ast.StringPat(str.getOrElse("???"))(Pos.NP)
        }
      case Patterns.MatchPattern(pat, arg) =>
        ???
      case Patterns.TuplePattern(elems) =>
        ???
      case Patterns.NilPattern =>
        ???
      case Patterns.ConsPattern(hd, tl) =>
        ???
      case Patterns.BinPattern(elems) =>
        ???
      case Patterns.BinOpPattern(op, pat1, pat2) =>
        ???
      case Patterns.UnOpPattern(op, pat1) =>
        ???
      case Patterns.RecordPattern(recordName, fields) =>
        ???
      case Patterns.RecordIndexPattern(recordName, fieldName) =>
        ???
      case Patterns.MapPattern(assocs) =>
        ???
      case Patterns.LocalEnumCtrPattern(enum, ctr, args) =>
        ???
      case Patterns.RemoteEnumCtrPattern(module, enum, ctr, args) =>
        ???
    }

  private def convertExpr(e: Exprs.Expr): Ast.Exp =
    e match {
      case Exprs.Variable(name) =>
        Ast.VarExp(new Ast.LocalVarName(name))(Pos.NP)
      case literal: Exprs.Literal =>
        literal match {
          case Exprs.AtomLiteral("true") =>
            Ast.BoolExp(true)(Pos.NP)
          case Exprs.AtomLiteral("false") =>
            Ast.BoolExp(false)(Pos.NP)
          case Exprs.AtomLiteral(_) =>
            sys.error(s"atoms are not supported in ST: $e")
          case Exprs.CharLiteral(ch) =>
            Ast.CharExp(ch.toString)(Pos.NP)
          case Exprs.FloatLiteral(fl) =>
            Ast.NumberExp(fl.intValue())(Pos.NP)
          case Exprs.IntLiteral(i) =>
            Ast.NumberExp(i)(Pos.NP)
          case Exprs.StringLiteral(Some(str)) =>
            Ast.StringExp(str)(Pos.NP)
          case Exprs.StringLiteral(None) =>
            Ast.StringExp("???")(Pos.NP)
        }
      case Exprs.Tuple(elems) =>
        ???
      case Exprs.Match(pat, arg) =>
        ???
      case Exprs.Nil =>
        ???
      case Exprs.Cons(hd, tl) =>
        ???
      case Exprs.Bin(elems) =>
        ???
      case Exprs.BinaryOp(op, exp1, exp2) =>
        ???
      case Exprs.UnaryOp(op, exp1) =>
        ???
      case Exprs.RecordCreate(recordName, fields) =>
        ???
      case Exprs.RecordUpdate(exp1, recordName, fields) =>
        ???
      case Exprs.RecordIndex(recordName, fieldName) =>
        ???
      case Exprs.RecordFieldAccess(exp, recordName, fieldName) =>
        ???
      case Exprs.MapCreate(entries) =>
        ???
      case Exprs.MapUpdate(exp, entries) =>
        ???
      case Exprs.Catch(exp) =>
        ???
      case Exprs.LocalCall(fun, args) =>
        ???
      case Exprs.RemoteCall(module, fun, args) =>
        ???
      case Exprs.LocalEnumCtr(enum, ctr, args) =>
        ???
      case Exprs.RemoteEnumCtr(module, enum, ctr, args) =>
        ???
      case Exprs.ListComprehension(template, qualifiers) =>
        ???
      case Exprs.BinaryComprehension(template, qualifiers) =>
        ???
      case Exprs.Block(exprs) =>
        ???
      case Exprs.If(clauses) =>
        ???
      case Exprs.Case(expr, clauses) =>
        ???
      case Exprs.Try(body, clauses, catchClauses, after) =>
        ???
      case Exprs.Receive(clauses) =>
        ???
      case Exprs.ReceiveWithTimeout(cl, timeout, default) =>
        ???
      case Exprs.LocalFun(funName, arity) =>
        ???
      case Exprs.RemoteFun(module, funName, arity) =>
        ???
      case Exprs.RemoteFunDynamic(module, funName, arity) =>
        ???
      case Exprs.Fun(clauses) =>
        ???
      case Exprs.NamedFun(funName, clauses) =>
        ???
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
