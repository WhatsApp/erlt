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
import com.whatsapp.sterlang.forms.{Exprs, Forms, Guards, Patterns, Types}

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
        val fun = Ast.Fun(funName, clauses.map(convertFunClause))(Pos.NP)
        Some(Ast.FunElem(fun))
      case Forms.Behaviour(_) | Forms.Compile(_) | Forms.EOF | Forms.File(_) | Forms.RecordDecl(_, _) |
          Forms.FunctionSpec(Forms.Callback, _, _) =>
        None
    }

  private def convertFunClause(clause: Exprs.Clause): Ast.Clause =
    Ast.Clause(clause.pats.map(convertPattern), clause.guards.map(convertGuard), convertBody(clause.body))

  private def convertCaseClause(clause: Exprs.Clause): Ast.Rule =
    clause.pats match {
      case List(pat) => Ast.Rule(convertPattern(pat), clause.guards.map(convertGuard), convertBody(clause.body))
      case _         => sys.error(s"unexpected clause: $clause")
    }

  private def convertGuard(guard: Guards.Guard): Ast.Guard =
    Ast.Guard(guard.elems.map(convertGExpr))

  private def convertGExpr(gExpr: Guards.GExpr): Ast.Exp =
    gExpr match {
      case Guards.GLiteral(literal) =>
        literal match {
          case Exprs.AtomLiteral(p, "true") =>
            Ast.BoolExp(true)(p)
          case Exprs.AtomLiteral(p, "false") =>
            Ast.BoolExp(false)(p)
          case Exprs.AtomLiteral(_, _) =>
            sys.error(s"atoms are not supported in ST: $gExpr")
          case Exprs.CharLiteral(p, ch) =>
            Ast.CharExp(ch.toString)(p)
          case Exprs.FloatLiteral(p, fl) =>
            Ast.NumberExp(fl.intValue())(p)
          case Exprs.IntLiteral(p, i) =>
            Ast.NumberExp(i)(p)
          case Exprs.StringLiteral(p, Some(str)) =>
            Ast.StringExp(str)(p)
          case Exprs.StringLiteral(p, None) =>
            Ast.StringExp("???")(p)
        }
      case Guards.GVariable(name) =>
        Ast.VarExp(new Ast.LocalVarName(name))(Pos.NP)
      case Guards.GTuple(elems) =>
        Ast.TupleExp(elems.map(convertGExpr))(Pos.NP)
      case Guards.GNil =>
        Ast.ListExp(List())(Pos.NP)
      case Guards.GCons(hd, tl) =>
        Ast.ConsExp(convertGExpr(hd), convertGExpr(tl))(Pos.NP)
      case Guards.GMapCreate(entries) =>
        Ast.RecordExp(entries.map(gAssocCreateToFieldExp))(Pos.NP)
      case Guards.GMapUpdate(exp, entries) =>
        Ast.RecordUpdateExp(convertGExpr(exp), Ast.RecordExp(entries.map(gAssocUpdateToFieldExp))(Pos.NP))(Pos.NP)
      case Guards.GCall(f, args) =>
        Ast.AppExp(Ast.VarExp(new Ast.RemoteFunName("erlang", f, args.length))(Pos.NP), args.map(convertGExpr))(Pos.NP)
      case Guards.GBinaryOp(".", exp, Guards.GLiteral(Exprs.AtomLiteral(_, field))) =>
        Ast.SelExp(convertGExpr(exp), field)(Pos.NP)
      case Guards.GBinaryOp(op, exp1, exp2) =>
        Ast.binOps.get(op) match {
          case Some(binOp) => Ast.BinOpExp(binOp, convertGExpr(exp1), convertGExpr(exp2))(Pos.NP)
          case None        => sys.error(s"not supported binOp ($op) in: $gExpr")
        }
      case Guards.GUnaryOp(op, exp1) =>
        Ast.unOps.get(op) match {
          case Some(uOp) => Ast.UOpExp(uOp, convertGExpr(exp1))(Pos.NP)
          case None      => sys.error(s"not supported unOp ($op) in: $gExpr")
        }
      case Guards.GLocalEnumCtr(enum, ctr, args) =>
        Ast.EnumConExp(Ast.LocalName(enum), ctr, args.map(convertGExpr))(Pos.NP)
      case Guards.GRemoteEnumCtr(module, enum, ctr, args) =>
        Ast.EnumConExp(Ast.RemoteName(module, enum), ctr, args.map(convertGExpr))(Pos.NP)
      case Guards.GBin(elems) =>
        ???
      case Guards.GRecordCreate(recordName, fields) =>
        ???
      case Guards.GRecordIndex(recordName, fieldName) =>
        ???
      case Guards.GRecordFieldAccess(test, recordName, fieldName) =>
        ???
    }

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
      case Patterns.MatchPattern(p1, p2) =>
        Ast.AndPat(convertPattern(p1), convertPattern(p2))(Pos.NP)
      case Patterns.VariablePattern("_") =>
        Ast.WildPat()(Pos.NP)
      case Patterns.VariablePattern(name) =>
        Ast.VarPat(name)(Pos.NP)
      case Patterns.LiteralPattern(literal) =>
        literal match {
          case Exprs.AtomLiteral(p, "true") =>
            Ast.BoolPat(true)(p)
          case Exprs.AtomLiteral(p, "false") =>
            Ast.BoolPat(false)(p)
          case Exprs.AtomLiteral(_, _) =>
            sys.error(s"atoms are not supported in ST: $p")
          case Exprs.CharLiteral(_, ch) =>
            sys.error("chars in patterns are not supported yet")
          case Exprs.FloatLiteral(p, fl) =>
            Ast.NumberPat(fl.intValue())(p)
          case Exprs.IntLiteral(p, i) =>
            Ast.NumberPat(i)(p)
          case Exprs.StringLiteral(p, str) =>
            Ast.StringPat(str.getOrElse("???"))(p)
        }
      case Patterns.TuplePattern(elems) =>
        Ast.TuplePat(elems.map(convertPattern))(Pos.NP)
      case Patterns.NilPattern =>
        Ast.ListPat(List())(Pos.NP)
      case Patterns.ConsPattern(hd, tl) =>
        Ast.ConsPat(convertPattern(hd), convertPattern(tl))(Pos.NP)
      case Patterns.LocalEnumCtrPattern(enum, ctr, args) =>
        Ast.EnumCtrPat(Ast.LocalName(enum), ctr, args.map(convertPattern))(Pos.NP)
      case Patterns.RemoteEnumCtrPattern(module, enum, ctr, args) =>
        Ast.EnumCtrPat(Ast.RemoteName(module, enum), ctr, args.map(convertPattern))(Pos.NP)
      case Patterns.MapPattern(assocs) =>
        val pats = assocs map {
          case (Patterns.LiteralPattern(Exprs.AtomLiteral(_, label)), pat) =>
            Ast.Field[Ast.Pat](label, convertPattern(pat))
          case other =>
            sys.error(s"wrong pattern assoc: $other")
        }
        Ast.RecordPat(pats, false)(Pos.NP)
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
    }

  private def convertExpr(e: Exprs.Expr): Ast.Exp =
    e match {
      case Exprs.Match(_, _) =>
        sys.error(s"such matches are not supported: $e")
      case Exprs.Variable(p, name) =>
        Ast.VarExp(new Ast.LocalVarName(name))(p)
      case literal: Exprs.Literal =>
        literal match {
          case Exprs.AtomLiteral(p, "true") =>
            Ast.BoolExp(true)(p)
          case Exprs.AtomLiteral(p, "false") =>
            Ast.BoolExp(false)(p)
          case Exprs.AtomLiteral(_, _) =>
            sys.error(s"atoms are not supported in ST: $e")
          case Exprs.CharLiteral(p, ch) =>
            Ast.CharExp(ch.toString)(p)
          case Exprs.FloatLiteral(p, fl) =>
            Ast.NumberExp(fl.intValue())(p)
          case Exprs.IntLiteral(p, i) =>
            Ast.NumberExp(i)(p)
          case Exprs.StringLiteral(p, Some(str)) =>
            Ast.StringExp(str)(p)
          case Exprs.StringLiteral(p, None) =>
            Ast.StringExp("???")(p)
        }
      case Exprs.Tuple(p, elems) =>
        Ast.TupleExp(elems.map(convertExpr))(p)
      case Exprs.Nil(p) =>
        Ast.ListExp(List())(p)
      case Exprs.Cons(p, hd, tl) =>
        Ast.ConsExp(convertExpr(hd), convertExpr(tl))(p)
      case Exprs.LocalEnumCtr(p, enum, ctr, args) =>
        Ast.EnumConExp(Ast.LocalName(enum), ctr, args.map(convertExpr))(p)
      case Exprs.RemoteEnumCtr(p, module, enum, ctr, args) =>
        Ast.EnumConExp(Ast.RemoteName(module, enum), ctr, args.map(convertExpr))(p)
      case Exprs.MapCreate(p, entries) =>
        Ast.RecordExp(entries.map(assocCreateToFieldExp))(p)
      case Exprs.MapUpdate(p, exp, entries) =>
        Ast.RecordUpdateExp(convertExpr(exp), Ast.RecordExp(entries.map(assocUpdateToFieldExp))(Pos.NP))(p)
      case Exprs.Block(p, exprs) =>
        Ast.BlockExpr(convertBody(exprs))(p)
      case Exprs.Case(expr, clauses) =>
        Ast.CaseExp(convertExpr(expr), clauses.map(convertCaseClause))(Pos.NP)
      case Exprs.LocalCall(p1, Exprs.AtomLiteral(p2, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.LocalFunName(f, args.length))(p2), args.map(convertExpr))(p1)
      case Exprs.LocalCall(p, head, args) =>
        Ast.AppExp(convertExpr(head), args.map(convertExpr))(p)
      case Exprs.RemoteCall(p1, Exprs.AtomLiteral(p2, m), Exprs.AtomLiteral(p3, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.RemoteFunName(m, f, args.length))(p2 ! p3), args.map(convertExpr))(p1)
      case Exprs.RemoteCall(_, _, _, _) =>
        sys.error(s"not supported remote call: $e")
      case Exprs.LocalFun(f, arity) =>
        Ast.VarExp(new Ast.LocalFunName(f, arity))(Pos.NP)
      case Exprs.RemoteFun(Exprs.AtomLiteral(p1, m), Exprs.AtomLiteral(_, f), Exprs.IntLiteral(p3, arity)) =>
        Ast.VarExp(new Ast.RemoteFunName(m, f, arity))(p1 ! p3)
      case Exprs.RemoteFun(_, _, _) =>
        sys.error(s"not supported: $e")
      case Exprs.Fun(clauses) =>
        Ast.FnExp(clauses.map(convertFunClause))(Pos.NP)
      case Exprs.NamedFun(funName, clauses) =>
        Ast.NamedFnExp(new Ast.LocalVarName(funName), clauses.map(convertFunClause))(Pos.NP)
      case Exprs.UnaryOp(p, op, exp1) =>
        Ast.unOps.get(op) match {
          case Some(uOp) => Ast.UOpExp(uOp, convertExpr(exp1))(p)
          case None      => sys.error(s"not supported unOp ($op) in: $e")
        }
      case Exprs.BinaryOp(p, ".", exp, Exprs.AtomLiteral(_, field)) =>
        Ast.SelExp(convertExpr(exp), field)(p)
      case Exprs.BinaryOp(p, op, exp1, exp2) =>
        Ast.binOps.get(op) match {
          case Some(binOp) => Ast.BinOpExp(binOp, convertExpr(exp1), convertExpr(exp2))(p)
          case None        => sys.error(s"not supported binOp ($op) in: $e")
        }
      case Exprs.ListComprehension(template, qualifiers) =>
        ???
      case Exprs.Bin(elems) =>
        ???
      case Exprs.RecordCreate(recordName, fields) =>
        ???
      case Exprs.RecordUpdate(exp1, recordName, fields) =>
        ???
      case Exprs.RecordIndex(recordName, fieldName) =>
        ???
      case Exprs.RecordFieldAccess(exp, recordName, fieldName) =>
        ???
      case Exprs.Catch(exp) =>
        ???
      case Exprs.BinaryComprehension(template, qualifiers) =>
        ???
      case Exprs.If(clauses) =>
        ???
      case Exprs.Try(body, clauses, catchClauses, after) =>
        ???
      case Exprs.Receive(clauses) =>
        ???
      case Exprs.ReceiveWithTimeout(cl, timeout, default) =>
        ???
    }

  private def assocCreateToFieldExp(assoc: Exprs.Assoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Exprs.OptAssoc(Exprs.AtomLiteral(_, label), e) =>
        Ast.Field(label, convertExpr(e))
      case other =>
        sys.error(s"incorrect assoc: $other")
    }

  private def assocUpdateToFieldExp(assoc: Exprs.Assoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Exprs.AssocExact(Exprs.AtomLiteral(_, label), e) =>
        Ast.Field(label, convertExpr(e))
      case other =>
        sys.error(s"incorrect assoc: $other")
    }

  private def gAssocCreateToFieldExp(assoc: Guards.GAssoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Guards.GAssocOpt(Guards.GLiteral(Exprs.AtomLiteral(_, label)), e) =>
        Ast.Field(label, convertGExpr(e))
      case other =>
        sys.error(s"incorrect assoc: $other")
    }

  private def gAssocUpdateToFieldExp(assoc: Guards.GAssoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Guards.GAssocExact(Guards.GLiteral(Exprs.AtomLiteral(_, label)), e) =>
        Ast.Field(label, convertGExpr(e))
      case other =>
        sys.error(s"incorrect assoc: $other")
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
