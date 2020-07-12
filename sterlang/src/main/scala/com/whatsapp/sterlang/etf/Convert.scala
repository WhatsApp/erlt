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
        val typeParams = params.map { case Types.TypeVariable(p, n) => Ast.TypeVar(n)(p) }
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
      case Forms.FunctionSpec(specP, Forms.Spec, (name, arity), types) =>
        types match {
          case List(Types.FunctionType(p, params, res)) =>
            val funType = Ast.FunType(params.map(convertType), convertType(res))(p)
            val funName = new Ast.LocalFunName(name, arity)
            val spec = Ast.Spec(funName, funType)(specP)
            Some(Ast.SpecElem(spec))
          case other =>
            sys.error(s"Unexpected spec: $form")
        }
      case Forms.FunctionDecl(p, name, arity, clauses) =>
        val funName = new Ast.LocalFunName(name, arity)
        val fun = Ast.Fun(funName, clauses.map(convertFunClause))(p)
        Some(Ast.FunElem(fun))
      case Forms.Behaviour(_) | Forms.Compile(_) | Forms.EOF | Forms.File(_) | Forms.RecordDecl(_, _) |
          Forms.FunctionSpec(_, Forms.Callback, _, _) =>
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
      case Guards.GVariable(p, name) =>
        Ast.VarExp(new Ast.LocalVarName(name))(p)
      case Guards.GTuple(p, elems) =>
        Ast.TupleExp(elems.map(convertGExpr))(p)
      case Guards.GNil(p) =>
        Ast.ListExp(List())(p)
      case Guards.GCons(p, hd, tl) =>
        Ast.ConsExp(convertGExpr(hd), convertGExpr(tl))(p)
      case Guards.GMapCreate(p, entries) =>
        Ast.RecordExp(entries.map(gAssocCreateToFieldExp))(p)
      case Guards.GMapUpdate(p, exp, entries) =>
        // this is not present in Erlang. Approximating
        val recExp = convertGExpr(exp)
        val updateRange = Pos.SP(recExp.p.asInstanceOf[Pos.SP].end, p.end)
        Ast.RecordUpdateExp(recExp, Ast.RecordExp(entries.map(gAssocUpdateToFieldExp))(updateRange))(p)
      case Guards.GCall(p, (fp, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.RemoteFunName("erlang", f, args.length))(fp), args.map(convertGExpr))(p)
      case Guards.GBinaryOp(p, ".", exp, Guards.GLiteral(Exprs.AtomLiteral(_, field))) =>
        Ast.SelExp(convertGExpr(exp), field)(p)
      case Guards.GBinaryOp(p, op, exp1, exp2) =>
        Ast.binOps.get(op) match {
          case Some(binOp) => Ast.BinOpExp(binOp, convertGExpr(exp1), convertGExpr(exp2))(p)
          case None        => sys.error(s"not supported binOp ($op) in: $gExpr")
        }
      case Guards.GUnaryOp(p, op, exp1) =>
        Ast.unOps.get(op) match {
          case Some(uOp) => Ast.UOpExp(uOp, convertGExpr(exp1))(p)
          case None      => sys.error(s"not supported unOp ($op) in: $gExpr")
        }
      case Guards.GLocalEnumCtr(p, enum, ctr, args) =>
        Ast.EnumConExp(Ast.LocalName(enum), ctr, args.map(convertGExpr))(p)
      case Guards.GRemoteEnumCtr(p, module, enum, ctr, args) =>
        Ast.EnumConExp(Ast.RemoteName(module, enum), ctr, args.map(convertGExpr))(p)
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
      case Patterns.MatchPattern(p, p1, p2) =>
        Ast.AndPat(convertPattern(p1), convertPattern(p2))(p)
      case Patterns.VariablePattern(p, "_") =>
        Ast.WildPat()(p)
      case Patterns.VariablePattern(p, name) =>
        Ast.VarPat(name)(p)
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
      case Patterns.TuplePattern(p, elems) =>
        Ast.TuplePat(elems.map(convertPattern))(p)
      case Patterns.NilPattern(p) =>
        Ast.ListPat(List())(p)
      case Patterns.ConsPattern(p, hd, tl) =>
        Ast.ConsPat(convertPattern(hd), convertPattern(tl))(p)
      case Patterns.LocalEnumCtrPattern(p, enum, ctr, args) =>
        Ast.EnumCtrPat(Ast.LocalName(enum), ctr, args.map(convertPattern))(p)
      case Patterns.RemoteEnumCtrPattern(p, module, enum, ctr, args) =>
        Ast.EnumCtrPat(Ast.RemoteName(module, enum), ctr, args.map(convertPattern))(p)
      case Patterns.MapPattern(p, assocs) =>
        val pats = assocs map {
          case (Patterns.LiteralPattern(Exprs.AtomLiteral(_, label)), pat) =>
            Ast.Field[Ast.Pat](label, convertPattern(pat))
          case other =>
            sys.error(s"wrong pattern assoc: $other")
        }
        Ast.RecordPat(pats, false)(p)
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
        val recExp = convertExpr(exp)
        // this is not present in Erlang. Approximating
        val updateRange = Pos.SP(recExp.p.asInstanceOf[Pos.SP].end, p.end)
        Ast.RecordUpdateExp(recExp, Ast.RecordExp(entries.map(assocUpdateToFieldExp))(updateRange))(p)
      case Exprs.Block(p, exprs) =>
        Ast.BlockExpr(convertBody(exprs))(p)
      case Exprs.Case(p, expr, clauses) =>
        Ast.CaseExp(convertExpr(expr), clauses.map(convertCaseClause))(p)
      case Exprs.LocalCall(p1, Exprs.AtomLiteral(p2, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.LocalFunName(f, args.length))(p2), args.map(convertExpr))(p1)
      case Exprs.LocalCall(p, head, args) =>
        Ast.AppExp(convertExpr(head), args.map(convertExpr))(p)
      case Exprs.RemoteCall(p1, Exprs.AtomLiteral(p2, m), Exprs.AtomLiteral(p3, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.RemoteFunName(m, f, args.length))(p2 ! p3), args.map(convertExpr))(p1)
      case Exprs.RemoteCall(_, _, _, _) =>
        sys.error(s"not supported remote call: $e")
      case Exprs.LocalFun(p, f, arity) =>
        Ast.VarExp(new Ast.LocalFunName(f, arity))(p)
      case Exprs.RemoteFun(p, Exprs.AtomLiteral(_, m), Exprs.AtomLiteral(_, f), Exprs.IntLiteral(_, arity)) =>
        Ast.VarExp(new Ast.RemoteFunName(m, f, arity))(p)
      case Exprs.RemoteFun(_, _, _, _) =>
        sys.error(s"not supported: $e")
      case Exprs.Fun(p, clauses) =>
        Ast.FnExp(clauses.map(convertFunClause))(p)
      case Exprs.NamedFun(p, funName, clauses) =>
        Ast.NamedFnExp(new Ast.LocalVarName(funName), clauses.map(convertFunClause))(p)
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
      case Types.BitstringType(p, List()) =>
        Ast.UserType(Ast.LocalName("binary"), List())(p)
      case Types.BitstringType(_, _) =>
        sys.error(s"Not supported (yet) binary type: $tp")
      case Types.FunctionType(p, args, result) =>
        Ast.FunType(args.map(convertType), convertType(result))(p)
      case Types.AssocMap(p, assocs) =>
        assocs match {
          case List() =>
            Ast.RecordType(List())(p)
          case List(Types.MapFieldOpt(List(k, v))) =>
            Ast.UserType(Ast.LocalName("map"), List(convertType(k), convertType(v)))(p)
          case _ =>
            assocs.last match {
              case Types.MapFieldExact(List(Types.TypeVariable(p1, "_"), Types.TypeVariable(p2, "_"))) =>
                Ast.OpenRecordType(assocs.init.map(convertAssoc), Ast.WildTypeVar()(p1 ! p2))(p)
              case _ =>
                Ast.RecordType(assocs.map(convertAssoc))(p)
            }
        }
      case Types.PredefinedType(p, "list", List(elemType)) =>
        Ast.ListType(convertType(elemType))(p)
      case Types.PredefinedType(p, name, params) =>
        Ast.UserType(Ast.LocalName(name), params.map(convertType))(p)
      case Types.RemoteType(p, module, typeName, params) =>
        Ast.UserType(Ast.RemoteName(module, typeName), params.map(convertType))(p)
      case Types.TupleTypeTyped(p, elems) =>
        Ast.TupleType(elems.map(convertType))(p)
      case Types.TypeVariable(p, "_") =>
        Ast.WildTypeVar()(p)
      case Types.TypeVariable(p, v) =>
        Ast.TypeVar(v)(p)
      case Types.UserType(p, name, params) =>
        Ast.UserType(Ast.LocalName(name), params.map(convertType))(p)
      case Types.AtomType(_) | Types.EmptyListType | Types.EnumCtr(_, _, _) | Types.FunTypeAny |
          Types.FunTypeAnyArgs(_) | Types.IntegerRangeType(_, _) | Types.AnyMap | Types.RecordType(_, _) |
          Types.TupleTypeAny(_) | Types.UnionType(_) | (_: Types.SingletonIntegerType) =>
        sys.error(s"erroneous type: $tp")
    }

  private def enumCon(tp: Types.Type): Ast.EnumCon =
    tp match {
      case Types.EnumCtr(p, name, params) =>
        Ast.EnumCon(name, params.map(convertType))(p)
      case other =>
        sys.error(s"Expected an enum ctr but got: $other")
    }

  private def convertAssoc(assoc: Types.AssocType): Ast.Field[Ast.Type] =
    assoc match {
      case Types.MapFieldExact(List(Types.AtomType(field), v)) =>
        Ast.Field(field, convertType(v))
      case other =>
        sys.error(s"unexpected assocs: $other")
    }
}
