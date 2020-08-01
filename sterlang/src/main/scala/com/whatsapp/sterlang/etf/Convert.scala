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

import com.whatsapp.sterlang.{Ast, ParseError, Pos, UnsupportedSyntaxError}
import com.whatsapp.sterlang.forms.{Exprs, Forms, Guards, Patterns, Types}

object Convert {
  private def typeSpecifiersMapping: Map[String, Ast.BinElemType] =
    Map(
      "integer" -> Ast.IntegerBinElemType,
      "float" -> Ast.FloatBinElemType,
      "binary" -> Ast.BinaryBinElemType,
      "bytes" -> Ast.BytesBinElemType,
      "bitstring" -> Ast.BitstringBinElemType,
      "bits" -> Ast.BitsBinElemType,
      "utf8" -> Ast.Utf8BinElemType,
      "utf16" -> Ast.Utf16BinElemType,
      "utf32" -> Ast.Utf32BinElemType,
    )

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
      case Forms.TypeDecl(p, typeAttr, typeName, params, body) =>
        val typeParams = params.map { case Types.TypeVariable(p, n) => Ast.TypeVar(n)(p) }
        typeAttr match {
          case Forms.Enum =>
            val enumCons =
              body match {
                case Types.UnionType(_, elems) =>
                  elems.map(enumCon)
                case single =>
                  List(enumCon(single))
              }
            val enumDef = Ast.EnumDef(typeName, typeParams, enumCons)(p)
            Some(Ast.EnumElem(enumDef))
          case Forms.Type =>
            val typeAlias = Ast.TypeAlias(typeName, typeParams, convertType(body))(p)
            Some(Ast.TypeAliasElem(typeAlias))
          case Forms.Opaque =>
            val opaque = Ast.Opaque(typeName, typeParams, convertType(body))(p)
            Some(Ast.OpaqueElem(opaque))
        }
      case Forms.FunctionSpec(specP, Forms.Spec, (name, arity), types) =>
        types match {
          case List(Types.FunctionType(p, params, res)) =>
            val funType = Ast.FunType(params.map(convertType), convertType(res))(p)
            val funName = new Ast.LocalFunName(name, arity)
            val spec = Ast.Spec(funName, funType)(specP)
            Some(Ast.SpecElem(spec))
          case _ =>
            // intersection types are not supported,
            // when clauses in fun types are not supported
            throw new UnsupportedSyntaxError(specP)
        }
      case Forms.FunctionDecl(p, name, arity, clauses) =>
        val funName = new Ast.LocalFunName(name, arity)
        val fun = Ast.Fun(funName, clauses.map(convertFunClause))(p)
        Some(Ast.FunElem(fun))
      case Forms.RecordDecl(p, name, eFields, kind) =>
        val fields = eFields.map(convertRecordField)
        val eRecordType = Ast.ErlangRecordDef(name, fields, recordKind(kind))(p)
        Some(Ast.ErlangRecordElem(eRecordType))
      case Forms.Behaviour(_) | Forms.Compile(_) | Forms.EOF | Forms.File(_) |
          Forms.FunctionSpec(_, Forms.Callback, _, _) | Forms.WildAttribute(_, _) =>
        None
      case Forms.Error(loc) =>
        throw ParseError(loc)
    }

  private def recordKind(recKind: Forms.RecordKind): Ast.RecordKind =
    recKind match {
      case Forms.RecRecord => Ast.ErlangRecord
      case Forms.ExnRecord => Ast.ExceptionRecord
      case Forms.MsgRecord => Ast.MessageRecord
    }

  private def convertRecordField(eRecordField: Forms.RecordFieldDecl): Ast.Field[Ast.Type] =
    eRecordField match {
      case Forms.RecordFieldUntyped(p, name, initValue) =>
        // untyped or implicitly typed record fields are not supported
        throw new UnsupportedSyntaxError(p)
      case Forms.RecordFieldTyped(p, name, Some(initValue), tp) =>
        // initializers are not supported (yet)
        throw new UnsupportedSyntaxError(p)
      case Forms.RecordFieldTyped(p, name, None, tp) =>
        Ast.Field(name, convertType(tp))(p)
    }

  private def convertFunClause(clause: Exprs.Clause): Ast.Clause =
    Ast.Clause(clause.pats.map(convertPattern), clause.guards.map(convertGuard), convertBody(clause.body))

  private def convertCaseClause(clause: Exprs.Clause): Ast.Rule =
    clause.pats match {
      case List(pat) =>
        Ast.Rule(convertPattern(pat), clause.guards.map(convertGuard), convertBody(clause.body))
      case _ =>
        // Impossible in practice, need to handle due to clumsiness of abstract forms
        throw new UnsupportedSyntaxError(clause.p)
    }

  private def convertCatchClause(clause: Exprs.Clause): Ast.Rule =
    clause.pats match {
      case List(Patterns.TuplePattern(p, List(exnClass, exn, stackTrace))) =>
        val stackTracePat = convertPattern(stackTrace)
        stackTracePat match {
          case Ast.WildPat() =>
          // OK
          case _ =>
            // we do not type "stack traces" yet
            throw new UnsupportedSyntaxError(stackTracePat.p)
        }
        val exnPat = convertPattern(exn)
        Ast.Rule(exnPat, clause.guards.map(convertGuard), convertBody(clause.body))
      case _ =>
        // Impossible in practice, need to handle due to clumsiness of abstract forms
        throw new UnsupportedSyntaxError(clause.p)
    }

  private def convertIfClause(ifClause: Exprs.IfClause): Ast.IfClause =
    Ast.IfClause(ifClause.guards.map(convertGuard), convertBody(ifClause.body))

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
          case Exprs.AtomLiteral(p, _) =>
            // atom literals are not supported on its own
            throw new UnsupportedSyntaxError(p)
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
          case Some(binOp) =>
            Ast.BinOpExp(binOp, convertGExpr(exp1), convertGExpr(exp2))(p)
          case None =>
            // Unknown binary operation
            throw new UnsupportedSyntaxError(p)
        }
      case Guards.GUnaryOp(p, op, exp1) =>
        Ast.unOps.get(op) match {
          case Some(uOp) =>
            Ast.UOpExp(uOp, convertGExpr(exp1))(p)
          case None =>
            // unknown unary operation
            throw new UnsupportedSyntaxError(p)
        }
      case Guards.GLocalEnumCtr(p, enum, ctr, args) =>
        Ast.EnumConExp(Ast.LocalName(enum), ctr, args.map(convertGExpr))(p)
      case Guards.GRemoteEnumCtr(p, module, enum, ctr, args) =>
        Ast.EnumConExp(Ast.RemoteName(module, enum), ctr, args.map(convertGExpr))(p)
      case Guards.GBin(p, elems) =>
        Ast.Bin(elems.map(convertGBinElem))(p)
      case Guards.GRecordCreate(p, recordName, fields) =>
        Ast.ERecordCreate(recordName, fields.map(gRecordField))(p)
      case Guards.GRecordIndex(p, recordName, fieldName) =>
        Ast.ERecordIndex(recordName, fieldName)(p)
      case Guards.GRecordFieldAccess(p, rec, recordName, fieldName) =>
        Ast.ERecordSelect(convertGExpr(rec), recordName, fieldName)(p)
    }

  private def convertBody(exprs: List[Exprs.Expr]): Ast.Body =
    Ast.Body(exprs.init.map(convertValDef), convertValDef(exprs.last))

  private def convertValDef(expr: Exprs.Expr): Ast.ValDef =
    expr match {
      case Exprs.Match(p, pat, exp) =>
        Ast.ValDef(convertPattern(pat), convertExpr(exp))
      case e =>
        val exp = convertExpr(e)
        Ast.ValDef(Ast.WildPat()(exp.p), exp)
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
          case Exprs.AtomLiteral(p, _) =>
            // atom literals are not supported on its own
            throw new UnsupportedSyntaxError(p)
          case Exprs.CharLiteral(p, _) =>
            // we do not handle chars in patterns explicitly yet
            throw new UnsupportedSyntaxError(p)
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
      case Patterns.MapPattern(p, open, assocs) =>
        val pats = assocs map { elem =>
          elem.key match {
            case Patterns.LiteralPattern(Exprs.AtomLiteral(_, label)) =>
              Ast.Field[Ast.Pat](label, convertPattern(elem.value))(elem.p)
            case other =>
              // we support only map pattern for polymorphic records
              throw new UnsupportedSyntaxError(other.p)
          }
        }
        Ast.RecordPat(pats, open)(p)
      case Patterns.BinPattern(p, elems) =>
        Ast.BinPat(elems.map(convertBinElemPat))(p)
      case Patterns.BinOpPattern(p, op, pat1, pat2) =>
        // we do not support things like (4 + 5) in patterns
        throw new UnsupportedSyntaxError(p)
      case Patterns.UnOpPattern(p, op, pat1) =>
        // NB: -1 is UnOp('-', 1) - possibly we should fix it in the parser
        throw new UnsupportedSyntaxError(p)
      case Patterns.RecordPattern(p, recordName, fields) =>
        Ast.ERecordPat(recordName, fields.map(recordFieldPattern))(p)
      case Patterns.RecordIndexPattern(p, recordName, fieldName) =>
        Ast.ERecordIndexPat(recordName, fieldName)(p)
    }

  private def convertExpr(e: Exprs.Expr): Ast.Exp =
    e match {
      case Exprs.Match(p, _, _) =>
        // see https://github.com/WhatsApp/erl2/issues/107
        throw new UnsupportedSyntaxError(p)
      case Exprs.Variable(p, name) =>
        Ast.VarExp(new Ast.LocalVarName(name))(p)
      case literal: Exprs.Literal =>
        literal match {
          case Exprs.AtomLiteral(p, "true") =>
            Ast.BoolExp(true)(p)
          case Exprs.AtomLiteral(p, "false") =>
            Ast.BoolExp(false)(p)
          case Exprs.AtomLiteral(p, _) =>
            // Atom literals are not supported on their own
            throw new UnsupportedSyntaxError(p)
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
      case Exprs.If(p, ifClauses) =>
        Ast.IfExp(ifClauses.map(convertIfClause))(p)
      case Exprs.LocalCall(p1, Exprs.AtomLiteral(p2, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.LocalFunName(f, args.length))(p2), args.map(convertExpr))(p1)
      case Exprs.LocalCall(p, head, args) =>
        Ast.AppExp(convertExpr(head), args.map(convertExpr))(p)
      case Exprs.RemoteCall(p1, Exprs.AtomLiteral(p2, m), Exprs.AtomLiteral(p3, f), args) =>
        Ast.AppExp(Ast.VarExp(new Ast.RemoteFunName(m, f, args.length))(p2 ! p3), args.map(convertExpr))(p1)
      case Exprs.RemoteCall(p, _, _, _) =>
        // Remote calls in the form of Module:Fun(...) are not supported
        throw new UnsupportedSyntaxError(p)
      case Exprs.LocalFun(p, f, arity) =>
        Ast.VarExp(new Ast.LocalFunName(f, arity))(p)
      case Exprs.RemoteFun(p, Exprs.AtomLiteral(_, m), Exprs.AtomLiteral(_, f), Exprs.IntLiteral(_, arity)) =>
        Ast.VarExp(new Ast.RemoteFunName(m, f, arity))(p)
      case Exprs.RemoteFun(p, _, _, _) =>
        // Remote funs in the form of Module:Fun/3 are not supported
        throw new UnsupportedSyntaxError(p)
      case Exprs.Fun(p, clauses) =>
        Ast.FnExp(clauses.map(convertFunClause))(p)
      case Exprs.NamedFun(p, funName, clauses) =>
        Ast.NamedFnExp(new Ast.LocalVarName(funName), clauses.map(convertFunClause))(p)
      case Exprs.UnaryOp(p, op, exp1) =>
        Ast.unOps.get(op) match {
          case Some(uOp) =>
            Ast.UOpExp(uOp, convertExpr(exp1))(p)
          case None =>
            // Unknown unary operation
            throw new UnsupportedSyntaxError(p)
        }
      case Exprs.BinaryOp(p, ".", exp, Exprs.AtomLiteral(_, field)) =>
        Ast.SelExp(convertExpr(exp), field)(p)
      case Exprs.BinaryOp(p, op, exp1, exp2) =>
        Ast.binOps.get(op) match {
          case Some(binOp) =>
            Ast.BinOpExp(binOp, convertExpr(exp1), convertExpr(exp2))(p)
          case None =>
            // Unknown binary operation
            throw new UnsupportedSyntaxError(p)
        }
      case Exprs.ListComprehension(p, template, qualifiers) =>
        Ast.Comprehension(
          convertExpr(template),
          qualifiers.map {
            case Exprs.Filter(e)       => Ast.Filter(convertExpr(e))
            case Exprs.LGenerate(p, e) => Ast.Generator(convertPattern(p), convertExpr(e))
            case Exprs.BGenerate(p, e) => Ast.BGenerator(convertPattern(p), convertExpr(e))
          },
        )(p)
      case Exprs.BinaryComprehension(p, template, qualifiers) =>
        Ast.BComprehension(
          convertExpr(template),
          qualifiers.map {
            case Exprs.Filter(e)       => Ast.Filter(convertExpr(e))
            case Exprs.LGenerate(p, e) => Ast.Generator(convertPattern(p), convertExpr(e))
            case Exprs.BGenerate(p, e) => Ast.BGenerator(convertPattern(p), convertExpr(e))
          },
        )(p)
      case Exprs.Bin(p, elems) =>
        Ast.Bin(elems.map(convertBinElem))(p)
      case Exprs.RecordCreate(p, recordName, fields) =>
        Ast.ERecordCreate(recordName, fields.map(recordField))(p)
      case Exprs.RecordUpdate(p, rec, recordName, fields) =>
        Ast.ERecordUpdate(convertExpr(rec), recordName, fields.map(recordField))(p)
      case Exprs.RecordIndex(p, recordName, fieldName) =>
        Ast.ERecordIndex(recordName, fieldName)(p)
      case Exprs.RecordFieldAccess(p, rec, recordName, fieldName) =>
        Ast.ERecordSelect(convertExpr(rec), recordName, fieldName)(p)
      case Exprs.Try(p, body, tryClauses, catchClauses, after) =>
        val tryBody = convertBody(body)
        val tryRules = tryClauses.map(convertCaseClause)
        val catchRules = catchClauses.map(convertCatchClause)
        val afterBody = after match {
          case Nil => None
          case _   => Some(convertBody(after))
        }
        tryRules match {
          case Nil => Ast.TryCatchExp(tryBody, catchRules, afterBody)(p)
          case _   => Ast.TryOfCatchExp(tryBody, tryRules, catchRules, afterBody)(p)
        }
      case Exprs.Catch(p, exp) =>
        // catch expressions are not supported
        throw new UnsupportedSyntaxError(p)
      case Exprs.Receive(p, clauses) =>
        // WIP -- https://github.com/WhatsApp/erl2/issues/122
        throw new UnsupportedSyntaxError(p)
      case Exprs.ReceiveWithTimeout(p, cl, timeout, default) =>
        // WIP -- https://github.com/WhatsApp/erl2/issues/122
        throw new UnsupportedSyntaxError(p)
    }

  private def convertBinElem(binElem: Exprs.BinElement): Ast.BinElement = {
    val expr = convertExpr(binElem.expr)
    val size = binElem.size.map(convertExpr)
    val binElemType = extractBinElemType(binElem.typeSpecifiers)
    Ast.BinElement(expr, size, binElemType)
  }

  private def convertGBinElem(binElem: Guards.GBinElement): Ast.BinElement = {
    val expr = convertGExpr(binElem.gExpr)
    val size = binElem.size.map(convertGExpr)
    val binElemType = extractBinElemType(binElem.typeSpecifiers)
    Ast.BinElement(expr, size, binElemType)
  }

  private def convertBinElemPat(binElemPat: Patterns.BinElementPattern): Ast.BinElementPat = {
    val pat = convertPattern(binElemPat.pat)
    val size = binElemPat.size.map(convertExpr)
    val binElemType = extractBinElemType(binElemPat.typeSpecifiers)
    Ast.BinElementPat(pat, size, binElemType)
  }

  private def extractBinElemType(specifiers: Exprs.TypeSpecifiers): Option[Ast.BinElemType] =
    specifiers match {
      case Exprs.DefaultTypeSpecifier =>
        None
      case Exprs.TypeSpecifierList(specifiers) =>
        specifiers.flatMap {
          case Exprs.TypeSpecifierUnit(_) => None
          case Exprs.TypeSpecifierId(id)  => typeSpecifiersMapping.get(id)
        }.headOption
    }

  private def assocCreateToFieldExp(assoc: Exprs.Assoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Exprs.OptAssoc(p, Exprs.AtomLiteral(_, label), e) =>
        Ast.Field(label, convertExpr(e))(p)
      case _ =>
        // "wrong anon struct syntax"
        throw new UnsupportedSyntaxError(assoc.p)
    }

  private def assocUpdateToFieldExp(assoc: Exprs.Assoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Exprs.AssocExact(p, Exprs.AtomLiteral(_, label), e) =>
        Ast.Field(label, convertExpr(e))(p)
      case _ =>
        // "wrong anon struct syntax"
        throw new UnsupportedSyntaxError(assoc.p)
    }

  private def recordField(field: Exprs.RecordField): Ast.Field[Ast.Exp] =
    Ast.Field(field.fieldName, convertExpr(field.value))(field.p)

  private def gRecordField(field: Guards.GRecordField): Ast.Field[Ast.Exp] =
    Ast.Field(field.fieldName, convertGExpr(field.value))(field.p)

  private def recordFieldPattern(field: Patterns.RecordFieldPattern): Ast.Field[Ast.Pat] =
    Ast.Field(field.fieldName, convertPattern(field.pat))(field.p)

  private def gAssocCreateToFieldExp(assoc: Guards.GAssoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Guards.GAssocOpt(p, Guards.GLiteral(Exprs.AtomLiteral(_, label)), e) =>
        Ast.Field(label, convertGExpr(e))(p)
      case _ =>
        // "wrong anon struct syntax"
        throw new UnsupportedSyntaxError(assoc.p)
    }

  private def gAssocUpdateToFieldExp(assoc: Guards.GAssoc): Ast.Field[Ast.Exp] =
    assoc match {
      case Guards.GAssocExact(p, Guards.GLiteral(Exprs.AtomLiteral(_, label)), e) =>
        Ast.Field(label, convertGExpr(e))(p)
      case _ =>
        // "wrong anon struct syntax"
        throw new UnsupportedSyntaxError(assoc.p)
    }

  private def convertType(tp: Types.Type): Ast.Type =
    tp match {
      case Types.AnnotatedType(_, _, tp) =>
        convertType(tp)
      case Types.BitstringType(p, List()) =>
        Ast.UserType(Ast.LocalName("binary"), List())(p)
      // <<_:M,_:_*N>>, see https://github.com/WhatsApp/erl2/issues/112
      case Types.BitstringType(p, _) =>
        throw new UnsupportedSyntaxError(p)
      case Types.FunctionType(p, args, result) =>
        Ast.FunType(args.map(convertType), convertType(result))(p)
      case Types.AssocMap(p, assocs) =>
        assocs match {
          case List() =>
            Ast.RecordType(List())(p)
          case List(Types.Assoc(_, Types.OptAssoc, k, v)) =>
            Ast.UserType(Ast.LocalName("map"), List(convertType(k), convertType(v)))(p)
          case _ =>
            assocs.last match {
              case Types.Assoc(_, Types.ReqAssoc, Types.TypeVariable(p1, "_"), Types.TypeVariable(p2, "_")) =>
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
      case Types.RecordType(p, name, List()) =>
        Ast.ERecordType(name)(p)
      case Types.RecordType(p, _, _) =>
        // redefining field types
        // Erlang support writing
        // -type my_rec = #already_defined_rec { field :: diff_type() }
        throw new UnsupportedSyntaxError(p)
      case Types.AtomType(p, _) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.EmptyListType(p) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.FunTypeAny(p) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.FunTypeAnyArgs(p, _) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.TupleTypeAny(p) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.AnyMap(p) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.EnumCtr(p, _, _) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.UnionType(p, _) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case Types.IntegerRangeType(p, _, _) =>
        // banned
        throw new UnsupportedSyntaxError(p)
      case _: Types.SingletonIntegerType =>
        // banned
        throw new UnsupportedSyntaxError(tp.p)
    }

  private def enumCon(tp: Types.Type): Ast.EnumCon =
    tp match {
      case Types.EnumCtr(p, name, params) =>
        Ast.EnumCon(name, params.map(convertType))(p)
      case _ =>
        throw new UnsupportedSyntaxError(tp.p)
    }

  private def convertAssoc(assoc: Types.Assoc): Ast.Field[Ast.Type] =
    assoc match {
      case Types.Assoc(p, Types.ReqAssoc, Types.AtomType(_, field), v) =>
        Ast.Field(field, convertType(v))(p)
      case _ =>
        // wrong "record syntax"
        throw new UnsupportedSyntaxError(assoc.p)
    }
}
