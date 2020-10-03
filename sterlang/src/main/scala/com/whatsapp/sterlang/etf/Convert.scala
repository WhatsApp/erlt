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

import com.whatsapp.sterlang.{Ast, ParseError, Doc, UnsupportedSyntaxError}
import com.whatsapp.sterlang.forms.{Exprs, Forms, Patterns, Types}

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

  def convertForm(form: Forms.Form): Option[Ast.ProgramElem] =
    form match {
      case Forms.Lang(mods) =>
        Some(Ast.LangElem(mods))
      case Forms.Module(name) =>
        Some(Ast.ModuleElem(name))
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
                  elems.map(convertEnumCtr)
                case single =>
                  List(convertEnumCtr(single))
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
      case Forms.FunctionSpec(specP, (name, arity), types) =>
        val List(Types.FunType(p, params, res)) = types
        val funType = Ast.FunType(params.map(convertType), convertType(res))(p)
        val funName = new Ast.LocalFunName(name, arity)
        val spec = Ast.Spec(funName, funType)(specP)
        Some(Ast.SpecElem(spec))
      case Forms.FunctionDecl(p, name, arity, clauses) =>
        val funName = new Ast.LocalFunName(name, arity)
        val fun = Ast.Fun(funName, clauses.map(convertFunClause))(p)
        Some(Ast.FunElem(fun))
      case Forms.StructDecl(p, name, eFields, kind) =>
        val fields = eFields.map(convertStructFieldDecl)
        val eStructType = Ast.StructDef(name, fields, convertStructKind(kind))(p)
        Some(Ast.StructElem(eStructType))
      case Forms.EOF =>
        None
      case Forms.Error(loc) =>
        throw ParseError(loc)
    }

  private def convertStructKind(strKind: Forms.StructKind): Ast.StructKind =
    strKind match {
      case Forms.StrStruct => Ast.StrStruct
      case Forms.ExnStruct => Ast.ExnStruct
      case Forms.MsgStruct => Ast.MsgStruct
    }

  private def convertStructFieldDecl(structFieldDecl: Forms.StructFieldDecl): Ast.Field[Ast.Type] =
    structFieldDecl match {
      case Forms.StructFieldDecl(p, name, None, tp) =>
        Ast.Field(name, convertType(tp))(p)
    }

  private def convertFunClause(funClause: Exprs.Clause): Ast.Clause =
    Ast.Clause(funClause.pats.map(convertPattern), funClause.guards.map(convertGuard), convertBody(funClause.body))

  private def convertCaseClause(caseClause: Exprs.Clause): Ast.Rule = {
    val List(pat) = caseClause.pats
    Ast.Rule(convertPattern(pat), caseClause.guards.map(convertGuard), convertBody(caseClause.body))
  }

  private def convertCatchClause(catchClause: Exprs.Clause): Ast.Rule = {
    val List(Patterns.TuplePattern(p, List(exnClass, exn, stackTrace))) = catchClause.pats
    val Ast.WildPat() = convertPattern(stackTrace)
    val Patterns.LiteralPattern(literal) = exnClass;
    val Exprs.AtomLiteral(_, "throw") = literal
    val exnPat = convertPattern(exn)
    Ast.Rule(exnPat, catchClause.guards.map(convertGuard), convertBody(catchClause.body))
  }

  private def convertIfClause(ifClause: Exprs.IfClause): Ast.IfClause =
    Ast.IfClause(ifClause.guards.map(convertGuard), convertBody(ifClause.body))

  private def convertGuard(guard: Exprs.Guard): Ast.Guard =
    Ast.Guard(guard.elems.map(convertExpr))

  private def convertBody(exprs: List[Exprs.Expr]): Ast.Body =
    Ast.Body(exprs.init.map(convertValDef), convertValDef(exprs.last))

  private def convertValDef(expr: Exprs.Expr): Ast.ValDef =
    expr match {
      case Exprs.Match(p, pat, exp) =>
        Ast.ValDef(convertPattern(pat), convertExpr(exp))
      case e =>
        val exp = convertExpr(e)
        Ast.ValDef(Ast.WildPat()(exp.r), exp)
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
            throw new UnsupportedSyntaxError(p, "Atom literals")
          case Exprs.CharLiteral(p, c) =>
            Ast.CharPat(c)(p)
          case Exprs.FloatLiteral(p, fl) =>
            Ast.NumberPat(fl.intValue())(p)
          case Exprs.IntLiteral(p, i) =>
            Ast.NumberPat(i)(p)
          case Exprs.StringLiteral(p, str) =>
            Ast.StringPat(str)(p)
        }
      case Patterns.TuplePattern(p, elems) =>
        Ast.TuplePat(elems.map(convertPattern))(p)
      case Patterns.NilPattern(p) =>
        Ast.NilPat()(p)
      case Patterns.ConsPattern(p, hd, tl) =>
        Ast.ConsPat(convertPattern(hd), convertPattern(tl))(p)
      case Patterns.LocalEnumPattern(p, enum, ctr, args) =>
        Ast.EnumPat(Ast.LocalName(enum), ctr, args.map(convertPattern))(p)
      case Patterns.RemoteEnumPattern(p, module, enum, ctr, args) =>
        Ast.EnumPat(Ast.RemoteName(module, enum), ctr, args.map(convertPattern))(p)
      case Patterns.ShapePattern(p, fields) =>
        val pats = fields map { elem =>
          val Patterns.LiteralPattern(Exprs.AtomLiteral(_, label)) = elem.key
          Ast.Field[Ast.Pat](label, convertPattern(elem.value))(elem.r)
        }
        Ast.ShapePat(pats)(p)
      case Patterns.BinPattern(p, elems) =>
        Ast.BinPat(elems.map(convertBinElemPattern))(p)
      case Patterns.BinOpPattern(p, op, pat1, pat2) =>
        // Classic erlang allows this:
        // test(1 + 3) -> 4. But it has very little sense for ST
        // we do not support things like (4 + 5) in patterns
        throw new UnsupportedSyntaxError(p, "Calculation in patterns")
      case Patterns.UnOpPattern(p, op, pat1) =>
        // - This should be fixed in parser in the first place.
        // NB: -1 is UnOp('-', 1) - possibly we should fix it in the parser
        throw new UnsupportedSyntaxError(p, "Calculation in patterns")
      case Patterns.StructPattern(p, structName, fields) =>
        Ast.StructPat(structName, fields.map(convertStructFieldPattern))(p)
    }

  private def convertExpr(e: Exprs.Expr): Ast.Exp =
    e match {
      case Exprs.Match(p, _, _) =>
        throw new UnsupportedSyntaxError(p, "Match expression")
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
            throw new UnsupportedSyntaxError(p, "Atom literal")
          case Exprs.CharLiteral(p, ch) =>
            Ast.CharExp(ch)(p)
          case Exprs.FloatLiteral(p, fl) =>
            Ast.NumberExp(fl.intValue())(p)
          case Exprs.IntLiteral(p, i) =>
            Ast.NumberExp(i)(p)
          case Exprs.StringLiteral(p, str) =>
            Ast.StringExp(str)(p)
        }
      case Exprs.Tuple(p, elems) =>
        Ast.TupleExp(elems.map(convertExpr))(p)
      case Exprs.Nil(p) =>
        Ast.NilExp()(p)
      case Exprs.Cons(p, hd, tl) =>
        Ast.ConsExp(convertExpr(hd), convertExpr(tl))(p)
      case Exprs.LocalEnum(p, enum, ctr, args) =>
        Ast.EnumExp(Ast.LocalName(enum), ctr, args.map(convertExpr))(p)
      case Exprs.RemoteEnum(p, module, enum, ctr, args) =>
        Ast.EnumExp(Ast.RemoteName(module, enum), ctr, args.map(convertExpr))(p)
      case Exprs.ShapeCreate(p, entries) =>
        Ast.ShapeCreateExp(entries.map(convertShapeField))(p)
      case Exprs.ShapeUpdate(p, exp, entries) =>
        val shapeExp = convertExpr(exp)
        // this is not present in Erlang. Approximating
        // TODO - it should be just fields in AST!
        val updateRange = Doc.Range(shapeExp.r.end, p.end)
        Ast.ShapeUpdateExp(shapeExp, Ast.ShapeCreateExp(entries.map(convertShapeField))(updateRange))(p)
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
      case Exprs.LocalFun(p, f, arity) =>
        Ast.VarExp(new Ast.LocalFunName(f, arity))(p)
      case Exprs.RemoteFun(p, Exprs.AtomLiteral(_, m), Exprs.AtomLiteral(_, f), Exprs.IntLiteral(_, arity)) =>
        Ast.VarExp(new Ast.RemoteFunName(m, f, arity))(p)
      case Exprs.Fun(p, clauses) =>
        Ast.FnExp(clauses.map(convertFunClause))(p)
      case Exprs.NamedFun(p, funName, clauses) =>
        Ast.NamedFnExp(new Ast.LocalVarName(funName), clauses.map(convertFunClause))(p)
      case Exprs.UnaryOp(p, op, exp1) =>
        Ast.UOpExp(Ast.unOps(op), convertExpr(exp1))(p)
      case Exprs.ShapeSelect(p, map, fieldName) =>
        Ast.ShapeSelectExp(convertExpr(map), fieldName)(p)
      case Exprs.BinaryOp(p, op, exp1, exp2) =>
        Ast.BinOpExp(Ast.binOps(op), convertExpr(exp1), convertExpr(exp2))(p)
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
      case Exprs.StructCreate(p, structName, fields) =>
        Ast.StructCreate(structName, fields.map(convertStructField))(p)
      case Exprs.StructUpdate(p, struct, structName, fields) =>
        Ast.StructUpdate(convertExpr(struct), structName, fields.map(convertStructField))(p)
      case Exprs.StructSelect(p, struct, structName, fieldName) =>
        Ast.StructSelect(convertExpr(struct), structName, fieldName)(p)
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
      case Exprs.Receive(p, clauses) =>
        Ast.ReceiveExp(clauses.map(convertCaseClause), None)(p)
      case Exprs.ReceiveWithTimeout(p, clauses, timeout, body) =>
        Ast.ReceiveExp(clauses.map(convertCaseClause), Some(Ast.AfterBody(convertExpr(timeout), convertBody(body))))(p)
    }

  private def convertBinElem(binElem: Exprs.BinElement): Ast.BinElement = {
    val expr = convertExpr(binElem.expr)
    val size = binElem.size.map(convertExpr)
    val binElemType = extractBinElemType(binElem.typeSpecifiers)
    Ast.BinElement(expr, size, binElemType)
  }

  private def convertBinElemPattern(binElemPat: Patterns.BinElementPattern): Ast.BinElementPat = {
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
        specifiers.flatMap(s => typeSpecifiersMapping.get(s.id)).headOption
    }

  private def convertShapeField(assoc: Exprs.ShapeField): Ast.Field[Ast.Exp] = {
    val Exprs.ShapeField(p, Exprs.AtomLiteral(_, label), e) = assoc
    Ast.Field(label, convertExpr(e))(p)
  }

  private def convertStructField(field: Exprs.StructField): Ast.Field[Ast.Exp] =
    Ast.Field(field.fieldName, convertExpr(field.value))(field.r)

  private def convertStructFieldPattern(field: Patterns.StructFieldPattern): Ast.Field[Ast.Pat] =
    Ast.Field(field.fieldName, convertPattern(field.pat))(field.r)

  private def convertType(tp: Types.Type): Ast.Type =
    tp match {
      case Types.AnnotatedType(_, _, tp) =>
        convertType(tp)
      case Types.BitstringType(p) =>
        Ast.UserType(Ast.LocalName("binary"), List())(p)
      case Types.FunType(p, args, result) =>
        Ast.FunType(args.map(convertType), convertType(result))(p)
      case Types.Shape(p, fieldTypes) =>
        Ast.ShapeType(fieldTypes.map(convertKeyValueType))(p)
      case Types.OpenShape(p, fieldType, extType) =>
        val eType = extType match {
          case Types.TypeVariable(p, "_") =>
            Left(Ast.WildTypeVar()(p))
          case Types.TypeVariable(p, v) =>
            Right(Ast.TypeVar(v)(p))
        }
        Ast.OpenShapeType(fieldType.map(convertKeyValueType), eType)(p)
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
      case Types.AtomType(p, _) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Atom literal")
      case Types.TupleTypeAny(p) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Bad tuple type")
      case Types.AnyMap(p) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Bad map type")
      case Types.EnumCtr(p, _, _) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Enum ctr used as a type")
      case Types.UnionType(p, _) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Union type")
    }

  private def convertEnumCtr(tp: Types.Type): Ast.EnumCtr =
    tp match {
      case Types.EnumCtr(p, name, params) =>
        Ast.EnumCtr(name, params.map(convertType))(p)
      case _ =>
        throw new UnsupportedSyntaxError(tp.r, "Enum ctr is expected")
    }

  private def convertKeyValueType(assoc: Types.ShapeField): Ast.Field[Ast.Type] = {
    val Types.ShapeField(p, Types.AtomType(_, field), v) = assoc
    Ast.Field(field, convertType(v))(p)
  }
}
