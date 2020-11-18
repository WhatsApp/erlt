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

package com.whatsapp.sterlang

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
          case Forms.Type =>
            val typeAlias = Ast.TypeAlias(typeName, typeParams, convertType(body))(p)
            Some(Ast.TypeAliasElem(typeAlias))
          case Forms.Opaque =>
            val opaque = Ast.Opaque(typeName, typeParams, convertType(body))(p)
            Some(Ast.OpaqueElem(opaque))
        }
      case Forms.Spec(specP, (name, arity), types) =>
        val List(Types.FunType(p, params, res)) = types
        val funType = Ast.FunType(params.map(convertType), convertType(res))(p)
        val funName = new Ast.LocalFunName(name, arity)
        val spec = Ast.Spec(funName, funType)(specP)
        Some(Ast.SpecElem(spec))
      case Forms.Function(p, name, arity, clauses) =>
        val funName = new Ast.LocalFunName(name, arity)
        val fun = Ast.Fun(funName, clauses.map(convertFunClause))(p)
        Some(Ast.FunElem(fun))
      case Forms.UncheckedFunction(name, arity) =>
        val funName = new Ast.LocalFunName(name, arity)
        Some(Ast.UncheckedFunElem(Ast.UncheckedFun(funName)))
      case Forms.EnumDecl(p, name, params, enumVariants) =>
        val typeParams = params.map { case Types.TypeVariable(p, n) => Ast.TypeVar(n)(p) }
        val enumDef = Ast.EnumDef(name, typeParams, enumVariants.map(convertEnumCtr))(p)
        Some(Ast.EnumElem(enumDef))
      case Forms.StructDecl(p, name, eParams, eFields, kind) =>
        val params = eParams.map { case Types.TypeVariable(p, n) => Ast.TypeVar(n)(p) }
        val fields = eFields.map(convertFieldDecl)
        val eStructType = Ast.StructDef(name, params, fields, convertStructKind(kind))(p)
        Some(Ast.StructElem(eStructType))
      case Forms.EOF =>
        None
      case Forms.UncheckedTypeDecl(r, typeName, params) =>
        val typeParams = params.map { case Types.TypeVariable(p, n) => Ast.TypeVar(n)(p) }
        Some(Ast.UncheckedOpaqueElem(Ast.UncheckedOpaque(typeName, typeParams)(r)))
      case Forms.Error(loc) =>
        throw new ParseError(loc)
      case Forms.WildAttribute(_, _) =>
        None
    }

  private def convertStructKind(strKind: Forms.StructKind): Ast.StructKind =
    strKind match {
      case Forms.StrStruct => Ast.StrStruct
      case Forms.ExnStruct => Ast.ExnStruct
      case Forms.MsgStruct => Ast.MsgStruct
    }

  private def convertFieldDecl(structFieldDecl: Forms.FieldDecl): Ast.FieldDecl =
    structFieldDecl match {
      case Forms.LblFieldDecl(p, name, default, tp) =>
        Ast.LblFieldDecl(name, convertType(tp), default.map(convertExpr))(p)
      case Forms.PosFieldDecl(p, tp) =>
        Ast.PosFieldDecl(convertType(tp))(p)
    }

  private def convertFunClause(funClause: Exprs.Clause): Ast.Clause =
    Ast.Clause(funClause.pats.map(convertPattern), funClause.guards.map(convertGuard), convertBody(funClause.body))

  private def convertCaseClause(caseClause: Exprs.Clause): Ast.Rule = {
    val List(pat) = caseClause.pats
    Ast.Rule(convertPattern(pat), caseClause.guards.map(convertGuard), convertBody(caseClause.body))
  }

  private def convertCatchClause(catchClause: Exprs.Clause): Ast.Rule = {
    val List(exn) = catchClause.pats
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
      case Patterns.PinnedVariablePattern(p, name) =>
        Ast.PinnedVarPat(name)(p)
      case Patterns.LiteralPattern(literal) =>
        literal match {
          case Exprs.AtomLiteral(p, "true") =>
            Ast.BoolPat(true)(p)
          case Exprs.AtomLiteral(p, "false") =>
            Ast.BoolPat(false)(p)
          case Exprs.AtomLiteral(p, atom) =>
            Ast.AtomPat(atom)(p)
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
      case Patterns.LocalEnumPattern(p, enum, ctr, fields) =>
        Ast.EnumPat(Ast.LocalName(enum), ctr, fields.map(convertFieldPat))(p)
      case Patterns.RemoteEnumPattern(p, module, enum, ctr, args) =>
        Ast.EnumPat(Ast.RemoteName(module, enum), ctr, args.map(convertFieldPat))(p)
      case Patterns.ShapePattern(p, fields) =>
        val pats = fields map { elem =>
          val Patterns.LiteralPattern(Exprs.AtomLiteral(_, label)) = elem.key
          Ast.LblField[Ast.Pat](label, convertPattern(elem.value))(elem.r)
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
      case Patterns.LocalStructPattern(p, structName, fields) =>
        Ast.StructPat(Ast.LocalName(structName), fields.map(convertFieldPat))(p)
      case Patterns.RemoteStructPattern(p, module, structName, fields) =>
        Ast.StructPat(Ast.RemoteName(module, structName), fields.map(convertFieldPat))(p)
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
          case Exprs.AtomLiteral(p, atom) =>
            Ast.AtomExp(atom)(p)
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
      case Exprs.LocalEnum(p, enum, ctr, fields) =>
        Ast.EnumExp(Ast.LocalName(enum), ctr, fields.map(convertFieldExp))(p)
      case Exprs.RemoteEnum(p, module, enum, ctr, args) =>
        Ast.EnumExp(Ast.RemoteName(module, enum), ctr, args.map(convertFieldExp))(p)
      case Exprs.ShapeCreate(p, entries) =>
        Ast.ShapeCreateExp(entries.map(convertShapeField))(p)
      case Exprs.ShapeUpdate(p, exp, entries) =>
        val shapeExp = convertExpr(exp)
        Ast.ShapeUpdateExp(shapeExp, entries.map(convertShapeField))(p)
      case Exprs.Block(p, exprs) =>
        Ast.BlockExpr(convertBody(exprs))(p)
      case Exprs.Case(p, expr, clauses) =>
        Ast.CaseExp(convertExpr(expr), clauses.map(convertCaseClause))(p)
      case Exprs.If(p, ifClauses) =>
        Ast.IfExp(ifClauses.map(convertIfClause))(p)
      case Exprs.LocalCall(p, Exprs.AtomLiteral(p2, "cast"), args) =>
        val List(
          Exprs.AtomLiteral(_, moduleName),
          Exprs.AtomLiteral(_, m1),
          Exprs.AtomLiteral(_, shapeType),
        ) = args
        Ast.SmartCastExp(moduleName, m1, shapeType)(p)
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
      case Exprs.LocalStructCreate(p, structName, fields) =>
        Ast.StructCreate(Ast.LocalName(structName), fields.map(convertFieldExp))(p)
      case Exprs.RemoteStructCreate(p, module, structName, fields) =>
        Ast.StructCreate(Ast.RemoteName(module, structName), fields.map(convertFieldExp))(p)
      case Exprs.LocalStructUpdate(p, struct, structName, fields) =>
        Ast.StructUpdate(convertExpr(struct), Ast.LocalName(structName), fields.map(convertFieldExp))(p)
      case Exprs.RemoteStructUpdate(p, struct, module, structName, fields) =>
        Ast.StructUpdate(convertExpr(struct), Ast.RemoteName(module, structName), fields.map(convertFieldExp))(p)
      case Exprs.LocalStructSelect(p, struct, structName, index) =>
        Ast.StructSelect(convertExpr(struct), Ast.LocalName(structName), convertIndex(index))(p)
      case Exprs.RemoteStructSelect(p, struct, module, structName, index) =>
        Ast.StructSelect(convertExpr(struct), Ast.RemoteName(module, structName), convertIndex(index))(p)
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
      case _ =>
        // $COVERAGE-OFF$ unreachable
        throw new IllegalStateException(e.toString)
      // $COVERAGE-ON$
    }

  private def convertIndex(index: Exprs.Index): Ast.Index =
    index match {
      case Exprs.LblIndex(lbl) => Ast.LblIndex(lbl)
      case Exprs.PosIndex(pos) => Ast.PosIndex(pos)
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

  private def convertShapeField(assoc: Exprs.ShapeField): Ast.LblField[Ast.Exp] = {
    val Exprs.ShapeField(p, Exprs.AtomLiteral(_, label), e) = assoc
    Ast.LblField(label, convertExpr(e))(p)
  }

  private def convertFieldExp(field: Exprs.Field): Ast.Field[Ast.Exp] =
    field match {
      case Exprs.LblField(r, fieldName, value) =>
        Ast.LblField(fieldName, convertExpr(value))(r)
      case Exprs.PosField(r, value) =>
        Ast.PosField(convertExpr(value))(r)
    }

  private def convertFieldPat(field: Patterns.FieldPattern): Ast.Field[Ast.Pat] =
    field match {
      case Patterns.LblFieldPattern(r, fieldName, value) =>
        Ast.LblField(fieldName, convertPattern(value))(r)
      case Patterns.PosFieldPattern(r, value) =>
        Ast.PosField(convertPattern(value))(r)
    }

  private def convertType(tp: Types.Type): Ast.Type =
    tp match {
      case Types.AnnotatedType(_, _, tp) =>
        convertType(tp)
      case Types.BitstringType(p) =>
        Ast.UserType(Ast.LocalName("binary"), List())(p)
      case Types.FunType(p, args, result) =>
        Ast.FunType(args.map(convertType), convertType(result))(p)
      case Types.Shape(p, fieldTypes) =>
        Ast.ShapeType(fieldTypes.map(converShapeFieldType))(p)
      case Types.OpenShape(p, fieldType, extType) =>
        val eType = extType match {
          case Types.TypeVariable(p, "_") =>
            Left(Ast.WildTypeVar()(p))
          case Types.TypeVariable(p, v) =>
            Right(Ast.TypeVar(v)(p))
        }
        Ast.OpenShapeType(fieldType.map(converShapeFieldType), eType)(p)
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
      case Types.TupleTypeAny(p) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Bad tuple type")
      case Types.AnyMap(p) =>
        // banned
        throw new UnsupportedSyntaxError(p, "Bad map type")
    }

  private def convertEnumCtr(variant: Forms.EnumVariantDecl): Ast.EnumCtr = {
    val Forms.EnumVariantDecl(p, name, fields) = variant
    Ast.EnumCtr(name, fields.map(convertFieldDecl))(p)
  }

  private def converShapeFieldType(assoc: Types.ShapeField): Ast.LblField[Ast.Type] = {
    val Types.ShapeField(p, field, v) = assoc
    Ast.LblField(field, convertType(v))(p)
  }
}
