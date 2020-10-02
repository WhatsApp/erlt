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

import scala.collection.mutable.ListBuffer

object TypePrinter2 {
  sealed trait Mode
  case object Types extends Mode
  case object Specs extends Mode
}

case class TypePrinter2(vars: Vars) {

  val printer = new TypePrinter(vars, new TypesUtil(vars))

  def showFunSpecs(funs: List[AnnAst.Fun], env: Env): List[String] = {
    funs.map { f =>
      val typeSchema = env(f.name)
      // fun((ArgType1, ArgType2, ...) -> ResType)
      val raw = printer.typeSchema(typeSchema, TypePrinter2.Specs)
      val inner = raw.substring(4, raw.length - 1)
      val slashIndex = f.name.lastIndexOf('/')
      val normV = f.name.substring(0, slashIndex)
      val pp = "-spec " + normV + inner + "."
      pp
    }
  }

  def printType(tp: Types.Type): String = {
    val fakeTypeScheme = STypes.TypeSchema(0, List(), STypes.PlainType(tp))
    printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
  }

  def printScheme(s: STypes.TypeSchema): String = {
    printer.typeSchema(s, TypePrinter2.Specs)
  }

  def showFunTypes(funs: List[AnnAst.Fun]): List[String] = {
    val buf = new ListBuffer[String]
    funs.foreach {
      case AnnAst.Fun(name, clauses, fType) =>
        val fakeTypeScheme = STypes.TypeSchema(0, List(), STypes.PlainType(fType))
        val typeSchemaString = printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
        val pp = "val " + name + ": " + typeSchemaString

        buf.append(pp)

        clauses.foreach { clause =>
          clause.pats.foreach(printPat(buf))
          printBody(buf)(clause.body)
        }
    }

    buf.toList

  }

  private def printPat(buf: ListBuffer[String])(pat: AnnAst.Pat): Unit =
    pat match {
      case AnnAst.WildPat() =>
      // nothing
      case AnnAst.VarPat(v) =>
        val fakeTypeScheme = STypes.TypeSchema(0, List(), STypes.PlainType(pat.typ))
        val typeSchemaString = printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
        val pp = "val " + v + ": " + typeSchemaString
        buf.append(pp)
      case AnnAst.AndPat(p1, p2) =>
        printPat(buf)(p1)
        printPat(buf)(p2)

      case AnnAst.LiteralPat(_) =>
      //
      case AnnAst.TuplePat(pats) =>
        pats.foreach(printPat(buf))
      case AnnAst.NilPat() =>
      //
      case AnnAst.ShapePat(fields) =>
        fields.foreach { f => printPat(buf)(f.value) }

      case AnnAst.ConsPat(hPat, tPat) =>
        printPat(buf)(hPat)
        printPat(buf)(tPat)
      case AnnAst.EnumPat(_, _, pats) =>
        pats.foreach(printPat(buf))

      case AnnAst.BinPat(elems) =>
        elems.foreach { elem =>
          printPat(buf)(elem.pat)
          elem.size.map(printExp(buf))
        }

      case AnnAst.StructPat(_, fields) =>
        fields.foreach { f => printPat(buf)(f.value) }
    }

  private def printBody(buf: ListBuffer[String])(body: AnnAst.Body): Unit = {
    printValDefsTypes(buf)(body.prelude)
    printValDef(buf)(body.main)
  }

  private def printValDefsTypes(buf: ListBuffer[String])(defs: List[AnnAst.ValDef]): Unit = {
    defs.foreach(printValDef(buf))
  }

  private def printValDef(buf: ListBuffer[String])(d: AnnAst.ValDef): Unit =
    d match {
      case AnnAst.ValDef(pat, exp, _, _, _) =>
        printPat(buf)(pat)
        printExp(buf)(exp)
    }

  private def printExp(buf: ListBuffer[String])(exp: AnnAst.Exp): Unit =
    exp match {
      case AnnAst.VarExp(_) =>
      // Nothing

      case AnnAst.LiteralExp(_) =>
      // Nothing
      case AnnAst.TupleExp(elems) =>
        elems.foreach(printExp(buf))
      case AnnAst.NilExp() =>
      // Nothing
      case AnnAst.BinExp(elems) =>
        elems.foreach { elem =>
          printExp(buf)(elem.expr)
          elem.size.foreach(printExp(buf))
        }

      case AnnAst.UOpExp(_, exp) =>
        printExp(buf)(exp)
      case AnnAst.BinOpExp(_, exp1, exp2) =>
        printExp(buf)(exp1)
        printExp(buf)(exp2)

      case AnnAst.CaseExp(selector, branches) =>
        printExp(buf)(selector)
        branches.foreach { branch =>
          printPat(buf)(branch.pat)
          printBody(buf)(branch.body)
        }

      case AnnAst.IfExp(bodies) =>
        bodies.foreach(printBody(buf))

      case AnnAst.Comprehension(template, qualifiers) =>
        qualifiers.foreach {
          case AnnAst.Filter(exp) =>
            printExp(buf)(exp)
          case AnnAst.Generator(pat, exp) =>
            printPat(buf)(pat)
            printExp(buf)(exp)
          case AnnAst.BGenerator(pat, exp) =>
            printPat(buf)(pat)
            printExp(buf)(exp)
        }
        printExp(buf)(template)
      case AnnAst.BComprehension(template, qualifiers) =>
        qualifiers.foreach {
          case AnnAst.Filter(exp) =>
            printExp(buf)(exp)
          case AnnAst.Generator(pat, exp) =>
            printPat(buf)(pat)
            printExp(buf)(exp)
          case AnnAst.BGenerator(pat, exp) =>
            printPat(buf)(pat)
            printExp(buf)(exp)
        }
        printExp(buf)(template)

      case AnnAst.BlockExp(body) =>
        printBody(buf)(body)

      case AnnAst.ShapeCreateExp(fields) =>
        fields.foreach(f => printExp(buf)(f.value))
      case AnnAst.ShapeSelectExp(exp, _) =>
        printExp(buf)(exp)
      case AnnAst.ShapeUpdateExp(exp, fields) =>
        printExp(buf)(exp)
        fields.foreach(f => printExp(buf)(f.value))

      case AnnAst.StructCreate(_, fields) =>
        fields.foreach(f => printExp(buf)(f.value))
      case AnnAst.StructUpdate(struct, _, fields) =>
        printExp(buf)(struct)
        fields.foreach(f => printExp(buf)(f.value))
      case AnnAst.StructSelect(struct, _, _) =>
        printExp(buf)(struct)

      case AnnAst.FnExp(clauses) =>
        clauses.foreach { clause =>
          clause.pats.foreach(printPat(buf))
          printBody(buf)(clause.body)
        }
      case AnnAst.NamedFnExp(name, clauses) =>
        // TODO: the source location is wrong, but I don't think it matters.
        printPat(buf)(AnnAst.VarPat(name)(exp.r)(exp.typ))
        clauses.foreach { clause =>
          clause.pats.foreach(printPat(buf))
          printBody(buf)(clause.body)
        }
      case AnnAst.AppExp(head, args) =>
        printExp(buf)(head)
        args.foreach(printExp(buf))

      case AnnAst.ConsExp(h, t) =>
        printExp(buf)(h)
        printExp(buf)(t)
      case AnnAst.EnumExp(_, _, exprs) =>
        exprs.foreach(printExp(buf))

      case AnnAst.TryCatchExp(tryBody, catchBranches, after) =>
        printBody(buf)(tryBody)
        catchBranches.foreach { branch =>
          printPat(buf)(branch.pat)
          printBody(buf)(branch.body)
        }
        after.foreach(printBody(buf))

      case AnnAst.TryOfCatchExp(tryBody, tryBranches, catchBranches, after) =>
        printBody(buf)(tryBody)
        tryBranches.foreach { branch =>
          printPat(buf)(branch.pat)
          printBody(buf)(branch.body)
        }
        catchBranches.foreach { branch =>
          printPat(buf)(branch.pat)
          printBody(buf)(branch.body)
        }
        after.foreach(printBody(buf))

      case AnnAst.ReceiveExp(branches, after) =>
        branches.foreach { branch =>
          printPat(buf)(branch.pat)
          printBody(buf)(branch.body)
        }
        after.foreach { afterBody =>
          printExp(buf)(afterBody.timeout)
          printBody(buf)(afterBody.body)
        }
    }
}
