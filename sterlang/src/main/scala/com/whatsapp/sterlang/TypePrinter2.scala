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

import java.io.StringWriter

object TypePrinter2 {
  sealed trait Mode
  case object TypeSchemes extends Mode
  case object Types extends Mode
}

case class TypePrinter2(vars: Vars, sw: Option[StringWriter]) {
  val A = Absyn
  val ST = STypes
  val printer = new TypePrinter(vars, new TypesUtil(vars))

  def printFunsTypeSchemes(funs: List[A.Fun], env: Env): Unit = {
    funs.foreach { f => printTypeScheme(env, f.name) }
  }

  private def printTypeScheme(env: Env, v: String): Unit = {
    env.get(v) match {
      case Some(typeSchema) =>
        /// "fun(...)"
        val raw = printer.typeSchema(typeSchema, TypePrinter2.TypeSchemes)
        val inner = raw.substring(4, raw.length - 1)
        val slashIndex = v.lastIndexOf('/')
        val normV = v.substring(0, slashIndex)
        val pp = "-spec " + normV + inner + "."
        output(pp)
      case None =>
        sys.error("showvar: variable not found")
    }
  }

  def printValDefsTypes(defs: List[A.ValDef]): Unit = {
    defs.foreach(printValDef)
  }

  def printFuns(funs: List[A.Fun]): Unit = {
    funs.foreach {
      case A.Fun(name, clauses, fType) =>
        val fakeTypeScheme = ST.TypeSchema(0, List(), ST.PlainType(fType))
        val typeSchemaString = printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
        val pp = "val " + name + ": " + typeSchemaString
        output(pp)

        clauses.foreach { clause =>
          clause.pats.foreach(printPat)
          printBody(clause.body)
        }
    }
  }

  def printType(tp: Types.Type): String = {
    val fakeTypeScheme = ST.TypeSchema(0, List(), ST.PlainType(tp))
    printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
  }

  def printScheme(s: ST.TypeSchema): String = {
    printer.typeSchema(s, TypePrinter2.TypeSchemes)
  }

  private def printValDef(d: A.ValDef): Unit =
    d match {
      case A.ValDef(pat, exp, _, _, _) =>
        printPat(pat)
        printExp(exp)
    }

  private def printPat(pat: A.Pat): Unit =
    pat match {
      case A.WildPat() =>
      // nothing
      case A.VarPat(v) =>
        val fakeTypeScheme = ST.TypeSchema(0, List(), ST.PlainType(pat.typ))
        val typeSchemaString = printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
        val pp = "val " + v + ": " + typeSchemaString
        output(pp)
      case A.AndPat(p1, p2) =>
        printPat(p1)
        printPat(p2)

      case A.LiteralPat(_) =>
      //
      case A.TuplePat(pats) =>
        pats.foreach(printPat)
      case A.ListPat(pats) =>
        pats.foreach(printPat)
      case A.RecordPat(fields, _) =>
        fields.foreach { f => printPat(f.value) }

      case A.ConsPat(hPat, tPat) =>
        printPat(hPat)
        printPat(tPat)
      case A.EnumConstructorPat(_, _, pats) =>
        pats.foreach(printPat)

      case A.BinPat(elems) =>
        elems.foreach { elem =>
          printPat(elem.pat)
          elem.size.map(printExp)
        }

      case A.ERecordPat(_, fields) =>
        fields.foreach { f => printPat(f.value) }
    }

  private def printBody(body: A.Body): Unit = {
    printValDefsTypes(body.prelude)
    printValDef(body.main)
  }

  private def printExp(exp: A.Exp): Unit =
    exp match {
      case A.VarExp(_) =>
      // Nothing

      case A.LiteralExp(_) =>
      // Nothing
      case A.TupleExp(elems) =>
        elems.foreach(printExp)
      case A.ListExp(elems) =>
        elems.foreach(printExp)
      case A.BinExp(elems) =>
        elems.foreach { elem =>
          printExp(elem.expr)
          elem.size.foreach(printExp)
        }

      case A.UOpExp(_, exp) =>
        printExp(exp)
      case A.BinOpExp(_, exp1, exp2) =>
        printExp(exp1)
        printExp(exp2)

      case A.CaseExp(selector, branches) =>
        printExp(selector)
        branches.foreach { branch =>
          printPat(branch.pat)
          printBody(branch.body)
        }

      case A.IfExp(bodies) =>
        bodies.foreach(printBody)

      case A.Comprehension(template, qualifiers) =>
        qualifiers.foreach {
          case A.Filter(exp) =>
            printExp(exp)
          case A.Generator(pat, exp) =>
            printPat(pat)
            printExp(exp)
          case A.BGenerator(pat, exp) =>
            printPat(pat)
            printExp(exp)
        }
        printExp(template)
      case A.BComprehension(template, qualifiers) =>
        qualifiers.foreach {
          case A.Filter(exp) =>
            printExp(exp)
          case A.Generator(pat, exp) =>
            printPat(pat)
            printExp(exp)
          case A.BGenerator(pat, exp) =>
            printPat(pat)
            printExp(exp)
        }
        printExp(template)

      case A.SeqExp(e1, e2) =>
        printExp(e1)
        printExp(e2)
      case A.BlockExp(body) =>
        printBody(body)

      case A.RecordExp(fields) =>
        fields.foreach(f => printExp(f.value))
      case A.RecordSelectionExp(exp, _) =>
        printExp(exp)
      case A.RecordUpdateExp(exp, fields) =>
        printExp(exp)
        fields.foreach(f => printExp(f.value))

      case A.ERecordCreate(_, fields) =>
        fields.foreach(f => printExp(f.value))
      case A.ERecordUpdate(rec, _, fields) =>
        printExp(rec)
        fields.foreach(f => printExp(f.value))
      case A.ERecordIndex(_, _) =>
      // Nothing
      case A.ERecordSelect(rec, _, _) =>
        printExp(rec)

      case A.FnExp(clauses) =>
        clauses.foreach { clause =>
          clause.pats.foreach(printPat)
          printBody(clause.body)
        }
      case A.NamedFnExp(name, clauses) =>
        // TODO: the source location is wrong, but I don't think it matters.
        printPat(A.VarPat(name)(exp.typ, exp.sourceLocation))
        clauses.foreach { clause =>
          clause.pats.foreach(printPat)
          printBody(clause.body)
        }
      case A.AppExp(head, args) =>
        printExp(head)
        args.foreach(printExp)

      case A.ConsExp(h, t) =>
        printExp(h)
        printExp(t)
      case A.EnumConstructorExp(_, _, exprs) =>
        exprs.foreach(printExp)
    }

  private def output(pp: String): Unit = {
    sw match {
      case Some(w) =>
        w.append(pp)
        w.append("\n")
      case None =>
        println(pp)
    }
  }
}
