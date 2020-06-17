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
    funs.foreach { f => printTypeScheme(env, f.f) }
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
          clause.pats.foreach(printTPat)
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
        printTPat(pat)
        printExp(exp)
    }

  private def printTPat(tPat: A.TPat): Unit =
    tPat.pat1 match {
      case A.WildPat =>
      // nothing
      case A.VarPat(v) =>
        val fakeTypeScheme = ST.TypeSchema(0, List(), ST.PlainType(tPat.tp))
        val typeSchemaString = printer.typeSchema(fakeTypeScheme, TypePrinter2.Types)
        val pp = "val " + v + ": " + typeSchemaString
        output(pp)
      case A.TuplePat(pats) =>
        pats.foreach(printTPat)
      case A.BoolPat(_) | A.NumberPat(_) | A.StringPat(_) =>
      //
      case A.RecordPat(fields, _) =>
        fields.foreach { f => printTPat(f.value) }
      case A.AndPat(p1, p2) =>
        printTPat(p1)
        printTPat(p2)
      case A.EnumCtrPat(_, _, pats) =>
        pats.foreach(printTPat)
      case A.ListPat(pats) =>
        pats.foreach(printTPat)
      case A.ConsPat(hPat, tPat) =>
        printTPat(hPat)
        printTPat(tPat)
    }

  private def printBody(body: A.Body): Unit = {
    printValDefsTypes(body.prelude)
    printValDef(body.main)
  }

  private def printExp(exp: A.Exp): Unit =
    exp match {
      case A.IfExp(exp1, exp2, exp3, _) =>
        printExp(exp1)
        printExp(exp2)
        printExp(exp3)
      case A.RecordUpdateExp(exp, _, fields, _) =>
        printExp(exp)
        fields.foreach(f => printExp(f.value))
      case A.BinOpExp(binOp, exp1, exp2, tp) =>
        printExp(exp1)
        printExp(exp2)
      case A.UOpExp(uOp, exp, tp) =>
        printExp(exp)
      case A.AppExp(head, args, tp) =>
        printExp(head)
        args.foreach(printExp)
      case A.SelExp(exp, tp, label, tp2) =>
        printExp(exp)
      case A.BoolExp(bool) =>
      // Nothing
      case A.NumberExp(n) =>
      // Nothing
      case A.CharExp(n) =>
      // Nothing
      case A.StringExp(s) =>
      // Nothing
      case A.UnitExp =>
      // Nothing
      case A.VarExp(v, tp) =>
      // Nothing
      case A.SeqExp(e1, e2) =>
        printExp(e1)
        printExp(e2)
      case A.ListExp(elems, tp) =>
        elems.foreach(printExp)
      case A.ConsExp(h, t, tp) =>
        printExp(h)
        printExp(t)
      case A.RecordExp(fields, tp) =>
        fields.foreach(f => printExp(f.value))
      case A.EnumConExp(enumName, conName, exprs, tp) =>
        exprs.foreach(printExp)
      case A.FnExp(clauses, typ) =>
        clauses.foreach { clause =>
          clause.pats.foreach(printTPat)
          printBody(clause.body)
        }
      case A.NamedFnExp(name, clauses, typ) =>
        printTPat(A.TPat(A.VarPat(name), typ))
        clauses.foreach { clause =>
          clause.pats.foreach(printTPat)
          printBody(clause.body)
        }
      case A.CaseExp(selector, branches, tp) =>
        printExp(selector)
        branches.foreach { branch =>
          printTPat(branch.pat)
          printBody(branch.body)
        }
      case A.TupleExp(elems, t) =>
        elems.foreach(printExp)
      case A.BlockExp(body) =>
        printBody(body)
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
