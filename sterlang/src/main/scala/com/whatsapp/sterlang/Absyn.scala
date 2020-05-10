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

object Absyn {
  type Type = Types.Type
  type RowType = Types.RowType
  type BinOp = Ast.BinOp
  type UOp = Ast.UOp

  case class Field[A](label: String, value: A)

  sealed trait Exp
  case class IfExp(exp1: Exp, exp2: Exp, exp3: Exp, tp: Type) extends Exp
  case class RecordUpdateExp(exp: Exp, tp1: Type, fields: List[Field[Exp]], tp: Type) extends Exp
  case class BinOpExp(binOp: BinOp, exp1: Exp, exp2: Exp, tp: Type) extends Exp
  case class UOpExp(uOp: UOp, exp: Exp, tp: Type) extends Exp
  case class AppExp(head: Exp, args: List[Exp], tp: Type) extends Exp
  case class SelExp(exp: Exp, tp: Type, label: String, tp2: Type) extends Exp
  case class BoolExp(bool: Boolean) extends Exp
  case class NumberExp(n: Int) extends Exp
  case class CharExp(c: String) extends Exp
  case class StringExp(s: String) extends Exp
  case object UnitExp extends Exp
  case class VarExp(v: String, tp: Type) extends Exp
  case class SeqExp(e1: Exp, e2: Exp) extends Exp
  case class ListExp(elems: List[Exp], tp: Type) extends Exp
  case class ConsExp(h: Exp, t: Exp, tp: Type) extends Exp
  case class BlockExp(body: Body) extends Exp
  case class RecordExp(fields: List[Field[Exp]], tp: Type) extends Exp
  case class EnumConExp(enumName: String, conName: String, args: List[Exp], tp: Type) extends Exp
  case class FnExp(clauses: List[Clause], typ: Type) extends Exp
  case class NamedFnExp(name: String, clauses: List[Clause], typ: Type) extends Exp
  case class TupleExp(elems: List[Exp], t: Type) extends Exp
  case class CaseExp(selector: Exp, branches: List[Branch], tp: Type) extends Exp

  case class ValDef(pat: TPat, exp: Exp, env: Env, depth: Int, vType: Type)
  case class Body(prelude: List[ValDef], main: ValDef, tp: Type)
  case class Fun(f: String, clauses: List[Clause], fType: Type)
  case class Clause(pats: List[TPat], body: Body)

  case class TPat(pat1: Pat, tp: Type)
  sealed trait Pat
  case object WildPat extends Pat
  case class VarPat(v: String) extends Pat
  case class BoolPat(v: Boolean) extends Pat
  case class NumberPat(i: Int) extends Pat
  case class StringPat(s: String) extends Pat
  case class TuplePat(pats: List[TPat]) extends Pat
  case class RecordPat(fields: List[Field[TPat]], open: Boolean) extends Pat
  case class AndPat(p1: TPat, p2: TPat) extends Pat
  case class EnumCtrPat(enumLabel: String, conLabel: String, pats: List[TPat]) extends Pat
  case class ListPat(pats: List[TPat]) extends Pat
  case class ConsPat(hPat: TPat, tPat: TPat) extends Pat

  case class Branch(pat: TPat, body: Body)
}
