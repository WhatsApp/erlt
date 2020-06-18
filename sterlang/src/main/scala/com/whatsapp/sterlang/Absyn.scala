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

import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang.Values.Value

object Absyn {
  type Type = Types.Type
  type RowType = Types.RowType
  type BinOp = Ast.BinOp
  type UOp = Ast.UOp

  /** A node in the intermediate language abstract syntax tree.
    *
    * The intermediate language is similar to the surface language,
    * but every expression is annotated with its type.
    */
  trait Node extends HasSourceLocation

  case class Field[A](label: String, value: A)

  sealed trait Exp extends Node {

    /** The type of the expression. */
    val typ: Type
  }

  // TODO: variableName, enumName, conName etc. should all be classes and have a source location.

  case class VarExp(name: String)(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // Begin TODO: these can be elaborated to constructor applications
  case class LiteralExp(value: Value)(val sourceLocation: Pos.P) extends Exp {
    override val typ: Type = value.typ
  }
  case class TupleExp(elems: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class ListExp(elems: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // End TODO

  case class UOpExp(uOp: UOp, exp: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class BinOpExp(binOp: BinOp, exp1: Exp, exp2: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // TODO: this can be unified with [CaseExp]
  case class IfExp(exp1: Exp, exp2: Exp, exp3: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class CaseExp(selector: Exp, branches: List[Branch])(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // TODO: is [[SeqExp]] significantly different than [[BlockExp]]?
  case class SeqExp(e1: Exp, e2: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class BlockExp(body: Body)(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // TODO: can [FnExp] be unified with [NamedFnExp]?
  case class FnExp(clauses: List[Clause])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class NamedFnExp(name: String, clauses: List[Clause])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class AppExp(head: Exp, args: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // TODO: should this have a RowType?
  case class RecordExp(fields: List[Field[Exp]])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // TODO: what's tp
  case class SelExp(exp: Exp, tp: Type, label: String)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // TODO: what's tp1
  case class RecordUpdateExp(exp: Exp, tp1: Type, fields: List[Field[Exp]])(val typ: Type, val sourceLocation: Pos.P)
      extends Exp

  // TODO: how is ConsExp different from EnumConsExp?
  case class ConsExp(h: Exp, t: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class EnumConExp(enumName: String, conName: String, args: List[Exp])(val typ: Type, val sourceLocation: Pos.P)
      extends Exp

  case class ValDef(pat: Pat, exp: Exp, env: Env, depth: Int, vType: Type)
  case class Body(prelude: List[ValDef], main: ValDef, tp: Type)
  case class Fun(f: String, clauses: List[Clause], fType: Type)
  case class Clause(pats: List[Pat], body: Body)

  sealed trait Pat extends Node {

    /** The type of values this pattern can match. */
    val typ: Type
  }

  case class WildPat()(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class VarPat(v: String)(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class AndPat(p1: Pat, p2: Pat)(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class LiteralPat(value: Value)(val sourceLocation: Pos.P) extends Pat {
    override val typ: Type = value.typ
  }
  case class TuplePat(pats: List[Pat])(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class ListPat(pats: List[Pat])(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class RecordPat(fields: List[Field[Pat]], open: Boolean)(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class ConsPat(hPat: Pat, tPat: Pat)(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class EnumCtrPat(enumLabel: String, conLabel: String, pats: List[Pat])(val typ: Type, val sourceLocation: Pos.P)
      extends Pat

  case class Branch(pat: Pat, body: Body)
}
