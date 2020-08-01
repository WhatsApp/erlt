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

package com.whatsapp.sterlang.forms

import com.whatsapp.sterlang.Pos
import com.whatsapp.sterlang.forms.Patterns._
import com.whatsapp.sterlang.forms.Guards._

object Exprs {

  case class Clause(p: Pos.SP, pats: List[Pattern], guards: List[Guard], body: List[Expr])
  case class IfClause(guards: List[Guard], body: List[Expr])

  sealed trait Expr { val p: Pos.SP }

  sealed trait Literal extends Expr { val p: Pos.SP }
  case class AtomLiteral(p: Pos.SP, atom: String) extends Literal
  case class CharLiteral(p: Pos.SP, ch: Char) extends Literal
  case class FloatLiteral(p: Pos.SP, fl: Double) extends Literal
  case class IntLiteral(p: Pos.SP, i: Int) extends Literal
  case class StringLiteral(p: Pos.SP, str: Option[String]) extends Literal

  case class Match(p: Pos.SP, pat: Pattern, arg: Expr) extends Expr
  case class Variable(p: Pos.SP, name: String) extends Expr
  case class Tuple(p: Pos.SP, elems: List[Expr]) extends Expr
  case class Nil(p: Pos.SP) extends Expr
  case class Cons(p: Pos.SP, hd: Expr, tl: Expr) extends Expr
  case class Bin(p: Pos.SP, elems: List[BinElement]) extends Expr
  case class BinaryOp(p: Pos.SP, op: String, exp1: Expr, exp2: Expr) extends Expr
  case class UnaryOp(p: Pos.SP, op: String, exp1: Expr) extends Expr
  case class RecordCreate(p: Pos.SP, recordName: String, fields: List[RecordField]) extends Expr
  case class RecordUpdate(p: Pos.SP, rec: Expr, recordName: String, fields: List[RecordField]) extends Expr
  case class RecordIndex(p: Pos.SP, recordName: String, fieldName: String) extends Expr
  case class RecordFieldAccess(p: Pos.SP, rec: Expr, recordName: String, fieldName: String) extends Expr
  case class MapCreate(p: Pos.SP, entries: List[Assoc]) extends Expr
  case class MapUpdate(p: Pos.SP, exp: Expr, entries: List[Assoc]) extends Expr
  case class Catch(p: Pos.SP, exp: Expr) extends Expr
  case class LocalCall(p: Pos.SP, fun: Expr, args: List[Expr]) extends Expr
  case class RemoteCall(p: Pos.SP, module: Expr, fun: Expr, args: List[Expr]) extends Expr
  case class LocalEnumCtr(p: Pos.SP, enum: String, ctr: String, args: List[Expr]) extends Expr
  case class RemoteEnumCtr(p: Pos.SP, module: String, enum: String, ctr: String, args: List[Expr]) extends Expr
  case class ListComprehension(p: Pos.SP, template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class BinaryComprehension(p: Pos.SP, template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class Block(p: Pos.SP, exprs: List[Expr]) extends Expr
  case class Case(p: Pos.SP, expr: Expr, clauses: List[Clause]) extends Expr
  case class If(p: Pos.SP, clauses: List[IfClause]) extends Expr
  case class Try(p: Pos.SP, body: List[Expr], clauses: List[Clause], catchClauses: List[Clause], after: List[Expr])
      extends Expr
  case class Receive(p: Pos.SP, clauses: List[Clause]) extends Expr
  case class ReceiveWithTimeout(p: Pos.SP, cl: List[Clause], timeout: Expr, default: List[Expr]) extends Expr
  case class LocalFun(p: Pos.SP, funName: String, arity: Int) extends Expr
  case class RemoteFun(p: Pos.SP, module: Expr, funName: Expr, arity: Expr) extends Expr
  case class Fun(p: Pos.SP, clauses: List[Clause]) extends Expr
  case class NamedFun(p: Pos.SP, funName: String, clauses: List[Clause]) extends Expr

  case class BinElement(expr: Expr, size: Option[Expr], typeSpecifiers: TypeSpecifiers)

  sealed trait TypeSpecifiers
  case object DefaultTypeSpecifier extends TypeSpecifiers
  case class TypeSpecifierList(specifiers: List[TypeSpecifier]) extends TypeSpecifiers
  sealed trait TypeSpecifier
  case class TypeSpecifierId(id: String) extends TypeSpecifier
  case class TypeSpecifierUnit(int: Int) extends TypeSpecifier

  case class RecordField(p: Pos.SP, fieldName: String, value: Expr)

  sealed trait Assoc { val p: Pos.SP }
  // X := Y - mandatory association
  case class AssocExact(p: Pos.SP, k: Expr, v: Expr) extends Assoc
  // X => Y - optional association
  case class OptAssoc(p: Pos.SP, k: Expr, v: Expr) extends Assoc

  sealed trait Qualifier
  sealed trait Generator extends Qualifier
  case class LGenerate(pat: Pattern, expr: Expr) extends Generator
  case class BGenerate(pat: Pattern, expr: Expr) extends Generator
  case class Filter(expr: Expr) extends Qualifier
}
