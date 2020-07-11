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
import com.whatsapp.sterlang.etf.ETerm
import com.whatsapp.sterlang.forms.Patterns._
import com.whatsapp.sterlang.forms.Guards._

object Exprs {

  case class Clause(pats: List[Pattern], guards: List[Guard], body: List[Expr])

  sealed trait Expr

  sealed trait Literal extends Expr
  case class AtomLiteral(p: Pos.SP, atom: String) extends Literal
  case class CharLiteral(p: Pos.SP, ch: Char) extends Literal
  case class FloatLiteral(p: Pos.SP, fl: Double) extends Literal
  case class IntLiteral(p: Pos.SP, i: Int) extends Literal
  case class StringLiteral(p: Pos.SP, str: Option[String]) extends Literal

  case class Match(pat: Pattern, arg: Expr) extends Expr
  case class Variable(p: Pos.SP, name: String) extends Expr
  case class Tuple(p: Pos.SP, elems: List[Expr]) extends Expr
  case class Nil(p: Pos.SP) extends Expr
  case class Cons(p: Pos.SP, hd: Expr, tl: Expr) extends Expr
  case class Bin(elems: List[BinElement]) extends Expr
  case class BinaryOp(p: Pos.SP, op: String, exp1: Expr, exp2: Expr) extends Expr
  case class UnaryOp(p: Pos.SP, op: String, exp1: Expr) extends Expr
  case class RecordCreate(recordName: String, fields: List[RecordField]) extends Expr
  case class RecordUpdate(exp1: Expr, recordName: String, fields: List[RecordField]) extends Expr
  case class RecordIndex(recordName: String, fieldName: String) extends Expr
  case class RecordFieldAccess(exp: Expr, recordName: String, fieldName: String) extends Expr
  case class MapCreate(p: Pos.SP, entries: List[Assoc]) extends Expr
  case class MapUpdate(p: Pos.SP, exp: Expr, entries: List[Assoc]) extends Expr
  case class Catch(exp: Expr) extends Expr
  case class LocalCall(p: Pos.SP, fun: Expr, args: List[Expr]) extends Expr
  case class RemoteCall(module: Expr, fun: Expr, args: List[Expr]) extends Expr
  case class LocalEnumCtr(enum: String, ctr: String, args: List[Expr]) extends Expr
  case class RemoteEnumCtr(module: String, enum: String, ctr: String, args: List[Expr]) extends Expr
  case class ListComprehension(template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class BinaryComprehension(template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class Block(exprs: List[Expr]) extends Expr
  case class If(clauses: List[Clause]) extends Expr
  case class Case(expr: Expr, clauses: List[Clause]) extends Expr
  case class Try(body: List[Expr], clauses: List[Clause], catchClauses: List[Clause], after: List[Expr]) extends Expr
  case class Receive(clauses: List[Clause]) extends Expr
  case class ReceiveWithTimeout(cl: List[Clause], timeout: Expr, default: List[Expr]) extends Expr
  case class LocalFun(funName: String, arity: Int) extends Expr
  case class RemoteFun(module: Expr, funName: Expr, arity: Expr) extends Expr
  case class Fun(clauses: List[Clause]) extends Expr
  case class NamedFun(funName: String, clauses: List[Clause]) extends Expr

  // TODO - proper size
  case class BinElement(expr: Expr, size: ETerm, typeSpecifiers: TypeSpecifiers)

  sealed trait TypeSpecifiers
  case object DefaultTypeSpecifier extends TypeSpecifiers
  case class TypeSpecifierList(specifiers: List[TypeSpecifier]) extends TypeSpecifiers
  sealed trait TypeSpecifier
  case class TypeSpecifierId(id: String) extends TypeSpecifier
  case class TypeSpecifierUnit(int: Int) extends TypeSpecifier

  case class RecordField(fieldName: String, value: Expr)

  sealed trait Assoc
  // X := Y - mandatory association
  case class AssocExact(k: Expr, v: Expr) extends Assoc
  // X => Y - optional association
  case class OptAssoc(k: Expr, v: Expr) extends Assoc

  sealed trait Qualifier
  sealed trait Generator extends Qualifier
  case class LGenerate(pat: Pattern, expr: Expr) extends Generator
  case class BGenerate(pat: Pattern, expr: Expr) extends Generator
  case class Filter(expr: Expr) extends Qualifier
}
