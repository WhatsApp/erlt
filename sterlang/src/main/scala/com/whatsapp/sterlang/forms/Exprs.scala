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

import com.whatsapp.sterlang.Doc
import com.whatsapp.sterlang.forms.Patterns._

object Exprs {

  case class Clause(r: Doc.Range, pats: List[Pattern], guards: List[Guard], body: List[Expr])
  case class IfClause(guards: List[Guard], body: List[Expr])

  sealed trait Expr { val r: Doc.Range }

  sealed trait Literal extends Expr { val r: Doc.Range }
  case class AtomLiteral(r: Doc.Range, atom: String) extends Literal
  case class CharLiteral(r: Doc.Range, ch: Char) extends Literal
  case class FloatLiteral(r: Doc.Range, fl: Double) extends Literal
  case class IntLiteral(r: Doc.Range, i: Int) extends Literal
  case class StringLiteral(r: Doc.Range, str: String) extends Literal

  case class Match(r: Doc.Range, pat: Pattern, arg: Expr) extends Expr
  case class Variable(r: Doc.Range, name: String) extends Expr
  case class Tuple(r: Doc.Range, elems: List[Expr]) extends Expr
  case class Nil(r: Doc.Range) extends Expr
  case class Cons(r: Doc.Range, hd: Expr, tl: Expr) extends Expr
  case class Bin(r: Doc.Range, elems: List[BinElement]) extends Expr
  case class BinaryOp(r: Doc.Range, op: String, exp1: Expr, exp2: Expr) extends Expr
  case class UnaryOp(r: Doc.Range, op: String, exp1: Expr) extends Expr
  case class LocalStructCreate(r: Doc.Range, structName: String, fields: List[StructField]) extends Expr
  case class RemoteStructCreate(r: Doc.Range, module: String, structName: String, fields: List[StructField])
      extends Expr
  case class LocalStructUpdate(r: Doc.Range, exp: Expr, structName: String, fields: List[StructField]) extends Expr
  case class RemoteStructUpdate(r: Doc.Range, exp: Expr, module: String, structName: String, fields: List[StructField])
      extends Expr
  case class LocalStructSelect(r: Doc.Range, struct: Expr, structName: String, fieldName: String) extends Expr
  case class RemoteStructSelect(r: Doc.Range, struct: Expr, module: String, structName: String, fieldName: String)
      extends Expr
  case class ShapeSelect(r: Doc.Range, exp: Expr, fieldName: String) extends Expr
  case class ShapeCreate(r: Doc.Range, entries: List[ShapeField]) extends Expr
  case class ShapeUpdate(r: Doc.Range, exp: Expr, entries: List[ShapeField]) extends Expr
  case class LocalCall(r: Doc.Range, fun: Expr, args: List[Expr]) extends Expr
  case class RemoteCall(r: Doc.Range, module: Expr, fun: Expr, args: List[Expr]) extends Expr
  case class LocalEnum(r: Doc.Range, enum: String, ctr: String, args: List[Expr]) extends Expr
  case class RemoteEnum(r: Doc.Range, module: String, enum: String, ctr: String, args: List[Expr]) extends Expr
  case class ListComprehension(r: Doc.Range, template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class BinaryComprehension(r: Doc.Range, template: Expr, qualifiers: List[Qualifier]) extends Expr
  case class Block(r: Doc.Range, exprs: List[Expr]) extends Expr
  case class Case(r: Doc.Range, expr: Expr, clauses: List[Clause]) extends Expr
  case class If(r: Doc.Range, clauses: List[IfClause]) extends Expr
  case class Try(r: Doc.Range, body: List[Expr], clauses: List[Clause], catchClauses: List[Clause], after: List[Expr])
      extends Expr
  case class Receive(r: Doc.Range, clauses: List[Clause]) extends Expr
  case class ReceiveWithTimeout(r: Doc.Range, cl: List[Clause], timeout: Expr, default: List[Expr]) extends Expr
  case class LocalFun(r: Doc.Range, funName: String, arity: Int) extends Expr
  case class RemoteFun(r: Doc.Range, module: Expr, funName: Expr, arity: Expr) extends Expr
  case class Fun(r: Doc.Range, clauses: List[Clause]) extends Expr
  case class NamedFun(r: Doc.Range, funName: String, clauses: List[Clause]) extends Expr

  case class Guard(elems: List[Expr])

  case class BinElement(expr: Expr, size: Option[Expr], typeSpecifiers: TypeSpecifiers)

  sealed trait TypeSpecifiers
  case object DefaultTypeSpecifier extends TypeSpecifiers
  case class TypeSpecifierList(specifiers: List[TypeSpecifier]) extends TypeSpecifiers
  case class TypeSpecifier(id: String)

  case class StructField(r: Doc.Range, fieldName: String, value: Expr)
  case class ShapeField(r: Doc.Range, k: Expr, v: Expr)

  sealed trait Qualifier
  sealed trait Generator extends Qualifier
  case class LGenerate(pat: Pattern, expr: Expr) extends Generator
  case class BGenerate(pat: Pattern, expr: Expr) extends Generator
  case class Filter(expr: Expr) extends Qualifier
}
