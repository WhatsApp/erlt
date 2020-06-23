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

package erlang.forms

import erlang.Data.EObject
import erlang.forms.AbstractGuard.Guard
import erlang.forms.AbstractPattern.Pattern

object AbstractExpr {

  case class AF_Clause(pats: List[Pattern], guards: List[Guard], body: List[AbstractExpr])

  sealed trait AbstractExpr

  sealed trait AF_Literal extends AbstractExpr
  case class AF_LiteralAtom(atom: String) extends AF_Literal
  case class AF_LiteralCharacter(ch: Char) extends AF_Literal
  case class AF_LiteralFloat(fl: Double) extends AF_Literal
  case class AF_LiteralInteger(i: Int) extends AF_Literal
  case class AF_LiteralString(str: String) extends AF_Literal
  case class AF_LiteralStringList() extends AF_Literal

  case class AF_Match(pat: Pattern, arg: AbstractExpr) extends AbstractExpr
  case class AF_Variable(name: String) extends AbstractExpr
  case class AF_Tuple(elems: List[AbstractExpr]) extends AbstractExpr
  case object AF_Nil extends AbstractExpr
  case class AF_Cons(hd: AbstractExpr, tl: AbstractExpr) extends AbstractExpr
  case class AF_Bin(elems: List[AF_BinElement]) extends AbstractExpr
  case class AF_BinaryOp(op: String, exp1: AbstractExpr, exp2: AbstractExpr) extends AbstractExpr
  case class AF_UnaryOp(op: String, exp1: AbstractExpr) extends AbstractExpr
  case class AF_RecordCreation(recordName: String, fields: List[AF_RecordField]) extends AbstractExpr
  case class AF_RecordUpdate(exp1: AbstractExpr, recordName: String, fields: List[AF_RecordField]) extends AbstractExpr
  case class AF_RecordIndex(recordName: String, fieldName: String) extends AbstractExpr
  case class AF_RecordFieldAccess(exp: AbstractExpr, recordName: String, fieldName: String) extends AbstractExpr
  case class AF_MapCreation(entries: List[AF_Assoc]) extends AbstractExpr
  case class AF_MapUpdate(exp: AbstractExpr, entries: List[AF_Assoc]) extends AbstractExpr
  case class AF_Catch(exp: AbstractExpr) extends AbstractExpr
  case class AF_LocalCall(fun: AbstractExpr, args: List[AbstractExpr]) extends AbstractExpr
  case class AF_RemoteCall(module: AbstractExpr, fun: AbstractExpr, args: List[AbstractExpr]) extends AbstractExpr
  case class AF_ListComprehension(template: AbstractExpr, qualifiers: List[AF_Qualifier]) extends AbstractExpr
  case class AF_BinaryComprehension(template: AbstractExpr, qualifiers: List[AF_Qualifier]) extends AbstractExpr
  case class AF_Block(exprs: List[AbstractExpr]) extends AbstractExpr
  case class AF_If(clauses: List[AF_Clause]) extends AbstractExpr
  case class AF_Case(expr: AbstractExpr, clauses: List[AF_Clause]) extends AbstractExpr
  case class AF_Try(body: List[AbstractExpr], cl1: List[AF_Clause], cl2: List[AF_Clause], extra: List[AbstractExpr])
      extends AbstractExpr
  case class AF_Receive(cl: List[AF_Clause]) extends AbstractExpr
  case class AF_ReceiveWithTimeout(cl: List[AF_Clause], timeout: AbstractExpr, default: List[AbstractExpr])
      extends AbstractExpr
  case class AF_LocalFun(funName: String, arity: Int) extends AbstractExpr
  // Static
  case class AF_RemoteFun(module: String, funName: String, arity: Int) extends AbstractExpr
  // Dynamic - TODO
  case class AF_RemoteFunDynamic(module: EObject, funName: EObject, arity: EObject) extends AbstractExpr
  case class AF_Fun(clauses: List[AF_Clause]) extends AbstractExpr
  case class AF_NamedFun(funName: String, clauses: List[AF_Clause]) extends AbstractExpr

  // TODO - proper size
  case class AF_BinElement(expr: AbstractExpr, size: EObject, typeSpecifiers: TypeSpecifiers)

  sealed trait TypeSpecifiers
  case object DefaultTypeSpecifier extends TypeSpecifiers
  case class TypeSpecifierList(specifiers: List[TypeSpecifier]) extends TypeSpecifiers
  sealed trait TypeSpecifier
  case class TypeSpecifierId(id: String) extends TypeSpecifier
  case class TypeSpecifierUnit(int: Int) extends TypeSpecifier

  case class AF_RecordField(fieldName: String, value: AbstractExpr)

  sealed trait AF_Assoc
  // X := Y - mandatory association
  case class AF_FieldExact(k: AbstractExpr, v: AbstractExpr) extends AF_Assoc
  // X => Y - optional association
  case class AF_FieldAssoc(k: AbstractExpr, v: AbstractExpr) extends AF_Assoc

  sealed trait AF_Qualifier
  sealed trait AF_Generator extends AF_Qualifier
  case class AF_Generate(pat: Pattern, expr: AbstractExpr) extends AF_Generator
  case class AF_BGenerate(pat: Pattern, expr: AbstractExpr) extends AF_Generator
  case class AF_Filter(expr: AbstractExpr) extends AF_Qualifier
}
