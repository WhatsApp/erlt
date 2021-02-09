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

package com.whatsapp.coralizer.ast

import com.whatsapp.coralizer.ast.BinarySpecifiers.Specifier
import com.whatsapp.coralizer.ast.Guards.Guard
import com.whatsapp.coralizer.ast.Pats.Pat

object Exprs {
  sealed trait Expr { val l: Int }
  case class Var(n: String)(val l: Int) extends Expr
  case class AtomLit(s: String)(val l: Int) extends Expr
  case class NumberLit()(val l: Int) extends Expr

  case class Block(exprs: List[Expr])(val l: Int) extends Expr
  case class Match(pat: Pat, expr: Expr)(val l: Int) extends Expr

  case class Tuple(elems: List[Expr])(val l: Int) extends Expr

  case class NilLit()(val l: Int) extends Expr
  case class Cons(h: Expr, t: Expr)(val l: Int) extends Expr

  case class Case(expr: Expr, clauses: List[Clause])(val l: Int) extends Expr
  case class If(clauses: List[Clause])(val l: Int) extends Expr

  case class LocalCall(id: Id, args: List[Expr])(val l: Int) extends Expr
  case class RemoteCall(id: RemoteId, args: List[Expr])(val l: Int) extends Expr
  case class LocalFun(id: Id)(val l: Int) extends Expr
  case class RemoteFun(id: RemoteId)(val l: Int) extends Expr

  case class UnOp(op: String, arg: Expr)(val l: Int) extends Expr
  case class BinOp(op: String, arg1: Expr, arg2: Expr)(val l: Int) extends Expr

  case class Binary(elems: List[BinaryElem])(val l: Int) extends Expr
  case class Catch(expr: Expr)(val l: Int) extends Expr
  case class TryCatchExpr(
      tryBody: List[Expr],
      catchClauses: List[Clause],
      after: Option[List[Expr]]
  )(val l: Int)
      extends Expr
  case class TryOfCatchExpr(
      tryBody: List[Expr],
      tryClauses: List[Clause],
      catchClauses: List[Clause],
      after: Option[List[Expr]]
  )(val l: Int)
      extends Expr

  case class Clause(pats: List[Pat], guards: List[Guard], body: List[Expr])(
      val l: Int
  )
  case class BinaryElem(expr: Expr, size: Option[Expr], specifier: Specifier)(
      val l: Int
  )
}
