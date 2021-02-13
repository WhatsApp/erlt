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

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Pats._

object Vars {
  private def patVars(pat: Pat): Set[String] =
    pat match {
      case PatWild() =>
        Set.empty
      case PatMatch(pat, arg) =>
        patVars(pat) ++ patVars(arg)
      case PatTuple(elems) =>
        elems.flatMap(patVars).toSet
      case PatNil() =>
        Set.empty
      case PatCons(h, t) =>
        patVars(h) ++ patVars(t)
      case PatNumber() =>
        Set.empty
      case PatAtom(_) =>
        Set.empty
      case PatVar(n) =>
        Set(n)
      case PatUnOp(_, arg) =>
        patVars(arg)
      case PatBinOp(_, arg1, arg2) =>
        patVars(arg1) ++ patVars(arg2)
      case PatBinary(elems) =>
        elems.flatMap(binaryElemVars).toSet
      case PatRecord(_, fields) =>
        fields.flatMap(f => patVars(f.pat)).toSet
      case PatRecordIndex(_, _) =>
        Set.empty
      case PatMap(kvs) =>
        kvs.flatMap(kv => patVars(kv._1) ++ patVars(kv._2)).toSet
    }

  private def binaryElemVars(elem: PatBinaryElem): Set[String] = {
    val sizeVars: Set[String] = elem.size match {
      case Pats.PatBinSizeConst => Set.empty
      case PatBinSizeVar(v)     => Set(v.n)
    }
    sizeVars ++ patVars(elem.pat)
  }

  private def binaryElemVars(elem: BinaryElem): Set[String] = {
    val sizeVars: Set[String] = elem.size.map(exprVars).getOrElse(Set.empty)
    sizeVars ++ exprVars(elem.expr)
  }

  def blockVars(block: List[Expr]): Set[String] =
    block.map(exprVars).reduce(_ ++ _)

  private def exprVars(expr: Expr): Set[String] = expr match {
    case Var(_) =>
      Set.empty
    case AtomLit(_) =>
      Set.empty
    case NumberLit() =>
      Set.empty
    case Block(exprs) =>
      blockVars(exprs)
    case Match(pat, expr) =>
      patVars(pat) ++ exprVars(expr)
    case Tuple(elems) =>
      elems.flatMap(exprVars).toSet
    case NilLit() =>
      Set.empty
    case Cons(h, t) =>
      exprVars(h) ++ exprVars(t)
    case Case(expr, clauses) =>
      exprVars(expr) ++ clausesVars(clauses)
    case If(clauses) =>
      clausesVars(clauses)
    case LocalCall(id, args) =>
      args.flatMap(exprVars).toSet
    case RemoteCall(id, args) =>
      args.flatMap(exprVars).toSet
    case LocalFun(id) =>
      Set.empty
    case RemoteFun(id) =>
      Set.empty
    case UnOp(op, arg) =>
      exprVars(arg)
    case BinOp(op, arg1, arg2) =>
      exprVars(arg1) ++ exprVars(arg2)
    case Binary(elems) =>
      elems.flatMap(binaryElemVars).toSet
    case Catch(e) =>
      exprVars(e)
    case TryCatchExpr(tryBody, catchClauses, after) =>
      Set.empty
    case TryOfCatchExpr(tryBody, tryClauses, catchClauses, after) =>
      Set.empty
    case Receive(clauses) =>
      clausesVars(clauses)
    case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
      clausesVars(clauses) intersect blockVars(timeoutBlock)
    case LComprehension(_, _) =>
      Set.empty
    case BComprehension(_, _) =>
      Set.empty
    case RecordCreate(_, fields) =>
      fields.flatMap(fieldVars).toSet
    case RecordUpdate(e, recName, fields) =>
      exprVars(e) ++ fields.flatMap(fieldVars)
    case RecordSelect(e, _, _) =>
      exprVars(e)
    case RecordIndex(_, _) =>
      Set.empty
    case MapCreate(kvs) =>
      kvs.flatMap(kv => List(kv._1, kv._2)).flatMap(exprVars).toSet
    case GenMapUpdate(m, kvs) =>
      exprVars(m) ++ kvs.flatMap(kv => List(kv._1, kv._2)).flatMap(exprVars)
    case ReqMapUpdate(m, avs) =>
      exprVars(m) ++ avs.map(_._2).flatMap(exprVars)
    case Fun(clauses) =>
      clausesVars(clauses)
    case FunCall(expr, args) =>
      exprVars(expr) ++ args.flatMap(exprVars)
  }

  private def fieldVars(recordField: RecordField): Set[String] =
    exprVars(recordField.value)

  def clausesVars(clauses: List[Clause]): Set[String] =
    clauses.map(clauseVars).reduce(_ intersect _)

  def clausesAndBlockVars(clauses: List[Clause], block: List[Expr]): Set[String] =
    (blockVars(block) :: clauses.map(clauseVars)).reduce(_ intersect _)

  def clauseVars(clause: Clause): Set[String] =
    clause.pats.flatMap(patVars).toSet ++ blockVars(clause.body)
}
