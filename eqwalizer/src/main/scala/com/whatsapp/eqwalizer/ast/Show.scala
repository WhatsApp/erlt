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

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Types._

object Show {
  def show(tp: Type): String =
    tp match {
      case AtomLitType(atom) =>
        s"'$atom'"
      case FunType(argTys, resTy) =>
        s"""fun((${argTys.map(show).mkString(", ")}) -> ${show(resTy)})"""
      case TupleType(elems) =>
        elems.map(show).mkString("{", ", ", "}")
      case UnionType(elemTys) =>
        elemTys.map(show).mkString(" | ")
      case LocalType(Id(t, _), args) =>
        s"""$t(${args.map(show).mkString(", ")})"""
      case RemoteType(RemoteId(m, t, _), args) =>
        s"""$m:$t(${args.map(show).mkString(", ")})"""
      case VarType(name) =>
        name
      case AnyType =>
        "term()"
      case AtomType =>
        "atom()"
      case NoneType =>
        "none()"
      case NumberType =>
        "number()"
      case PidType =>
        "pid()"
      case PortType =>
        "port()"
      case ReferenceType =>
        "reference()"
    }

  def show(e: Expr): String = e match {
    case Exprs.Var(n) =>
      n
    case Exprs.AtomLit(atom) =>
      s"'$atom'"
    case Exprs.NumberLit() =>
      "number_expr"
    case Exprs.Block(_) =>
      "block_expr"
    case Exprs.LocalCall(Id(f, _), args) =>
      s"""$f(${args.map(show).mkString(", ")})"""
    case Exprs.RemoteCall(RemoteId(m, t, _), args) =>
      s"""$m:$t(${args.map(show).mkString(", ")})"""
    case Exprs.LocalFun(id) =>
      id.toString
    case Exprs.RemoteFun(id) =>
      id.toString
    case Exprs.Tuple(elems) =>
      elems.map(show).mkString("{", ", ", "}")
    case Exprs.Match(_, _) =>
      "match_expr"
    case Exprs.Case(_, _) =>
      "case_expr"
  }
}
