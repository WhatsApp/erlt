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

package com.whatsapp.corq.ast

import com.whatsapp.corq.ast.Exprs._
import com.whatsapp.corq.ast.Types._
import erlang.CErl._

object Show {
  def show(tp: Type): String =
    tp match {
      case AtomLitType(atom) =>
        s"'$atom'"
      case FunType(argTys, resTy) =>
        s"""fun((${argTys.map(show).mkString(", ")}) -> ${show(resTy)})"""
      case TupleType(elems) =>
        elems.map(show).mkString("{", ", ", "}")
      case ListType(elemType) =>
        s"""[${show(elemType)}]"""
      case NilType =>
        "[]"
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
      case BinaryType =>
        "binary()"
    }

  def show(e: CErl): String =
    e match {
      case CAlias(anno, v, pat)                         => ""
      case CApply(anno, op, args)                       => ""
      case CBinary(anno, segments)                      => ""
      case CBitstr(anno, value, size, unit, typ, flags) => ""
      case CCall(anno, module, name, args)              => ""
      case CCase(anno, arg, clauses)                    => ""
      case CCatch(anno, body)                           => ""
      case CClause(anno, pats, guard, body)             => ""
      case CCons(anno, hd, tl)                          => ""
      case CFun(anno, vars, body)                       => ""
      case CLet(anno, vars, arg, body)                  => ""
      case CLetRec(anno, defs, body)                    => ""
      case CLiteral(anno, value)                        => ""
      case CMap(anno, arg, es, isPat)                   => ""
      case CMapPair(anno, op, key, cVal)                => ""
      case CModule(anno, name, exports, attrs, defs)    => ""
      case CPrimOp(anno, name, args)                    => ""
      case CReceive(anno, clauses, timeout, action)     => ""
      case CSeq(anno, arg, body)                        => ""
      case CTry(anno, arg, vars, body, evars, handler)  => ""
      case CTuple(anno, es)                             => ""
      case CValues(anno, es)                            => ""
      case CVar(anno, name)                             => ""
      case C___XXX(raw)                                 => ""
    }
}
