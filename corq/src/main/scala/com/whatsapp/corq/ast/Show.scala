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

import com.whatsapp.corq.ast.Types._
import erlang.CErl._
import erlang.Data._

object Show {
  def show(tp: Type): String =
    tp match {
      case CValuesType(tys) =>
        s"values <${tys.map(show).mkString(", ")}>"
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

  def showExprs(exprs: List[CErl]): String = exprs map show mkString ","
  def showDatas(datas: List[EObject]): String = datas map show mkString ","

  def show(data: EObject): String =
    data match {
      case EDouble(_) | ELong(_)           => data.toString
      case EAtom(atom)                     => s"'$atom'"
      case EBitStr(bin, pad_bits)          => "<<...>>"
      case EExternalFun(RemoteId(m, f, a)) => s"$m:$f/$a"
      case EList(elems, _lastTail) =>
        s"[${showDatas(elems)}]"
      case EString(str)  => s""""$str""""
      case ETuple(elems) => s"{${showDatas(elems)}}"
      // never hit
      // case EMap(entries) =>
      //   val inside = entries.foldLeft("")((memo, a) => s"$memo ${show(a._1)} => ${show(a._2)}")
      //   s"#{$inside}"
      // case EPid(node, id, serial, creation) => "pid"
      // case EPort(node, id, creation) => "port"
      // case ERef(node, creation, ids) => "ref"
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $data")
      // $COVERAGE-ON$
    }

  def show(e: CErl): String =
    e match {
      case CApply(_, f, args) =>
        s"""$f(${showExprs(args)})"""
      case CCall(_, m, f, args) =>
        s"""${show(m)}:${show(f)}(${showExprs(args)})"""
      case CCase(_, _arg, _clauses)                          => "case ..."
      case CCatch(_, _body)                                  => "catch ..."
      case c @ CClause(_, pats, _guard, _body)               => showExprs(pats)
      case CFun(_, vars, _body)                              => s"""fun (${showExprs(vars)})"""
      case CLet(_, _vars, _arg, _body)                       => "let ..."
      case CLiteral(_, value)                                => show(value)
      case CTry(_, _arg, _bodyVars, _body, _evars, _handler) => "CTry"
      case CVar(_, name) =>
        name match {
          case VarNameInt(i)                   => s"var$i"
          case VarNameAtom(atom)               => s"$atom"
          case VarNameAtomInt(Id(name, arity)) => s"$name/$arity"
        }
      case CAlias(anno, cvar, pat) => show(cvar)
      case CCons(anno, h, t) =>
        s"[${show(h)} | ${show(t)}]"
      // not used yet
      case CTuple(_, elems) =>
        elems.map(show).mkString("{", ", ", "}")
      // case CBinary(anno, segments) =>"CBinary"
      // case CBitstr(anno, value, size, unit, typ, flags) =>"CBitstr"
      // case CLetRec(anno, defs, body) => "CLetRec"
      // case CMap(anno, arg, es, isPat) => "CMap"
      // case CMapPair(anno, op, key, cVal) => "CMapPair"
      // case CModule(anno, name, exports, attrs, defs) => "CModule"
      // case CPrimOp(anno, name, args) => "CPrimOp"
      // case CReceive(anno, clauses, timeout, action) => "CReceive"
      // case CSeq(anno, arg, body) => "CSeq"
      case CValues(anno, es) =>
        es map show mkString ("ValueList: <", ", ", ">")
    }
}
