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

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.tc.Check
import com.whatsapp.eqwalizer.ast.Exprs.Fun
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.TypeMismatch

object Subtype {
  def subType(t1: Type, t2: Type): Boolean = {
    (t1, t2) match {
      case (_, _) if t1 == t2   => true
      case (_, AnyType)         => true
      case (NoneType, _)        => true
      case (UnionType(tys1), _) => tys1.forall(subType(_, t2))
      case (_, UnionType(tys2)) => tys2.exists(subType(t1, _))

      case (AtomLitType(_), AtomType) => true

      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(subType)
      case (NilType, ListType(_)) =>
        true
      case (ListType(et1), ListType(et2)) =>
        subType(et1, et2)
      case (FunType(args1, res1), FunType(args2, res2)) if args1.size == args2.size =>
        subType(res1, res2) && args2.lazyZip(args1).forall(subType)
      case (FoonType(clauses, module, env), FunType(argTys, resTy)) =>
        val check = new Check(module)
        try {
          clauses.foreach(check.checkClause(_, argTys, resTy, env, Set.empty))
          true
        }
        catch {
          case _: TypeMismatch => false
        }

      case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
        subType(kT1, kT2) && subType(vT1, vT2)
      case (ShapeMap(props), DictMap(kT, vT)) =>
        val shapeDomain = UnionType(props.map(prop => AtomLitType(prop.key)))
        val shapeCodomain = UnionType(props.map(_.tp))
        subType(shapeDomain, kT) && subType(shapeCodomain, vT)
      case (ShapeMap(props1), ShapeMap(props2)) =>
        val keys1 = props1.map(_.key).toSet
        val keys2 = props2.map(_.key).toSet
        if (keys1 != keys2) return false
        val reqKeys1 = props1.collect { case ReqProp(k, _) => k }.toSet
        val reqKeys2 = props2.collect { case ReqProp(k, _) => k }.toSet
        if (!reqKeys2.subsetOf(reqKeys1)) return false
        val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
        for (prop1 <- props1) {
          val t1 = prop1.tp
          val t2 = kvs2(prop1.key)
          if (!subType(t1, t2)) return false
        }
        true

      case _ =>
        false
    }
  }

  def eqv(t1: Type, t2: Type): Boolean =
    subType(t1, t2) && subType(t2, t1)

  def join(t1: Type, t2: Type): Type =
    if (subType(t1, t2)) t2
    else if (subType(t2, t1)) t1
    else UnionType(List(t1, t2))

  def meet(t1: Type, t2: Type): Type =
    if (subType(t1, t2)) t1
    else if (subType(t2, t1)) t2
    else
      (t1, t2) match {
        case (UnionType(ty1s), _) => UnionType(ty1s.map(meet(_, t2)))
        case (_, UnionType(ty2s)) => UnionType(ty2s.map(meet(t1, _)))

        case (TupleType(elems1), TupleType(elems2)) if elems1.size == elems2.size =>
          TupleType(elems1.lazyZip(elems2).map(meet))
        case (ListType(et1), ListType(et2)) =>
          ListType(meet(et1, et2))
        case (FunType(args1, res1), FunType(args2, res2)) if args1.size == args2.size =>
          FunType(args1.lazyZip(args2).map(join), meet(res1, res2))

        case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
          DictMap(meet(kT1, kT2), meet(vT1, vT2))
        case (ShapeMap(props1), ShapeMap(props2)) =>
          val keys1 = props1.map(_.key).toSet
          val keys2 = props2.map(_.key).toSet
          if (keys1 != keys2) return NoneType
          val reqKeys1 = props1.collect { case ReqProp(k, _) => k }.toSet
          val reqKeys2 = props1.collect { case ReqProp(k, _) => k }.toSet
          val allReqKeys = reqKeys1.union(reqKeys2)
          val allOptKeys = keys1.diff(allReqKeys)
          val kvs1 = props1.map(prop => prop.key -> prop.tp).toMap
          val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
          val reqProps = allReqKeys.toList.sorted.map(k => ReqProp(k, meet(kvs1(k), kvs2(k))))
          val optProps = allOptKeys.toList.sorted.map(k => OptProp(k, meet(kvs1(k), kvs2(k))))
          ShapeMap(reqProps ++ optProps)

        case (_, _) =>
          NoneType
      }
}
