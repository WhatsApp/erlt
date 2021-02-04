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

package com.whatsapp.corq.tc

import com.whatsapp.corq.ast.Types._
import erlang.CErl._

// These operations are sound approximations...
// They should be used really carefully, - they can be sound in one context,
// but unsound in another context
object Approx {
  def asListType(t: Type): Option[ListType] =
    extractListElem(t) match {
      case Nil => None
      case ts  => Some(ListType(UnionType(ts)))
    }

  private def extractListElem(t: Type): List[Type] =
    t match {
      case AnyType =>
        List(AnyType)
      case UnionType(tys) =>
        tys.flatMap(extractListElem)
      case NilType =>
        List(AnyType)
      case ListType(elemType) =>
        List(elemType)
      case _ =>
        List()
    }

  def asTupleType(t: Type, arity: Int): Option[TupleType] =
    extractTupleElems(t, arity) match {
      case Nil => None
      case tss => Some(TupleType(tss.transpose.map(UnionType)))
    }

  private def extractTupleElems(t: Type, arity: Int): List[List[Type]] =
    t match {
      case AnyType =>
        List(List.fill(arity)(AnyType))
      case UnionType(tys) =>
        tys.flatMap(extractTupleElems(_, arity))
      case TupleType(argTys) =>
        if (argTys.size == arity) List(argTys) else List()
      case _ =>
        List()
    }

  def asCValuesType(t: Type, arity: Int): Option[CValuesType] =
    extractValueElems(t, arity) match {
      case Nil => None
      case tss => Some(CValuesType(tss.transpose.map(UnionType)))
    }

  private def extractValueElems(t: Type, arity: Int): List[List[Type]] =
    t match {
      case AnyType =>
        List(List.fill(arity)(AnyType))
      case UnionType(tys) =>
        tys.flatMap(extractValueElems(_, arity))
      case CValuesType(argTys) =>
        if (argTys.size == arity) List(argTys) else Nil
      case _ => Nil
    }

  def combineEnvs(
               initialEnv: Env,
               op: (Type, Type) => Type,
               envs: Iterable[Env]
             ): Env = {
    val vars = initialEnv.keySet
    var acc = envs.head
    for {
      v <- vars
      other <- envs
    } {
      val default = initialEnv(v)
      val left = acc.getOrElse(v, default)
      val right = other.getOrElse(v, default)
      val ty = op(left, right)
      acc += (v -> ty)
    }
    acc
  }

}
