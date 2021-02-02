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

import com.whatsapp.eqwalizer.ast.Types._

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

  def joinEnvs(envs: List[Env]): Env = {
    var envAcc: Env = envs.head
    val vars = envAcc.keySet
    for { env <- envs; v <- vars } envAcc = envAcc.updated(v, Subtype.join(envAcc(v), env(v)))
    envAcc
  }
}