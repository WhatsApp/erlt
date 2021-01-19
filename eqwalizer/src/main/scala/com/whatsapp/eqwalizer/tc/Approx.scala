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

  def joinEnvs(initEnv: Env, envs: List[Env]): Env = {
    val vars = initEnv.keySet
    var envAcc = initEnv
    for { e <- envs } {
      envAcc = vars.toList.map(v => v -> Subtype.join(envAcc(v), e(v))).toMap
    }
    envAcc
  }

  def joinEnvsAll(envs: List[Env]): Env = {
    val allVars: Set[String] = envs.map(_.keySet).reduce(_ ++ _)
    var envAcc: Env = Map.empty
    for {
      env <- envs
      v <- allVars
    } (envAcc.get(v), env.get(v)) match {
      case (_, None) =>
      // Nothing
      case (None, Some(t)) =>
        envAcc = envAcc.updated(v, t)
      case (Some(t1), Some(t2)) =>
        envAcc = envAcc.updated(v, Subtype.join(t1, t2))
    }
    envAcc
  }
}
