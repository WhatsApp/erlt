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

package com.whatsapp.sterlang

class Vars() {
  import UnionFind.Var

  type TV = Types.TypeVarValue
  type RV = Types.RowTypeVarValue

  private val tVars: UnionFind[TV] = new UnionFind
  private val rVars: UnionFind[RV] = new UnionFind

  def tVar(value: TV): Var[TV] = tVars.make(value)
  def rVar(value: RV): Var[RV] = rVars.make(value)

  def tGet(key: Var[TV]): TV = tVars.get(key)
  def rGet(key: Var[RV]): RV = rVars.get(key)

  def tSet(key: Var[TV], value: TV): Unit = tVars.set(key, value)
  def rSet(key: Var[RV], value: RV): Unit = rVars.set(key, value)

  def tEq(key1: Var[TV], key2: Var[TV]): Boolean = tVars.eq(key1, key2)
  def rEq(key1: Var[RV], key2: Var[RV]): Boolean = rVars.eq(key1, key2)

  def tLink(key1: Var[TV], key2: Var[TV]): Boolean = tVars.link(key1, key2)

  object TVarOrdering extends Ordering[Var[TV]] {
    override def compare(key1: Var[TV], key2: Var[TV]): Int =
      Integer.compare(tVars.id(key1), tVars.id(key2))
  }
  object RVarOrdering extends Ordering[Var[RV]] {
    override def compare(key1: Var[RV], key2: Var[RV]): Int =
      Integer.compare(rVars.id(key1), rVars.id(key2))
  }
}
