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

// Naive implementation of UnionFind
object UnionFind {
  class Var[A](private[UnionFind] var rank: Int) {
    override def toString: String = s"Var[rank=$rank]"
  }
}
class UnionFind[A] {
  import scala.collection.mutable.{ArrayBuffer, Map => MMap}
  import UnionFind._

  private var nextRank = 0
  private val rankToValue: MMap[Int, A] = MMap()
  private val vars: ArrayBuffer[Var[A]] = ArrayBuffer()

  private def nextTRank(): Int = {
    nextRank = nextRank + 1
    nextRank - 1
  }

  def make(value: A): Var[A] = {
    val tVar = new Var[A](nextTRank())
    vars.append(tVar)
    rankToValue.put(tVar.rank, value)
    tVar
  }

  def get(v: Var[A]): A =
    rankToValue(v.rank)
  def set(v: Var[A], value: A): Unit =
    rankToValue.put(v.rank, value)
  def eq(v1: Var[A], v2: Var[A]): Boolean =
    v1.rank == v2.rank
  def id(v: Var[A]): Int =
    v.rank

  def link(v1: Var[A], v2: Var[A]): Boolean =
    if (v1.rank == v2.rank) {
      false
    } else {
      val (rank1, rank2) = (v1.rank, v2.rank)
      val rank = nextTRank()
      rankToValue.put(rank, rankToValue(rank2))

      vars.foreach { v =>
        if (v.rank == rank1 || v.rank == rank2)
          v.rank = rank
      }

      rankToValue.remove(rank1)
      rankToValue.remove(rank2)
      true
    }
}
