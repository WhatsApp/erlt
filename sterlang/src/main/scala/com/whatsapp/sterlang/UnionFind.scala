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

object UnionFind {
  class Var[A](private[UnionFind] var x: URefC[A])

  // Pointer (PTR) or Equivalence Class Representative (ECR)
  private sealed trait URefC[A]
  private case class ECR[A](var a: (A, Int)) extends URefC[A]
  private case class PTR[A](var ref: Var[A]) extends URefC[A]

  private[UnionFind] def uRef[A](a: (A, Int)): Var[A] =
    new Var(ECR(a))

  private[UnionFind] def find[A](uref: Var[A]): Var[A] =
    uref.x match {
      case ECR(_) =>
        uref
      case PTR(p1) =>
        val p2 = find(p1)
        uref.x = PTR(p2)
        p2
    }

  private[UnionFind] def _link[A](p: Var[A], q: Var[A]): Boolean = {
    val p1 = find(p)
    val q1 = find(q)
    if (p1 eq q1) {
      false
    } else {
      p1.x = PTR(q)
      true
    }
  }

  private[UnionFind] def _eq[A](p: Var[A], q: Var[A]): Boolean =
    find(p) eq find(q)

  private[UnionFind] def _get[A](p: Var[A]): (A, Int) = {
    val ECR(a) = find(p).x
    a
  }

  private[UnionFind] def _set[A](p: Var[A], x: (A, Int)): Unit = {
    val t @ ECR(_) = find(p).x
    t.a = x
  }
}

final class UnionFind[A] {
  import UnionFind._

  private var tNext = 0

  def make(a: A): Var[A] = {
    val i = tNext
    tNext = tNext + 1
    UnionFind.uRef((a, i))
  }

  def get(tv: Var[A]): A =
    UnionFind._get(tv)._1

  def set(tv: Var[A], a: A): Unit =
    UnionFind._set(tv, (a, id(tv)))

  def id(tv: Var[A]): Int =
    UnionFind._get(tv)._2

  def eq(tv1: Var[A], tv2: Var[A]): Boolean =
    UnionFind._eq(tv1, tv2)

  def link(tv1: Var[A], tv2: Var[A]): Boolean =
    UnionFind._link(tv1, tv2)
}
