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

import com.whatsapp.eqwalizer.ast.Exprs.Clause
import com.whatsapp.eqwalizer.ast.Pats._

object Vars {
  private def patVars(pat: Pat): Set[String] =
    pat match {
      case PatWild() =>
        Set.empty
      case PatMatch(pat, arg) =>
        patVars(pat) ++ patVars(arg)
      case PatTuple(elems) =>
        elems.flatMap(patVars).toSet
      case PatNil() =>
        Set.empty
      case PatCons(h, t) =>
        patVars(h) ++ patVars(t)
      case PatNumber() =>
        Set.empty
      case PatAtom(_) =>
        Set.empty
      case PatVar(n) =>
        Set(n)
      case PatUnOp(_, arg) =>
        patVars(arg)
      case PatBinOp(_, arg1, arg2) =>
        patVars(arg1) ++ patVars(arg2)
      case PatBinary(elems) =>
        elems.flatMap(binaryElemVars).toSet
    }

  private def binaryElemVars(elem: PatBinaryElem): Set[String] = {
    val sizeVars: Set[String] = elem.size match {
      case Pats.PatBinSizeConst => Set.empty
      case PatBinSizeVar(v)     => Set(v.n)
    }
    sizeVars ++ patVars(elem.pat)
  }

  def clauseVars(clause: Clause): Set[String] =
    clause.pats.flatMap(patVars).toSet
}
