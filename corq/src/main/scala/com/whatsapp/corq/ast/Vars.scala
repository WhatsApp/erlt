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

import com.whatsapp.corq.ast.Exprs.Clause
import com.whatsapp.corq.ast.Pats._
import erlang.CErl._
import erlang.Data._

object Vars {
  private def patVars(pat: CErl): Set[CVar] =
    pat match {
      case CAlias(_, cvar: CVar, _expr) =>
        Set(cvar)
      case CTuple(_, elems) =>
        elems.flatMap(patVars).toSet
      case CCons(_, hd, tl) =>
        patVars(hd) ++ patVars(tl)
      case CBinary(anno, elems) =>
        elems.flatMap(binaryElemVars).toSet
      case _: CLiteral => Set.empty
      case cvar: CVar  => Set(cvar)
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $pat")
      // $COVERAGE-ON$
    }
  private def binaryElemVars(elem: CBitstr): Set[CVar] = {
    Set.empty
//    val sizeVars: Set[CVar] = elem.size match {
//      case Pats.PatBinSizeConst => Set.empty
//      case PatBinSizeVar(v)     => Set(v.n)
//    }
//    sizeVars ++ patVars(elem.pat)
  }

  def clauseVars(clause: CClause): Set[CVar] =
    clause.pats.flatMap(patVars).toSet
}
