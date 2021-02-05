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

// copied from analyzer
package erlang

import com.whatsapp.corq.ast.Id
import erlang.Data.Anno

object CErl {

  sealed trait CErl {
    def anno: Anno

    /**
      * zero indicates no known line number
      */
    def line: Int = anno.line
  }
  case class CAlias(anno: Anno, v: CVar, pat: CErl) extends CErl
  case class CApply(anno: Anno, op: CErl, args: List[CErl]) extends CErl
  case class CBinary(anno: Anno, segments: List[CBitstr]) extends CErl
  case class CBitstr(
      anno: Anno,
      value: CErl,
      size: CErl,
      unit: CErl,
      typ: CErl,
      flags: CErl
  ) extends CErl
  case class CCall(anno: Anno, module: CErl, name: CErl, args: List[CErl])
      extends CErl
  case class CCase(anno: Anno, sel: CErl, clauses: List[CClause]) extends CErl
  case class CCatch(anno: Anno, body: CErl) extends CErl
  case class CClause(anno: Anno, pats: List[CErl], guard: CErl, body: CErl)
      extends CErl
  case class CCons(anno: Anno, hd: CErl, tl: CErl) extends CErl
  case class CFun(anno: Anno, vars: List[CVar], body: CErl) extends CErl
  case class CLet(anno: Anno, vars: List[CVar], arg: CErl, body: CErl)
      extends CErl
  case class CLetRec(anno: Anno, defs: Map[CVar, CFun], body: CErl) extends CErl
  case class CLiteral(anno: Anno, value: Data.EObject) extends CErl
  // TODO
  case class CMap(anno: Anno, arg: CErl, es: List[CErl], isPat: Boolean)
      extends CErl
  // TODO
  case class CMapPair(anno: Anno, op: CErl, key: CErl, cVal: CErl) extends CErl
  case class CModule(
      anno: Anno,
      name: CErl,
      exports: List[CErl],
      attrs: List[(CErl, CErl)],
      defs: List[(CErl, CErl)]
  ) extends CErl
  case class CPrimOp(anno: Anno, name: CErl, args: List[CErl]) extends CErl
  case class CReceive(
      anno: Anno,
      clauses: List[CErl],
      timeout: CErl,
      action: CErl
  ) extends CErl
  // TODO - can be any()???
  case class CSeq(anno: Anno, arg: CErl, body: CErl) extends CErl
  case class CTry(
      anno: Anno,
      arg: CErl,
      bodyVars: List[CVar],
      body: CErl,
      evars: List[CVar],
      handler: CErl
  ) extends CErl
  case class CTuple(anno: Anno, elems: List[CErl]) extends CErl
  case class CValues(anno: Anno, elems: List[CErl]) extends CErl
  case class CVar(anno: Anno, name: VarName) extends CErl
  case class C___XXX(anno: Anno, raw: Data.EObject) extends CErl

  sealed trait VarName
  case class VarNameInt(i: Int) extends VarName {
    override def toString = s"_$i"
  }
  case class VarNameAtom(atom: String) extends VarName {
    override def toString = s"'$atom'"
  }
  case class VarNameAtomInt(id: Id) extends VarName {
    override def toString = id.toString
  }

}