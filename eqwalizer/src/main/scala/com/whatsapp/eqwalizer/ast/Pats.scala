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

import com.whatsapp.eqwalizer.ast.BinarySpecifiers.Specifier

object Pats {
  sealed trait Pat { val l: Int }

  case class PatWild()(val l: Int) extends Pat
  case class PatMatch(pat: Pat, arg: Pat)(val l: Int) extends Pat
  case class PatTuple(elems: List[Pat])(val l: Int) extends Pat

  case class PatNil()(val l: Int) extends Pat
  case class PatCons(h: Pat, t: Pat)(val l: Int) extends Pat

  case class PatNumber()(val l: Int) extends Pat
  case class PatAtom(s: String)(val l: Int) extends Pat
  case class PatVar(n: String)(val l: Int) extends Pat
  case class PatRecord(recName: String, fields: List[PatRecordField])(val l: Int) extends Pat
  case class PatRecordIndex(recName: String, fieldName: String)(val l: Int) extends Pat

  case class PatUnOp(op: String, arg: Pat)(val l: Int) extends Pat
  case class PatBinOp(op: String, arg1: Pat, arg2: Pat)(val l: Int) extends Pat

  case class PatBinary(elems: List[PatBinaryElem])(val l: Int) extends Pat
  case class PatBinaryElem(pat: Pat, size: PatBinSize, specifier: Specifier)(val l: Int)
  case class PatRecordField(name: String, pat: Pat)
  case class PatMap(kvs: List[(Pat, Pat)])(val l: Int) extends Pat

  sealed trait PatBinSize
  case object PatBinSizeConst extends PatBinSize
  case class PatBinSizeVar(v: PatVar) extends PatBinSize
}
