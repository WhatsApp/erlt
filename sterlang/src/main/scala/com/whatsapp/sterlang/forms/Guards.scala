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

package com.whatsapp.sterlang.forms

import com.whatsapp.sterlang.etf.ETerm

object Guards {
  case class Guard(elems: List[GExpr])
  sealed trait GExpr
  case class GLiteral(literal: Exprs.Literal) extends GExpr
  case class GVariable(variable: String) extends GExpr
  case class GTuple(elems: List[GExpr]) extends GExpr
  case object GNil extends GExpr
  case class GCons(hd: GExpr, tl: GExpr) extends GExpr
  case class GBin(elems: List[GBinElement]) extends GExpr
  case class GBinaryOp(op: String, test1: GExpr, test2: GExpr) extends GExpr
  case class GUnaryOp(op: String, test1: GExpr) extends GExpr
  case class GRecordCreate(recordName: String, fields: List[GRecordField]) extends GExpr
  case class GRecordIndex(recordName: String, fieldName: String) extends GExpr
  case class GRecordFieldAccess(test: GExpr, recordName: String, fieldName: String) extends GExpr
  case class GMapCreate(entries: List[GAssoc]) extends GExpr
  case class GMapUpdate(exp: GExpr, entries: List[GAssoc]) extends GExpr
  // calling erlang:funName
  case class GCall(funName: String, args: List[GExpr]) extends GExpr
  case class GBinElement(test: GExpr, size: ETerm, typeSpecifiers: Exprs.TypeSpecifiers)
  case class GRecordField(fieldName: String, test: GExpr)

  sealed trait GAssoc
  // X := Y - mandatory association
  case class GAssocExact(k: GExpr, v: GExpr) extends GAssoc
  // X => Y - optional association
  case class GAssocOpt(k: GExpr, v: GExpr) extends GAssoc
}
