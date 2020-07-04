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
import com.whatsapp.sterlang.forms.Exprs._

object Guards {
  case class Guard(tests: List[Test])
  sealed class Test
  case class TestLiteral(literal: Literal) extends Test
  case class TestVariable(variable: String) extends Test
  case class TestTuple(elems: List[Test]) extends Test
  case object TestNil extends Test
  case class TestCons(hd: Test, tl: Test) extends Test
  case class TestBin(elems: List[TestBinElement]) extends Test
  case class TestBinaryOp(op: String, test1: Test, test2: Test) extends Test
  case class TestUnaryOp(op: String, test1: Test) extends Test
  case class TestRecordCreation(recordName: String, fields: List[RecordFieldTest]) extends Test
  case class TestRecordIndex(recordName: String, fieldName: String) extends Test
  case class TestRecordFieldAccess(test: Test, recordName: String, fieldName: String) extends Test
  case class TestMapCreate(entries: List[Assoc]) extends Test
  case class TestMapUpdate(exp: Expr, entries: List[Assoc]) extends Test
  case class TestCall(funName: String, args: List[Test]) extends Test
  // calling erlang:funName
  case class TestGuardErlangCall(funName: String, args: List[Test]) extends Test
  case class TestBinElement(test: Test, size: ETerm, typeSpecifiers: TypeSpecifiers)
  case class RecordFieldTest(fieldName: String, test: Test)
}
