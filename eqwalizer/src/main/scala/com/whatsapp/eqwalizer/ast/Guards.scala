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

object Guards {
  case class Guard(tests: List[Test])

  sealed trait Test { val l: Int }
  case class TestVar(v: String)(val l: Int) extends Test
  case class TestAtom(s: String)(val l: Int) extends Test
  case class TestNumber()(val l: Int) extends Test
  case class TestTuple(elems: List[Test])(val l: Int) extends Test
  case class TestNil()(val l: Int) extends Test
  case class TestCons(h: Test, t: Test)(val l: Int) extends Test
  case class TestLocalCall(id: Id, args: List[Test])(val l: Int) extends Test

  case class TestUnOp(op: String, arg: Test)(val l: Int) extends Test
  case class TestBinOp(op: String, arg1: Test, arg2: Test)(val l: Int) extends Test
}
