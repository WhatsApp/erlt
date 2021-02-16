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
  case class TestRecordCreate(recName: String, fields: List[TestRecordField])(val l: Int) extends Test
  case class TestRecordSelect(rec: Test, recName: String, fieldName: String)(val l: Int) extends Test
  case class TestRecordIndex(recName: String, fieldName: String)(val l: Int) extends Test
  case class TestMapCreate(kvs: List[(Test, Test)])(val l: Int) extends Test
  case class TestReqMapUpdate(map: Test, kvs: List[(String, Test)])(val l: Int) extends Test
  case class TestGenMapUpdate(map: Test, kvs: List[(Test, Test)])(val l: Int) extends Test

  case class TestUnOp(op: String, arg: Test)(val l: Int) extends Test
  case class TestBinOp(op: String, arg1: Test, arg2: Test)(val l: Int) extends Test
  // Simplification. See https://fburl.com/binaryguards
  case class TestBinaryLit()(val l: Int) extends Test

  case class TestRecordField(name: String, value: Test)
}
