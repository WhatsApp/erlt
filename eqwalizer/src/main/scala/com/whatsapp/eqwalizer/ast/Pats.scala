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
