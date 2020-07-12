package com.whatsapp.sterlang

import com.whatsapp.sterlang.etf._

package object forms {
  def sp(term: ETerm): Pos.SP =
    term match {
      case ETuple(
            List(
              ETuple(List(ELong(line1), ELong(col1))),
              ETuple(List(ELong(line2), ELong(col2))),
            )
          ) =>
        val loc1 = Pos.Loc(line1.toInt, col1.toInt)
        val loc2 = Pos.Loc(line2.toInt, col2.toInt)
        assert(loc1 != Pos.Loc(0, 0))
        assert(loc2 != Pos.Loc(0, 0))
        assert(loc1 != loc2)
        Pos.SP(loc1, loc2)
      case _ =>
        sys.error(s"cannot parse location: $term")
    }

  def spRecordHack(term: ETerm): (Pos.SP, Boolean) =
    term match {
      case ETuple(
            List(
              ETuple(List(ELong(line1), ELong(col1))),
              ETuple(List(ELong(line2), ELong(col2))),
            )
          ) =>
        val loc1 = Pos.Loc(line1.toInt, col1.toInt)
        val loc2 = Pos.Loc(line2.toInt, col2.toInt)
        assert(loc1 != Pos.Loc(0, 0))
        assert(loc2 != Pos.Loc(0, 0))
        assert(loc1 != loc2)
        (Pos.SP(loc1, loc2), false)
      case ETuple(
            List(
              ETuple(List(ELong(line1), ELong(col1))),
              ETuple(List(ELong(line2), ELong(col2))),
              EAtom("open_rec"),
            )
          ) =>
        val loc1 = Pos.Loc(line1.toInt, col1.toInt)
        val loc2 = Pos.Loc(line2.toInt, col2.toInt)
        assert(loc1 != Pos.Loc(0, 0))
        assert(loc2 != Pos.Loc(0, 0))
        assert(loc1 != loc2)
        (Pos.SP(loc1, loc2), true)
      case _ =>
        sys.error(s"cannot parse location: $term")
    }
}
