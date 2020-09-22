package com.whatsapp.sterlang

import com.whatsapp.sterlang.etf._

package object forms {
  def r(term: ETerm): Doc.Range = {
    val ETuple(List(ETuple(List(ELong(line1), ELong(col1))), ETuple(List(ELong(line2), ELong(col2))))) = term
    val pos1 = Doc.Pos(line1.toInt, col1.toInt)
    val pos2 = Doc.Pos(line2.toInt, col2.toInt)
    assert(pos1 != Doc.Pos(0, 0))
    assert(pos2 != Doc.Pos(0, 0))
    assert(pos1 != pos2)
    Doc.Range(pos1, pos2)
  }
}
