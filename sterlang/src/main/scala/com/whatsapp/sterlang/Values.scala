package com.whatsapp.sterlang

import com.whatsapp.sterlang.Types.ConType

object Values {

  /** The result of evaluating an expression. */
  sealed trait Value {
    /** The type of the value. */
    def typ: ConType
  }

  case object UnitValue extends Value {
    override def typ: ConType = METypes.UnitType
  }

  case class BooleanValue(value: Boolean) extends Value {
    override def typ: ConType = METypes.BoolType
  }

  case class IntegerValue(value: Int) extends Value {
    override def typ: ConType = METypes.IntType
  }

  case class CharValue(value: String) extends Value {
    override def typ: ConType = METypes.CharType
  }

  case class StringValue(value: String) extends Value {
    override def typ: ConType = METypes.StringType
  }
}
