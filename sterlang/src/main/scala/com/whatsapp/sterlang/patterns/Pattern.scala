package com.whatsapp.sterlang.patterns

import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang.Types.Type
import com.whatsapp.sterlang.Values.Value
import com.whatsapp.sterlang.{Absyn, Pos}

/** Provides a simplified pattern syntax used during exhaustiveness checking. */
private[patterns] object Pattern {
  sealed trait Pat extends HasSourceLocation {

    /** The type of values this pattern can match. */
    val typ: Type
  }

  case class Wildcard()(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class Record(fields: List[Absyn.Field[Pat]], open: Boolean)(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class ConstructorApplication(constructor: Constructor, arguments: List[Pat])(
      val typ: Type,
      val sourceLocation: Pos.P,
  ) extends Pat

  // TODO: this stuff should be moved to [[Ast]] and [[Absyn]].
  sealed trait Constructor
  case class Literal(value: Value) extends Constructor
  case class Tuple(length: Int) extends Constructor
  case object EmptyList extends Constructor
  case object Cons extends Constructor
  case class EnumConstructor(enum: String, constructor: String) extends Constructor
}
