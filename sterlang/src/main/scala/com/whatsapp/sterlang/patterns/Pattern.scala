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

  /** Converts a surface syntax pattern to the simplified representation here. */
  def simplify(pattern: Absyn.Pat): Pat =
    pattern match {
      case Absyn.WildPat() =>
        Pattern.Wildcard()(typ = pattern.typ, sourceLocation = pattern.sourceLocation)

      case Absyn.VarPat(_) =>
        // Variable names are irrelevant for our purposes
        Pattern.Wildcard()(typ = pattern.typ, sourceLocation = pattern.sourceLocation)

      case Absyn.AndPat(p1, p2) =>
        (simplify(p1), simplify(p2)) match {
          case (_: Absyn.WildPat, p2Simple) => p2Simple
          case (p1Simple, _: Absyn.WildPat) => p1Simple
          case _                            =>
            // For now, we only reason about "and" patterns that are used to name the overall value.
            // For example, `{5, Y} = Z` is OK, whereas `{X, "string"} = {5, Y}` isn't.
            ???
        }

      case Absyn.LiteralPat(value) =>
        Pattern.ConstructorApplication(Pattern.Literal(value), Nil)(
          typ = value.typ,
          sourceLocation = pattern.sourceLocation,
        )

      case Absyn.TuplePat(elements) =>
        Pattern.ConstructorApplication(Pattern.Tuple(elements.length), elements.map(simplify))(
          typ = pattern.typ,
          sourceLocation = pattern.sourceLocation,
        )

      case Absyn.RecordPat(_, _) => ???

      case Absyn.ListPat(Nil) =>
        Pattern.ConstructorApplication(Pattern.EmptyList, Nil)(
          typ = pattern.typ,
          sourceLocation = pattern.sourceLocation,
        )

      case Absyn.ListPat(_ :: _) => ???

      case Absyn.ConsPat(head, tail) =>
        Pattern.ConstructorApplication(Pattern.Cons, List(simplify(head), simplify(tail)))(
          typ = pattern.typ,
          sourceLocation = pattern.sourceLocation,
        )

      case Absyn.EnumConstructorPat(enum, constructor, arguments) =>
        Pattern.ConstructorApplication(Pattern.EnumConstructor(enum, constructor), arguments.map(simplify))(
          typ = pattern.typ,
          sourceLocation = pattern.sourceLocation,
        )
    }
}
