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

package com.whatsapp.sterlang.patterns

import com.whatsapp.sterlang.Absyn
import com.whatsapp.sterlang.Values.Value

/** Provides a simplified pattern syntax used during exhaustiveness checking. */
private[patterns] object Pattern {
  sealed trait Pat

  case object Wildcard extends Pat

  case class ConstructorApplication(constructor: Constructor, arguments: List[Pat]) extends Pat

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
        Wildcard

      case Absyn.VarPat(_) =>
        // Variable names are irrelevant for pattern checking
        Pattern.Wildcard

      case Absyn.AndPat(p1, p2) =>
        (simplify(p1), simplify(p2)) match {
          case (Wildcard, p2Simple) => p2Simple
          case (p1Simple, Wildcard) => p1Simple
          case _                    =>
            // For now, we only reason about "and" patterns that are used to name the overall value.
            // For example, `{5, Y} = Z` is OK, whereas `{5, Y} = {X, "string"}` isn't.
            ???
        }

      case Absyn.LiteralPat(value) =>
        ConstructorApplication(Literal(value), Nil)

      case Absyn.TuplePat(elements) =>
        ConstructorApplication(Tuple(elements.length), elements.map(simplify))

      case Absyn.RecordPat(_, _) => ???

      case Absyn.ListPat(elements) =>
        elements.foldRight(ConstructorApplication(EmptyList, Nil)) {
          case (head, tail) => ConstructorApplication(Cons, List(simplify(head), tail))
        }

      case Absyn.ConsPat(head, tail) =>
        ConstructorApplication(Cons, List(simplify(head), simplify(tail)))

      case Absyn.EnumConstructorPat(enum, constructor, arguments) =>
        ConstructorApplication(EnumConstructor(enum, constructor), arguments.map(simplify))
    }
}
