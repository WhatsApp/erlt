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

package com.whatsapp.sterlang

import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang.Values.Value

import scala.collection.mutable.ListBuffer

object Absyn {
  type Type = Types.Type
  type RowType = Types.RowType
  type BinOp = Ast.BinOp
  type UOp = Ast.UOp

  /** A node in the intermediate language abstract syntax tree.
    *
    * The intermediate language is similar to the surface language,
    * but every expression is annotated with its type.
    */
  trait Node extends HasSourceLocation with Product

  case class Field[A](label: String, value: A)

  sealed trait Exp extends Node {

    /** The type of the expression. */
    val typ: Type
  }

  // TODO: variableName, enumName, conName etc. should all be classes and have a source location.

  case class VarExp(name: String)(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // Begin TODO: these can be elaborated to constructor applications
  case class LiteralExp(value: Value)(val sourceLocation: Pos.P) extends Exp {
    override val typ: Type = value.typ
  }
  case class TupleExp(elements: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class ListExp(elements: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class ConsExp(head: Exp, tail: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // End TODO

  case class UOpExp(operator: UOp, argument: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class BinOpExp(operator: BinOp, argument1: Exp, argument2: Exp)(val typ: Type, val sourceLocation: Pos.P)
      extends Exp
  case class CaseExp(selector: Exp, branches: List[Branch])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class IfExp(bodies: List[Body])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class Comprehension(template: Exp, qualifiers: List[Qualifier])(val typ: Type, val sourceLocation: Pos.P)
      extends Exp

  sealed trait Qualifier
  case class Filter(exp: Exp) extends Qualifier
  case class Generator(pat: Pat, exp: Exp) extends Qualifier

  // TODO: is [[SeqExp]] significantly different than [[BlockExp]]?
  case class SeqExp(e1: Exp, e2: Exp)(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class BlockExp(body: Body)(val sourceLocation: Pos.P) extends Exp {
    override val typ: Type = body.typ
  }

  // TODO: can [FnExp] be unified with [NamedFnExp]?
  case class FnExp(clauses: List[Clause])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class NamedFnExp(name: String, clauses: List[Clause])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  case class AppExp(function: Exp, arguments: List[Exp])(val typ: Type, val sourceLocation: Pos.P) extends Exp

  // TODO: should this have a RowType?
  case class RecordExp(fields: List[Field[Exp]])(val typ: Type, val sourceLocation: Pos.P) extends Exp
  // TODO: what's tp
  case class RecordSelectionExp(record: Exp, tp: Type, label: String)(val typ: Type, val sourceLocation: Pos.P)
      extends Exp
  // TODO: what's tp1
  case class RecordUpdateExp(record: Exp, tp1: Type, fields: List[Field[Exp]])(val typ: Type, val sourceLocation: Pos.P)
      extends Exp

  case class EnumConstructorExp(enum: String, constructor: String, arguments: List[Exp])(
      val typ: Type,
      val sourceLocation: Pos.P,
  ) extends Exp

  case class ValDef(pat: Pat, value: Exp, env: Env, depth: Int, typ: Type) extends Node {
    override val sourceLocation: Pos.P = Pos.merge(pat.sourceLocation, value.sourceLocation)
  }
  case class Body(prelude: List[ValDef], main: ValDef, typ: Type) extends Node {
    override val sourceLocation: Pos.P = {
      val start = if (prelude.isEmpty) main else prelude.head
      Pos.merge(start.sourceLocation, main.sourceLocation)
    }
  }
  case class Fun(name: String, clauses: List[Clause], typ: Type)(val sourceLocation: Pos.P) extends Node
  case class Clause(pats: List[Pat], body: Body)

  case class Branch(pat: Pat, body: Body)

  sealed trait Pat extends Node {

    /** The type of values this pattern can match. */
    val typ: Type
  }

  case class WildPat()(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class VarPat(name: String)(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class AndPat(p1: Pat, p2: Pat)(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class LiteralPat(value: Value)(val sourceLocation: Pos.P) extends Pat {
    override val typ: Type = value.typ
  }
  case class TuplePat(elements: List[Pat])(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class ListPat(elements: List[Pat])(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class RecordPat(fields: List[Field[Pat]], open: Boolean)(val typ: Type, val sourceLocation: Pos.P) extends Pat

  case class ConsPat(head: Pat, tail: Pat)(val typ: Type, val sourceLocation: Pos.P) extends Pat
  case class EnumConstructorPat(enum: String, constructor: String, arguments: List[Pat])(
      val typ: Type,
      val sourceLocation: Pos.P,
  ) extends Pat

  /** Returns an iterator over all immediate children nodes. This is empty for leaf nodes. */
  def children(node: Node): Iterator[Node] = {
    val pending = ListBuffer[Any](node.productIterator)
    val result = ListBuffer[Node]()

    while (pending.nonEmpty) {
      val n = pending.remove(0)
      n match {
        case n: Node =>
          result.append(n)
        case c: IterableOnce[_] =>
          pending.prependAll(c)
        case c: Field[_] =>
          pending.prepend(c.value)
        case c: Clause =>
          pending.prepend(c.body)
          pending.prependAll(c.pats)
        case c: Branch =>
          pending.prepend(c.body)
          pending.prepend(c.pat)
        case _ => // ignore
      }
    }

    result.iterator
  }
}
