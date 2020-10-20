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

import scala.collection.mutable.ListBuffer

object AnnAst {
  import Types.Type
  import Ast.{BinElemType, BinOp, UOp}

  /** A node in the intermediate language abstract syntax tree.
    *
    * The intermediate language is similar to the surface language,
    * but every expression is annotated with its type.
    */
  trait Node extends Product {
    val r: Doc.Range
  }

  case class Field[A](label: String, value: A)

  sealed trait Exp extends Node {
    val typ: Type
  }

  // TODO: variableName, enumName, conName etc. should all be classes and have a source location.
  case class VarExp(name: String)(val typ: Type, val r: Doc.Range) extends Exp

  case class LiteralExp(value: Ast.Val)(val typ: Type, val r: Doc.Range) extends Exp
  case class TupleExp(elements: List[Exp])(val typ: Type, val r: Doc.Range) extends Exp
  case class NilExp()(val typ: Type, val r: Doc.Range) extends Exp
  case class BinExp(elements: List[BinElement])(val typ: Type, val r: Doc.Range) extends Exp
  case class ConsExp(head: Exp, tail: Exp)(val typ: Type, val r: Doc.Range) extends Exp

  case class UOpExp(operator: UOp, argument: Exp)(val typ: Type, val r: Doc.Range) extends Exp
  case class BinOpExp(operator: BinOp, argument1: Exp, argument2: Exp)(val typ: Type, val r: Doc.Range) extends Exp
  case class CaseExp(selector: Exp, branches: List[Branch])(val typ: Type, val r: Doc.Range) extends Exp
  case class IfExp(bodies: List[Body])(val typ: Type, val r: Doc.Range) extends Exp
  case class Comprehension(template: Exp, qualifiers: List[Qualifier])(val typ: Type, val r: Doc.Range) extends Exp
  case class BComprehension(template: Exp, qualifiers: List[Qualifier])(val typ: Type, val r: Doc.Range) extends Exp
  case class StructCreate(struct: String, fields: List[Field[Exp]])(val typ: Type, val r: Doc.Range) extends Exp
  case class StructUpdate(exp: Exp, struct: String, fields: List[Field[Exp]])(
      val typ: Type,
      val r: Doc.Range,
  ) extends Exp
  case class StructSelect(exp: Exp, struct: String, fieldName: String)(val typ: Type, val r: Doc.Range) extends Exp
  case class TryCatchExp(tryBody: Body, catchBranches: List[Branch], after: Option[Body])(
      val typ: Type,
      val r: Doc.Range,
  ) extends Exp
  case class TryOfCatchExp(tryBody: Body, tryBranches: List[Branch], catchBranches: List[Branch], after: Option[Body])(
      val typ: Type,
      val r: Doc.Range,
  ) extends Exp
  case class ReceiveExp(branches: List[Branch], after: Option[AfterBody])(val typ: Type, val r: Doc.Range) extends Exp

  case class AfterBody(timeout: Exp, body: Body)

  sealed trait Qualifier
  case class Filter(exp: Exp) extends Qualifier
  case class Generator(pat: Pat, exp: Exp) extends Qualifier
  case class BGenerator(pat: Pat, exp: Exp) extends Qualifier

  case class BinElement(expr: Exp, size: Option[Exp], binElemType: Option[BinElemType])

  case class BlockExp(body: Body)(val r: Doc.Range) extends Exp {
    override val typ: Type = body.typ
  }

  case class FnExp(clauses: List[Clause])(val typ: Type, val r: Doc.Range) extends Exp
  case class NamedFnExp(name: String, clauses: List[Clause])(val typ: Type, val r: Doc.Range) extends Exp
  case class AppExp(function: Exp, arguments: List[Exp])(val typ: Type, val r: Doc.Range) extends Exp

  case class ShapeCreateExp(fields: List[Field[Exp]])(val typ: Type, val r: Doc.Range) extends Exp
  case class ShapeSelectExp(exp: Exp, label: String)(val typ: Type, val r: Doc.Range) extends Exp
  case class ShapeUpdateExp(exp: Exp, fields: List[Field[Exp]])(val typ: Type, val r: Doc.Range) extends Exp
  case class EnumExp(enum: String, ctr: String, args: List[Exp])(val typ: Type, val r: Doc.Range) extends Exp

  case class ValDef(pat: Pat, value: Exp, env: Env, depth: Int, typ: Type) extends Node {
    override val r: Doc.Range = Doc.merge(pat.r, value.r)
  }
  case class Body(prelude: List[ValDef], main: ValDef, typ: Type) extends Node {
    override val r: Doc.Range = {
      val start = if (prelude.isEmpty) main else prelude.head
      Doc.merge(start.r, main.r)
    }
  }
  case class Fun(name: String, clauses: List[Clause], typ: Type)(val r: Doc.Range) extends Node

  case class Clause(pats: List[Pat], guards: List[Guard], body: Body)
  case class Branch(pat: Pat, guards: List[Guard], body: Body)
  case class Guard(expressions: List[Exp])

  sealed trait Pat extends Node {
    val typ: Type
  }

  case class WildPat()(val r: Doc.Range)(val typ: Type) extends Pat
  case class LiteralPat(value: Ast.Val)(val r: Doc.Range)(val typ: Type) extends Pat
  case class VarPat(name: String)(val r: Doc.Range)(val typ: Type) extends Pat
  case class PinnedVarPat(name: String)(val r: Doc.Range)(val typ: Type) extends Pat
  case class AndPat(p1: Pat, p2: Pat)(val r: Doc.Range)(val typ: Type) extends Pat
  case class TuplePat(elements: List[Pat])(val r: Doc.Range)(val typ: Type) extends Pat
  case class NilPat()(val r: Doc.Range)(val typ: Type) extends Pat
  case class BinPat(elements: List[BinElementPat])(val r: Doc.Range)(val typ: Type) extends Pat
  case class ShapePat(fields: List[Field[Pat]])(val r: Doc.Range)(val typ: Type) extends Pat
  case class ConsPat(head: Pat, tail: Pat)(val r: Doc.Range)(val typ: Type) extends Pat
  case class StructPat(structName: String, fields: List[Field[Pat]])(val r: Doc.Range)(val typ: Type) extends Pat
  case class EnumPat(enum: String, ctr: String, args: List[Pat])(val r: Doc.Range)(val typ: Type) extends Pat

  case class BinElementPat(pat: Pat, size: Option[Exp], binElemType: Option[BinElemType])

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
