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

import com.whatsapp.sterlang._

/** Provides a simplified pattern syntax used during exhaustiveness checking. */
private[patterns] object Pattern {
  sealed trait Pat

  /** Matches any value. */
  case object Wildcard extends Pat

  /** A constructor applied to the correct number of arguments. */
  case class CtrApp(ctr: Ctr, args: List[Pat]) extends Pat

  sealed trait Ctr
  case class Literal(value: Ast.Val) extends Ctr
  case class Tuple(length: Int) extends Ctr
  case object EmptyList extends Ctr
  case object Cons extends Ctr
  case class Shape(fields: List[String]) extends Ctr
  case class Struct(struct: String, fields: List[String]) extends Ctr
  case class OpenStruct(struct: String, fields: List[String]) extends Ctr
  case class EnumCtr(enum: String, ctr: String) extends Ctr

  /** An unknown constructor of some/any data type.
    *
    * Intuitively, this constructor will match some unknown subset of the value space.
    * We compile patterns we cannot reason about to this class.
    *
    * Constructors corresponding to different patterns should be different.
    * So, we use the range of the original pattern as an identity of AbstractCtr
    */
  case class AbstractCtr(r: Doc.Range) extends Ctr

  def show(p: Pat): String = {
    def tuple(args: List[Pat]): String =
      args.map(show).mkString("{", ", ", "}")

    p match {
      case Wildcard                                    => "_"
      case CtrApp(Literal(Ast.BooleanVal(value)), Nil) => value.toString
      case CtrApp(Literal(_), _)                       => throw new IllegalArgumentException()
      case CtrApp(Tuple(_), args)                      => tuple(args)
      case CtrApp(EmptyList, Nil)                      => "[]"
      case CtrApp(Cons, List(head, tail))              =>
        // Logic for displaying multi element lists nicely, e.g., [E1, E2, E3 | T].
        val result = new StringBuilder("[")
        result ++= show(head)

        def processTail(tail: Pat): Unit =
          tail match {
            case CtrApp(EmptyList, Nil) =>
              result += ']'
            case CtrApp(Cons, h :: t :: Nil) =>
              result ++= ", "
              result ++= show(h)
              processTail(t)
            case _ =>
              result ++= " | "
              result ++= show(tail)
              result += ']'
          }

        processTail(tail)
        result.toString()
      case CtrApp(EmptyList, _) | CtrApp(Cons, _) =>
        throw new IllegalArgumentException()
      case CtrApp(Shape(fieldNames), args) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(args).filter(_._2 != Wildcard)
        fields.map(f => s"${f._1} := ${show(f._2)}").mkString(start = "#{", sep = ", ", end = "}")

      case CtrApp(Struct(struct, fieldNames), args) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(args).filter(_._2 != Wildcard)

        // TODO: quote field names when necessary
        fields.map(f => s"${f._1} = ${show(f._2)}").mkString(start = s"#$struct{", sep = ", ", end = "}")

      case CtrApp(OpenStruct(struct, fieldNames), args) =>
        show(CtrApp(Struct(struct, fieldNames), args))

      case CtrApp(EnumCtr(enum, ctr), args) =>
        s"$enum.$ctr${tuple(args)}"

      case CtrApp(AbstractCtr(_), _) =>
        "_"
    }
  }

  /** Converts a surface syntax pattern to the simplified representation here. */
  def simplify(tu: TypesUtil, program: Ast.Program)(pattern: AnnAst.Pat): Pat =
    pattern match {
      case AnnAst.WildPat() =>
        Wildcard

      case AnnAst.VarPat(_) =>
        // Variable names are irrelevant for pattern checking
        Pattern.Wildcard

      case AnnAst.AndPat(p1, p2) =>
        (simplify(tu, program)(p1), simplify(tu, program)(p2)) match {
          case (Wildcard, p2Simple) => p2Simple
          case (p1Simple, Wildcard) => p1Simple
          case _                    =>
            // For now, we only reason about "and" patterns that are used to name the overall value.
            // For example, `{5, Y} = Z` is OK, whereas `{5, Y} = {X, "string"}` isn't.
            ???
        }

      case AnnAst.LiteralPat(value) =>
        CtrApp(Literal(value), Nil)

      case AnnAst.TuplePat(elements) =>
        CtrApp(Tuple(elements.length), elements.map(simplify(tu, program)))

      case AnnAst.ShapePat(patternFields) =>
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val allFieldNames: List[String] = tu.labels(pattern.typ).sorted
        CtrApp(
          Shape(allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(tu, program)).getOrElse(Wildcard)),
        )

      case AnnAst.StructPat(name, patternFields) =>
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val structDef = program.structDefs.find(_.name == name).get
        val allFieldNames = structDef.fields.map(_.label).sorted
        val ctr = if (structDef.kind == Ast.StrStruct) Struct else OpenStruct
        CtrApp(
          ctr(name, allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(tu, program)).getOrElse(Wildcard)),
        )

      case _: AnnAst.BinPat =>
        CtrApp(AbstractCtr(pattern.r), Nil)

      case AnnAst.NilPat() =>
        CtrApp(EmptyList, Nil)

      case AnnAst.ConsPat(head, tail) =>
        CtrApp(Cons, List(simplify(tu, program)(head), simplify(tu, program)(tail)))

      case AnnAst.EnumPat(enum, ctr, args) =>
        CtrApp(EnumCtr(enum, ctr), args.map(simplify(tu, program)))
    }
}
