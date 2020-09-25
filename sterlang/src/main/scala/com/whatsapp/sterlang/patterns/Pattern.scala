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

import com.whatsapp.sterlang.TyCons.RecordTyCon
import com.whatsapp.sterlang.Values.Value
import com.whatsapp.sterlang._

/** Provides a simplified pattern syntax used during exhaustiveness checking. */
private[patterns] object Pattern {
  sealed trait Pat

  /** Matches any value. */
  case object Wildcard extends Pat

  /** A constructor applied to the correct number of arguments. */
  case class ConstructorApplication(constructor: Constructor, arguments: List[Pat]) extends Pat

  sealed trait Constructor
  case class Literal(value: Value) extends Constructor
  case class Tuple(length: Int) extends Constructor
  case object EmptyList extends Constructor
  case object Cons extends Constructor
  case class Record(fields: List[String]) extends Constructor
  case class ClassicStruct(structName: String, fields: List[String]) extends Constructor
  case class OpenVariantStruct(structName: String, fields: List[String]) extends Constructor
  case class EnumConstructor(enum: String, constructor: String) extends Constructor

  /** An unknown constructor of some/any data type.
    *
    * Intuitively, this constructor will match some unknown subset of the value space.
    * We compile patterns we cannot reason about to this class.
    *
    * Note that this isn't a case class because we want every instance to be unique.
    */
  class AbstractConstructor() extends Constructor

  def show(p: Pat): String = {
    def tuple(arguments: List[Pat]): String =
      arguments.map(show).mkString(start = "{", sep = ", ", end = "}")

    p match {
      case Wildcard                                                         => "_"
      case ConstructorApplication(Literal(Values.BooleanValue(value)), Nil) => value.toString
      case ConstructorApplication(Literal(_), _)                            => throw new IllegalArgumentException()
      case ConstructorApplication(Tuple(_), arguments)                      => tuple(arguments)
      case ConstructorApplication(EmptyList, Nil)                           => "[]"
      case ConstructorApplication(Cons, List(head, tail))                   =>
        // Logic for displaying multi element lists nicely, e.g., [E1, E2, E3 | T].
        val result = new StringBuilder("[")
        result ++= show(head)

        def processTail(tail: Pat): Unit =
          tail match {
            case ConstructorApplication(EmptyList, Nil) =>
              result += ']'
            case ConstructorApplication(Cons, h :: t :: Nil) =>
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
      case ConstructorApplication(EmptyList, _) | ConstructorApplication(Cons, _) =>
        throw new IllegalArgumentException()
      case ConstructorApplication(Record(fieldNames), arguments) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(arguments).filter(_._2 != Wildcard)
        fields.map(f => s"${f._1} := ${show(f._2)}").mkString(start = "#{", sep = ", ", end = "}")

      case ConstructorApplication(ClassicStruct(recordName, fieldNames), arguments) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(arguments).filter(_._2 != Wildcard)

        // TODO: quote field names when necessary
        fields.map(f => s"${f._1} = ${show(f._2)}").mkString(start = s"#$recordName{", sep = ", ", end = "}")

      case ConstructorApplication(OpenVariantStruct(recordName, fieldNames), arguments) =>
        show(ConstructorApplication(ClassicStruct(recordName, fieldNames), arguments))

      case ConstructorApplication(EnumConstructor(enum, constructor), arguments) =>
        s"$enum.$constructor${tuple(arguments)}"

      case ConstructorApplication(_: AbstractConstructor, _) =>
        "_"
    }
  }

  /** Converts a surface syntax pattern to the simplified representation here. */
  def simplify(vars: Vars, program: Ast.Program)(pattern: AnnAst.Pat): Pat =
    pattern match {
      case AnnAst.WildPat() =>
        Wildcard

      case AnnAst.VarPat(_) =>
        // Variable names are irrelevant for pattern checking
        Pattern.Wildcard

      case AnnAst.AndPat(p1, p2) =>
        (simplify(vars, program)(p1), simplify(vars, program)(p2)) match {
          case (Wildcard, p2Simple) => p2Simple
          case (p1Simple, Wildcard) => p1Simple
          case _                    =>
            // For now, we only reason about "and" patterns that are used to name the overall value.
            // For example, `{5, Y} = Z` is OK, whereas `{5, Y} = {X, "string"}` isn't.
            ???
        }

      case AnnAst.LiteralPat(value) =>
        ConstructorApplication(Literal(value), Nil)

      case AnnAst.TuplePat(elements) =>
        ConstructorApplication(Tuple(elements.length), elements.map(simplify(vars, program)))

      case AnnAst.RecordPat(patternFields) =>
        val recordType = resolveRecordType(vars)(pattern.typ)
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val allFieldNames: List[String] = getFieldNames(recordType).sorted
        ConstructorApplication(
          Record(allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(vars, program)).getOrElse(Wildcard)),
        )

      case AnnAst.StructPat(name, patternFields) =>
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val structDef = program.structDefs.find(_.name == name).get
        val allFieldNames = structDef.fields.map(_.label).sorted
        val constructor = if (structDef.kind == Ast.StrStruct) ClassicStruct else OpenVariantStruct
        ConstructorApplication(
          constructor(name, allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(vars, program)).getOrElse(Wildcard)),
        )

      case _: AnnAst.BinPat =>
        ConstructorApplication(new AbstractConstructor(), Nil)

      case AnnAst.NilPat() =>
        ConstructorApplication(EmptyList, Nil)

      case AnnAst.ConsPat(head, tail) =>
        ConstructorApplication(Cons, List(simplify(vars, program)(head), simplify(vars, program)(tail)))

      case AnnAst.EnumConstructorPat(enum, constructor, arguments) =>
        ConstructorApplication(EnumConstructor(enum, constructor), arguments.map(simplify(vars, program)))
    }

  /** Returns all field names present in a row type. */
  private def getFieldNames(rowType: Types.RowType): List[String] =
    rowType match {
      case _: Types.RowVarType | Types.RowEmptyType => Nil
      case Types.RowFieldType(f, rest)              => f.label :: getFieldNames(rest)
    }

  /** Resolves a type to a record type and resolves any row variables in the record type. */
  private def resolveRecordType(vars: Vars)(typ: Types.Type): Types.RowType =
    typ match {
      case Types.VarType(typeVar) =>
        vars.tGet(typeVar) match {
          case Types.Instance(constructor) => resolveRecordType(vars)(constructor)
          case _: Types.Open               => throw new IllegalStateException()
        }
      case Types.ConType(RecordTyCon, List(), List(rowType)) => resolveRowVariable(vars)(rowType)
      case _: Types.ConType                                  => throw new IllegalStateException()
    }

  /** Resolves any row variables in a row type if possible. */
  private def resolveRowVariable(vars: Vars)(rowType: Types.RowType): Types.RowType =
    rowType match {
      case Types.RowVarType(typeVar) =>
        vars.rGet(typeVar) match {
          case Types.RowInstance(resolvedRowType) => resolvedRowType
          case _: Types.RowOpen                   => rowType
        }
      case Types.RowEmptyType              => Types.RowEmptyType
      case Types.RowFieldType(field, rest) => Types.RowFieldType(field, resolveRowVariable(vars)(rest))
    }
}
