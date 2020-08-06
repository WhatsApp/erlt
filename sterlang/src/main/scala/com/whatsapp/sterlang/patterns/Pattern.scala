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

  case object Wildcard extends Pat

  case class ConstructorApplication(constructor: Constructor, arguments: List[Pat]) extends Pat

  sealed trait Constructor
  case class Literal(value: Value) extends Constructor
  case class Tuple(length: Int) extends Constructor
  case object EmptyList extends Constructor
  case object Cons extends Constructor
  case class Record(fields: List[String], open: Boolean) extends Constructor
  case class ErlangRecord(recordName: String, fields: List[String]) extends Constructor
  case class OpenVariantRecord(recordName: String, fields: List[String]) extends Constructor
  case class EnumConstructor(enum: String, constructor: String) extends Constructor

  def show(p: Pat): String = {
    def tuple(arguments: List[Pat]): String =
      arguments.map(show).mkString(start = "{", sep = ", ", end = "}")

    p match {
      case Wildcard                                                         => "_"
      case ConstructorApplication(Literal(Values.UnitValue), Nil)           => tuple(Nil)
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
      case ConstructorApplication(Record(fieldNames, open), arguments) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(arguments).filter(_._2 != Wildcard)

        // We need to display closed records as open if we dropped any fields above
        val start = (if (open || arguments.contains(Wildcard)) "#" else "") + "#{"
        // TODO: quote field names when necessary (better yet, add generic function to print atoms)
        fields.map(f => s"${f._1} := ${show(f._2)}").mkString(start = start, sep = ", ", end = "}")

      case ConstructorApplication(ErlangRecord(recordName, fieldNames), arguments) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(arguments).filter(_._2 != Wildcard)

        // TODO: quote field names when necessary
        fields.map(f => s"${f._1} = ${show(f._2)}").mkString(start = s"#$recordName{", sep = ", ", end = "}")

      case ConstructorApplication(OpenVariantRecord(recordName, fieldNames), arguments) =>
        show(ConstructorApplication(ErlangRecord(recordName, fieldNames), arguments))

      case ConstructorApplication(EnumConstructor(enum, constructor), arguments) =>
        s"$enum.$constructor${tuple(arguments)}"
    }
  }

  /** Converts a surface syntax pattern to the simplified representation here. */
  def simplify(vars: Vars, program: Ast.Program)(pattern: Absyn.Pat): Pat =
    pattern match {
      case Absyn.WildPat() =>
        Wildcard

      case Absyn.VarPat(_) =>
        // Variable names are irrelevant for pattern checking
        Pattern.Wildcard

      case Absyn.AndPat(p1, p2) =>
        (simplify(vars, program)(p1), simplify(vars, program)(p2)) match {
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
        ConstructorApplication(Tuple(elements.length), elements.map(simplify(vars, program)))

      case Absyn.RecordPat(patternFields, _) =>
        val recordType = resolveRecordType(vars)(pattern.typ)
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val allFieldNames: List[String] = getFieldNames(recordType).sorted
        ConstructorApplication(
          Record(allFieldNames, isOpen(recordType)),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(vars, program)).getOrElse(Wildcard)),
        )

      case Absyn.ERecordPat(recordName, patternFields) =>
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val recordDefinition = program.erlangRecordDefs.find(_.name == recordName).get
        val allFieldNames = recordDefinition.fields.map(_.label).sorted
        val constructor = if (recordDefinition.kind == Ast.ErlangRecord) ErlangRecord else OpenVariantRecord
        ConstructorApplication(
          constructor(recordName, allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify(vars, program)).getOrElse(Wildcard)),
        )

      case Absyn.ListPat(elements) =>
        elements.foldRight(ConstructorApplication(EmptyList, Nil)) {
          case (head, tail) => ConstructorApplication(Cons, List(simplify(vars, program)(head), tail))
        }

      case Absyn.ConsPat(head, tail) =>
        ConstructorApplication(Cons, List(simplify(vars, program)(head), simplify(vars, program)(tail)))

      case Absyn.EnumConstructorPat(enum, constructor, arguments) =>
        ConstructorApplication(EnumConstructor(enum, constructor), arguments.map(simplify(vars, program)))
    }

  /** Returns all field names present in a row type. */
  private def getFieldNames(rowType: Types.RowType): List[String] =
    rowType match {
      case _: Types.RowVarType | Types.RowEmptyType => Nil
      case Types.RowFieldType(f, rest)              => f.label :: getFieldNames(rest)
    }

  /** Returns true if the given row type has a row variable. */
  private def isOpen(rowType: Types.RowType): Boolean =
    rowType match {
      case _: Types.RowVarType         => true
      case Types.RowEmptyType          => false
      case Types.RowFieldType(_, rest) => isOpen(rest)
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
