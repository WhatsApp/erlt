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

// Low-lever (encapsulate) unify error
class UnifyError(msg: String) extends Exception(msg)
case object Circularity extends UnifyError("Circularity")
case object RowCircularity extends UnifyError("RowCircularity")
case class FieldMismatch(label: String) extends UnifyError(s"Field mismatch: $label")
case class TyConMismatch(tc1: TyCons.TyCon, tc2: TyCons.TyCon) extends UnifyError(s"TyCon mismatch $tc1 <> $tc2")

class TypeError(msg: String) extends Exception(msg)

case class Cycle(aliasName: String) extends Exception(s"The type alias $aliasName is cyclic")

sealed trait Severity
case object Error extends Severity
case object Warning extends Severity

case class RangedError(range: Doc.Range, title: String, description: Option[String]) extends Exception(title) {
  val severity: Severity = Error
}
class InfiniteTypeError(range: Doc.Range, t1: String, t2: String)
    extends RangedError(range, s"Infinite type", Some(s"$t1 <> $t2"))
class TypeMismatchError(range: Doc.Range, required: String, found: String)
    extends RangedError(range, s"Type Mismatch", Some(s"Found:    $found\nRequired: $required"))
class UnboundVar(range: Doc.Range, val name: String) extends RangedError(range, s"Unbound variable: $name", None)
class AlreadyBoundVar(range: Doc.Range, val name: String)
    extends RangedError(range, s"Already bound variable: $name", None)
class UnknownEnum(range: Doc.Range, val enumName: String) extends RangedError(range, s"Unknown enum: $enumName", None)
class UnknownEnumCon(range: Doc.Range, val enumConName: String)
    extends RangedError(range, s"Unknown enum constructor: $enumConName", None)
class SpecError(range: Doc.Range, val fName: String, val specType: String, val elabType: String)
    extends RangedError(range, s"Spec mismatch for $fName", Some(s"Specified:  `$specType`\nElaborated: `$elabType`"))
class InconsistentStructTypeParamsError(range: Doc.Range, val specParams: String, val elabParams: String)
    extends RangedError(
      range,
      s"Inconsistent struct type params",
      Some(s"Specified:  `$specParams`\nElaborated: `$elabParams`"),
    )
class DuplicateFields(range: Doc.Range, names: List[String])
    extends RangedError(range, s"Duplicate fields: ${names.mkString(", ")}", None)
class CyclicTypeAlias(range: Doc.Range, aliasName: String)
    extends RangedError(range, s"Cyclic type alias: $aliasName", None)
class UnknownType(range: Doc.Range, name: String, arity: Int)
    extends RangedError(range, s"Unknown type: $name/$arity", None)
class UnknownStruct(range: Doc.Range, name: String) extends RangedError(range, s"Unknown struct: #$name{}", None)
class UnknownField(range: Doc.Range, fieldName: String) extends RangedError(range, s"Unknown field $fieldName", None)
class UnInitializedField(range: Doc.Range, fieldName: String)
    extends RangedError(range, s"Uninitialized field $fieldName", None)
class PosFieldMismatch(range: Doc.Range, actual: Int, expect: Int)
    extends RangedError(range, s"Got $actual positional field(s) instead of $expect", None)
class UnconditionalExceptionUpdate(range: Doc.Range, structName: String)
    extends RangedError(range, s"Unconditional update. $structName is exception()", None)
class UnconditionalMessageUpdate(range: Doc.Range, structName: String)
    extends RangedError(range, s"Unconditional update. $structName is message()", None)
class UnconditionalExceptionSelect(range: Doc.Range, structName: String)
    extends RangedError(range, s"Unconditional select. $structName is exception()", None)
class UnconditionalMessageSelect(range: Doc.Range, structName: String)
    extends RangedError(range, s"Unconditional select. $structName is message()", None)
class IllegalCatchPattern(range: Doc.Range) extends RangedError(range, s"Illegal catch pattern", None)
class IllegalReceivePattern(range: Doc.Range) extends RangedError(range, s"Illegal receive pattern", None)
class ExceptionType(range: Doc.Range, exceptionName: String)
    extends RangedError(range, s"$exceptionName() is not a type. Did you mean `exception()`?", None)
class MessageType(range: Doc.Range, messageName: String)
    extends RangedError(range, s"$messageName() is not a type. Did you mean `message()`?", None)
class PolymorphicException(range: Doc.Range)
    extends RangedError(range, s"Exception structs do not allow type parameters", None)
class PolymorphicMessage(range: Doc.Range)
    extends RangedError(range, s"Message structs do not allow type parameters", None)
class DuplicateTypeVar(range: Doc.Range, v: String) extends RangedError(range, s"Duplicate type var: $v", None)
class UnboundTypeVariable(range: Doc.Range, name: String)
    extends RangedError(range, s"Unbound type variable: $name", None)
class RHSOpenShape(range: Doc.Range)
    extends RangedError(
      range,
      s"Shape extension type in a type definition",
      Some("Shape extensions are not allowed in type definitions. Shape extensions are allowed in specs only."),
    )
class TypeVarKindConflict(range: Doc.Range, varName: String)
    extends RangedError(
      range,
      s"Plain type variable $varName is used as row type variable",
      Some("All occurrences of type variable should of the same kind (plain or row)"),
    )
class InconsistentShapeExtension(range: Doc.Range, varName: String, prevKind: List[String], thisKind: List[String])
    extends RangedError(
      range,
      s"Shape extension type variable $varName is used inconsistently",
      Some(
        s"""Expected constraint: ${prevKind.mkString("`", ", ", "`")}
           |Found constraint:    ${thisKind.mkString("`", ", ", "`")}""".stripMargin
      ),
    )
class IllegalWildTypeVariable(range: Doc.Range)
    extends RangedError(range, "Wild type variable is not allowed here", None)
class UselessTypeVar(range: Doc.Range, name: String) extends RangedError(range, s"Useless type variable: $name", None)
class DuplicateEnumCon(range: Doc.Range, name: String)
    extends RangedError(range, s"Duplicate enum constructor: $name", None)
class DuplicateType(range: Doc.Range, name: String) extends RangedError(range, s"Duplicate type: $name", None)
class DuplicateStruct(range: Doc.Range, name: String) extends RangedError(range, s"Duplicate struct: #$name{}", None)
class DuplicateFun(range: Doc.Range, name: String) extends RangedError(range, s"Duplicate fun: $name", None)
class UnSpecedExportedFun(range: Doc.Range, name: String)
    extends RangedError(range, s"Fun: $name is exported, but lacks a spec", None)

case class ParseError(pos: Doc.Pos) extends Exception(s"Parse error at ${pos.line}:${pos.column}")
class UnsupportedSyntaxError(range: Doc.Range, reason: String)
    extends RangedError(range, s"Unsupported Syntax", Some(reason))

sealed trait PatternWarning extends RangedError {
  override val severity: Severity = Warning
}
class MissingPatternsWarning(range: Doc.Range, confident: Boolean, exampleClause: String)
    extends RangedError(
      range = range,
      title = (if (confident) "" else "Possibly ") + "Missing Patterns",
      description = Some(s"missing: $exampleClause"),
    )
    with PatternWarning
class UselessPatternWarning(range: Doc.Range)
    extends RangedError(range = range, title = "Useless Pattern", description = None)
    with PatternWarning
