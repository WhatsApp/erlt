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

package com.whatsapp.eqwalizer.ast

object Diagnostics {

  case class SkippedConstructDiagnostics(line: Int, construct: SkippedConstruct) extends Exception

  sealed trait SkippedConstruct
  sealed trait SkippedExpr extends SkippedConstruct
  sealed trait SkippedPat extends SkippedConstruct
  sealed trait SkippedType extends SkippedConstruct
  case object SkippedGuard extends SkippedConstruct {
    override def toString: String = "G"
  }
  case object SkippedNilType extends SkippedType {
    override def toString: String = "T: []"
  }
  case object TypeAnyFun extends SkippedType {
    override def toString: String = "T: fun()"
  }
  case object TypeFunAnyArg extends SkippedType {
    override def toString: String = "T: fun((...) -> Type)"
  }
  case object TypeAnyMap extends SkippedType {
    override def toString: String = "T: map()"
  }
  case object TypeMap extends SkippedType {
    override def toString: String = "T: #{...}"
  }
  case class TypeRecord(name: String) extends SkippedType {
    override def toString: String = s"T: #$name{...}"
  }
  case class TypeUnOp(op: String) extends SkippedType {
    override def toString: String = s"T: $op _"
  }
  case class TypeBinOp(op: String) extends SkippedType {
    override def toString: String = s"T: _ $op _"
  }
  case object TypeAnyTuple extends SkippedType {
    override def toString: String = "T: tuple()"
  }
  case object TypeBinary extends SkippedType {
    override def toString: String = "T: binary()"
  }
  case object TypeList extends SkippedType {
    override def toString: String = "T: [...]"
  }
  case class TypePredefined(name: String) extends SkippedType {
    override def toString: String = s"T: $name()"
  }
  case object PatBin extends SkippedPat {
    override def toString: String = s"P: <<...>>"
  }
  case class PatBinOp(op: String) extends SkippedPat {
    override def toString: String = s"P: _ $op _"
  }
  case class PatUnOp(op: String) extends SkippedPat {
    override def toString: String = s"P: $op _"
  }
  case class PatRecord(name: String) extends SkippedPat {
    override def toString: String = s"P: #$name{...}"
  }
  case object PatList extends SkippedPat {
    override def toString: String = "P: [...]"
  }
  case object PatString extends SkippedPat {
    override def toString: String = """P: "...""""
  }
  case object PatMap extends SkippedPat {
    override def toString: String = "P: #{...}"
  }
  case object ExpBin extends SkippedExpr {
    override def toString: String = "E: <<...>>"
  }
  case class ExpBinOp(op: String) extends SkippedExpr {
    override def toString: String = s"E: _ $op _"
  }
  case class ExpUnOp(op: String) extends SkippedExpr {
    override def toString: String = s"E: $op _"
  }
  case class ExpRecord(name: String) extends SkippedExpr {
    override def toString: String = s"E: #$name{...}"
  }
  case object ExpMap extends SkippedExpr {
    override def toString: String = "E: #{...}"
  }
  case object ExpCatch extends SkippedExpr {
    override def toString: String = "E: catch"
  }
  case object ExpLC extends SkippedExpr {
    override def toString: String = "E: [ || ]"
  }
  case object ExpBC extends SkippedExpr {
    override def toString: String = "E: << || >>"
  }
  case object ExpIf extends SkippedExpr {
    override def toString: String = "E: if"
  }
  case object ExpList extends SkippedExpr {
    override def toString: String = "E: [...]"
  }
  case object ExpString extends SkippedExpr {
    override def toString: String = """E: "...""""
  }
  case object ExpTry extends SkippedExpr {
    override def toString: String = "E: try"
  }
  case object ExpReceive extends SkippedExpr {
    override def toString: String = "E: receive"
  }
  case object ExpDynFun extends SkippedExpr {
    override def toString: String = "E: dynfun"
  }
  case object ExpDCall extends SkippedExpr {
    override def toString: String = "E: dcall"
  }
  case object ExpAnonFun extends SkippedExpr {
    override def toString: String = "E: lambda"
  }
  case object ExpNamedFun extends SkippedExpr {
    override def toString: String = "E: named_fun"
  }
}
