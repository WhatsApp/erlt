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

package com.whatsapp.corq.ast

// These are diagnostics about work in progress:
// What is supported and what is not supported yet.
// Later - when all the constructs are supported,
// these items would not be needed at all.
object WIPDiagnostics {

  case class SkippedConstructDiagnostics(line: Int, construct: SkippedConstruct)
      extends Exception

  sealed trait SkippedConstruct
  sealed trait SkippedExpr extends SkippedConstruct
  sealed trait SkippedPat extends SkippedConstruct
  sealed trait SkippedType extends SkippedConstruct
  sealed trait SkippedGuard extends SkippedConstruct
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
  case class TypePredefined(name: String) extends SkippedType {
    override def toString: String = s"T: $name()"
  }
  case object TypeIntersection extends SkippedType {
    override def toString: String = "Intersection"
  }
  case object PatListConcat extends SkippedPat {
    override def toString: String = "P: _ ++ _"
  }
  case class PatRecord(name: String) extends SkippedPat {
    override def toString: String = s"P: #$name{...}"
  }
  case object PatString extends SkippedPat {
    override def toString: String = """P: "...""""
  }
  case object PatMap extends SkippedPat {
    override def toString: String = "P: #{...}"
  }
  case class ExpRecord(name: String) extends SkippedExpr {
    override def toString: String = s"E: #$name{...}"
  }
  case object ExpMap extends SkippedExpr {
    override def toString: String = "E: #{...}"
  }
  case object ExpLC extends SkippedExpr {
    override def toString: String = "E: [ || ]"
  }
  case object ExpBC extends SkippedExpr {
    override def toString: String = "E: << || >>"
  }
  case object ExpString extends SkippedExpr {
    override def toString: String = """E: "...""""
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
  case object ExpListConcat extends SkippedExpr {
    override def toString: String = "E: _ ++ _"
  }
  case object ExpListSubtract extends SkippedExpr {
    override def toString: String = "E: _ -- _"
  }
  case class TestRecord(name: String) extends SkippedGuard {
    override def toString: String = s"G: #$name{...}"
  }
  case object TestMap extends SkippedExpr {
    override def toString: String = "G: #{...}"
  }
  case object TestString extends SkippedExpr {
    override def toString: String = """G: "...""""
  }

  sealed trait ExpansionFailure {
    def diag: String
  }
  case class RecursiveType(id: RemoteId)
      extends Exception
      with ExpansionFailure {
    def diag: String = s"R: $id"
  }
  case class UnknownId(id: RemoteId) extends Exception with ExpansionFailure {
    def diag: String = s"U: $id"
  }
  case class RecursiveConstraint(n: String)
      extends Exception
      with ExpansionFailure {
    def diag: String = s"C: $n"
  }
}
