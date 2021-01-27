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

package com.whatsapp.corq.tc

import com.whatsapp.corq.ast.Guards._
import com.whatsapp.corq.ast.Id
import com.whatsapp.corq.ast.Types._

object ElabGuard {
//  private def elabPredicateType(pred: String): Option[Type] = pred match {
//    case "is_atom"      => Some(AtomType)
//    case "is_boolean"   => Some(booleanType)
//    case "is_float"     => Some(floatType)
//    case "is_integer"   => Some(integerType)
//    case "is_number"    => Some(NumberType)
//    case "is_pid"       => Some(PidType)
//    case "is_port"      => Some(PortType)
//    case "is_reference" => Some(ReferenceType)
//    case _              => None
//  }
//
//  def elabGuards(guards: List[Guard], env: Env): Env =
//    if (guards.isEmpty) env
//    else Approx.joinEnvsAll(guards.map(elabGuard(_, env)))
//
//  private def elabGuard(guard: Guard, env: Env): Env = {
//    var envAcc = env
//    guard.tests.foreach { test =>
//      envAcc = elabTestT(test, trueType, envAcc)
//    }
//    envAcc
//  }
//
//  private def elabTest(test: Test, env: Env): Env =
//    test match {
//      case TestVar(_) | TestAtom(_) | TestNumber() | TestTuple(_) | TestNil() | TestCons(_, _) =>
//        env
//      case TestLocalCall(_, _) =>
//        env
//      case unOp: TestUnOp =>
//        elabUnOp(unOp, env)
//      case binOp: TestBinOp =>
//        elabBinOp(binOp, env)
//      case TestBinaryLit() =>
//        env
//    }
//
//  private def elabTestT(test: Test, t: Type, env: Env): Env =
//    test match {
//      case TestVar(v) =>
//        val testType = env.get(v) match {
//          case Some(vt) => Subtype.meet(vt, t)
//          case None     => t
//        }
//        env + (v -> testType)
//      case TestLocalCall(Id(f, 1), List(TestVar(v))) if Subtype.eqv(trueType, t) =>
//        elabUnaryPredicate(f, v, env)
//      case TestBinOp("andalso", arg1, arg2) =>
//        val env1 = elabTestT(arg1, AtomLitType("true"), env)
//        val env2 = elabTestT(arg2, t, env1)
//        env2
//      case TestBinOp("orelse", arg1, _) =>
//        val env1 = elabTestT(arg1, booleanType, env)
//        env1
//      case _ =>
//        elabTest(test, env)
//    }
//
//  private def elabUnaryPredicate(f: String, v: String, env: Env): Env =
//    elabPredicateType(f) match {
//      case None     => env
//      case Some(pt) => env + (v -> Subtype.meet(pt, env(v)))
//    }
//
//  def elabUnOp(unOp: TestUnOp, env: Env): Env = {
//    val TestUnOp(op, arg) = unOp
//    op match {
//      case "not"              => elabTestT(arg, booleanType, env)
//      case "bnot" | "+" | "-" => elabTestT(arg, NumberType, env)
//      // $COVERAGE-OFF$
//      case _ => throw new IllegalStateException()
//      // $COVERAGE-ON$
//    }
//  }
//
//  private def elabBinOp(binOp: TestBinOp, env: Env): Env = {
//    val TestBinOp(op, arg1, arg2) = binOp
//    op match {
//      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
//        val env1 = elabTestT(arg1, NumberType, env)
//        val env2 = elabTestT(arg2, NumberType, env1)
//        env2
//      case "or" | "and" | "xor" =>
//        val env1 = elabTestT(arg1, booleanType, env)
//        val env2 = elabTestT(arg2, booleanType, env1)
//        env2
//      case _ =>
//        val env1 = elabTest(arg1, env)
//        val env2 = elabTest(arg2, env1)
//        env2
//    }
//  }
}
