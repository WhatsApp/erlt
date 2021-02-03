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

import com.whatsapp.corq.ast.Forms.SkippedFunDecl
import com.whatsapp.corq.ast.Types._
import com.whatsapp.corq.ast.WIPDiagnostics
import com.whatsapp.corq.ast.WIPDiagnostics.ExpString
import erlang.CErl._
import erlang.Data._

import scala.annotation.tailrec

case class ElabGuard(elab: Elab) {

  type EnvMap = Map[CVar, Env]

  val predicateToType = Map[String, Type](
    "is_atom" -> AtomType,
    "is_boolean" -> booleanType,
    "is_float" -> floatType,
    "is_integer" -> integerType,
    "is_number" -> NumberType,
    "is_pid" -> PidType,
    "is_port" -> PortType,
    "is_reference" -> ReferenceType,
    // these aren't correct: unknown length list and tuple are not yet implemented
    "is_list" -> AnyType,
    "is_tuple" -> AnyType,
    "bnot" -> integerType
  )

  private def elabGuardCall(
      op: String,
      args: List[CErl],
      env: Env,
      envs: EnvMap
  ): (Env, EnvMap) =
    if (op == "not")
      (env, envs) // skip, not doing negated types yet
    else
      args match {
        case (cvar: CVar) :: Nil =>
          val predTy = predicateToType.getOrElse(
            op,
            throw new IllegalStateException(s"unrecognized guard op $op")
          )
          val meetTy = Subtype.meet(predTy, env(cvar))
          (env + (cvar -> meetTy), envs)
        case arg :: Nil =>
          (elabGuardUnOp(op, arg, env), envs)
        case List(left: CVar, right: CVar)
            if envs.contains(left) && envs.contains(left) =>
          val typeOp = op match {
            case "or"  => Subtype.join _
            case "and" => Subtype.meet _
          }
          val combined = Env.combine(env, typeOp, List(envs(left), envs(right)))
          (combined, envs)
        case List(arg1, arg2) =>
          (elabGuardBinOp(op, arg1, arg2, env), envs)
        // $COVERAGE-OFF$
        case x => sys.error(s"unexpected $x")
        // $COVERAGE-ON$
      }

  def elabGuard(guard: CErl, env: Env): Env =
    // optimization to consider: pre-walk the guard to figure out which vars
    // are narrowed, so we can work with smaller environments
    elabGuardChain(guard, env, Map.empty)._1

  /**
    * let 1 = test
    *   in let 2 = test
    *     in  let 3 = test
    *         in let 4 =  2 or 3 (join the envs represented by 2 and 3)
    *            in 1 and 2 (take the meet)
    */
  private def elabGuardChain(
      guard: CErl,
      env: Env,
      envs: Map[CVar, Env]
  ): (Env, EnvMap) = {
    guard match {
      case CLet(_, List(cvar: CVar), expr, body) =>
        val (env1, _) = elabGuardChain(expr, env, envs)
        val effects = envs + (cvar -> env1)
        val nextEnv = expr match {
          case ErlangCall(op, _) if predicateToType contains op =>
            // use original env so we don't mix the effects
            // of type tests on both branches
            env
          case _ =>
            env1
        }
        elabGuardChain(body, nextEnv, effects)
      case _: CLiteral | _: CVar => (env, envs)
      case ErlangCall(op, args) =>
        elabGuardCall(op, args, env, envs)
      case ccase: CCase =>
        (elabGuardCase(ccase, env), envs)
      case CTry(
            _,
            arg,
            /* bodyVars */ CVar(_, VarNameAtom("Try")) :: Nil,
            /* body */ CVar(Anno(0), VarNameAtom("Try")),
            /* evars */ List(
              CVar(Anno(0), VarNameAtom("T")),
              CVar(Anno(0), VarNameAtom("R"))
            ),
            handler
          ) =>
        val env1 = elabGuard(arg, env)
        elabGuardChain(handler, env1, envs)
      // $COVERAGE-OFF$
      case x => sys.error(s"unexpected $x")
      // $COVERAGE-ON$
    }
  }

  /**
    * Rather than do anything very advanced, we cheat and figure
    * out whether we are in an `andalso` or an `orelse`
    *
    * andalso:
    *
    * `test04(X, Y) when X andalso Y -> ok` has a case like this:
    *         let 4 = case X of
    *         true -> Y
    *         false -> false     %%% NOTICE THIS PART: false -> false
    *         _ -> error()
    *
    *  orelse:
    *
    * `test04(X, Y) when X orelse Y -> ok` has a case like this:
    *         let 4 = case X of
    *         true -> true   %%% NOTICE THIS PART: true -> true
    *         false -> Y
    *         _ -> error()
    */
  private def elabGuardCase(ccase: CCase, env0: Env): Env = {
    var env = env0
    val CCase(_, sel, clauses) = ccase
    env = elabGuard(sel, env)
    val isAndAlso = clauses exists {
      case CClause(_, List(LitAtom("false")), _, LitAtom("false")) => true
      case _                                                       => false
    }
    val isOrElse = clauses exists {
      case CClause(_, List(LitAtom("true")), _, LitAtom("true")) => true
      case _                                                     => false
    }
    assert(!(isAndAlso && isOrElse))
    val (op, leftBoolTy, rightBoolTy) = if (isAndAlso) {
      (Subtype.meet _, trueType, trueType)
    } else if (isOrElse) {
      // approximation: we know at least one is true, but can't represent that
      (Subtype.join _, booleanType, booleanType)
    } else {
      // $COVERAGE-OFF$
      sys.error("should be unreachable")
      // $COVERAGE-ON$
    }
    val clauseEnvs = clauses map (_.body) flatMap {
      case ErlangCall("error", _) =>
        None
      case c => Some(elabGuard(c, env))
    }
    env = Env.combine(env, op, clauseEnvs)
    sel match {
      case cvar: CVar => env += (cvar -> leftBoolTy)
      case _          => env
    }
    clauses map (_.body) foreach {
      case cvar: CVar => env += (cvar -> rightBoolTy)
      case _          => ()
    }
    env
  }

  private def elabTestT(test: CErl, t: Type, env: Env): Env = {
    test match {
      case cvar: CVar =>
        val testType = env.get(cvar) match {
          case Some(vt) =>
            Subtype.meet(vt, t)
          case None => t
        }
        env + (cvar -> testType)
      case _ => env
    }
  }

  def elabGuardUnOp(op: String, arg: CErl, env: Env): Env = {
    op match {
      case "not"              => elabTestT(arg, booleanType, env)
      case "bnot" | "+" | "-" => elabTestT(arg, NumberType, env)
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $op")
      // $COVERAGE-ON$
    }
  }

  private def elabGuardBinOp(
      op: String,
      arg1: CErl,
      arg2: CErl,
      env: Env
  ): Env = {
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" |
          "bsl" | "bsr" =>
        val env1 = elabTestT(arg1, NumberType, env)
        val env2 = elabTestT(arg2, NumberType, env1)
        env2
      // TODO: can this be combined with the next case?
      case "or" | "and" | "xor" =>
        val env1 = elabTestT(arg1, booleanType, env)
        val env2 = elabTestT(arg2, booleanType, env1)
        env2
      case "==" | "=:=" =>
        val arg2Ty = elab.elabExpr(arg2, env)
        elabTestT(arg1, arg2Ty, env)
      case "op" | "/=" | "=<" | "<" | ">=" | ">" =>
        env
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $op")
      // $COVERAGE-ON$
    }
  }

}
