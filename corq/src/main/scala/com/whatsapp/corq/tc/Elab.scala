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

import com.whatsapp.corq.ast.Types._
import com.whatsapp.corq.ast.{RemoteId, Vars, WIPDiagnostics}
import com.whatsapp.corq.tc.TcDiagnostics._
import erlang.CErl._
import erlang.Data._
import com.whatsapp.corq.ast.WIPDiagnostics.SkippedConstructDiagnostics

final class Elab(val module: String, check: Check) {

  def elabData(parent: CLiteral, data: EObject, env: Env): Type =
    data match {
      case EAtom(atom) =>
        AtomLitType(atom)
      case EExternalFun(remoteId) =>
        Util
          .getCallType(remoteId)
          .getOrElse(throw UnboundRemoteId(parent.anno.line, remoteId))
      case EList(Nil, None) =>
        NilType
      case EList(hd :: tl, None) =>
        val hdType = elabData(parent, hd, env)
        val tlType = elabData(parent, EList(tl, None), env)
        consType(parent, hdType, tlType)
      case _: EBitStr =>
        BinaryType
      case _: EDouble =>
        NumberType
      case ELong(_) =>
        NumberType
      case _: EPid =>
        PidType
      case _: EPort =>
        PortType
      case _: ERef =>
        ReferenceType
      case _: EString =>
        ListType(NumberType)
      case ETuple(elems) =>
        val elemTypes = elems map { elabData(parent, _, env) }
        TupleType(elemTypes)
      case _: EMap =>
        throw SkippedConstructDiagnostics(
          parent.anno.line,
          WIPDiagnostics.ExpMap
        )
      case _ => sys.error(s"unexpected $data")
    }

  private def elabTryHandler(
      evars: List[CVar],
      body: CErl,
      env: Env
  ): (Type, Env) = {
    val handlerTypes = evars.map(_.name) zip (evars.size match {
      case 3 =>
        /* Class:Msg:stack */
        BuiltIn.catchMatchWithStackTypes
      case _ =>
        ElabPat.elabPats(evars, List(BuiltIn.catchMatchNoStackType), env)._1
    })
    val env1 = env ++ handlerTypes
    val (handlerTy, env2) = elabExpr(body, env1)
    (handlerTy, env2)
  }

  def elabExpr(expr: CErl, env: Env): (Type, Env) =
    expr match {
      case CApply(_, cvar @ CVar(Anno(line), varName), args) =>
        val FunType(funArgTys, funResTy) = varName match {
          case VarNameAtomInt(id) =>
            Util.getApplyType(module, id).getOrElse(throw UnboundId(line, id))
          case _ =>
            env.getOrElse(cvar.name, throw UnboundVar(cvar.anno.line, cvar))
        }
        val env1 = check.checkExprs(args, funArgTys, env)
        (funResTy, env1)
      case CBinary(_, _) =>
        (BinaryType, env)
      case CBitstr(_, _, _, _, _, _) =>
        (BinaryType, env)
      case CCall(_, moduleExpr, funExpr, args) =>
        // can allow union types here in the future, but must be atom literal types now
        // which is kind of useless
        val module = getStaticAtom(moduleExpr, env)
        val fun = getStaticAtom(funExpr, env)
        val id = RemoteId(module, fun, args.size)
        (id, args) match {
          // secret erlang:make_fun/3
          case (
                BuiltIn.MakeFun,
                List(LitAtom(eMod), LitAtom(eFun), CLiteral(_, ELong(eArity)))
              ) =>
            val id2 = RemoteId(eMod, eFun, eArity.toInt)
            val ty = Util
              .getCallType(id2)
              .getOrElse(throw UnboundRemoteId(funExpr.anno.line, id2))
            (ty, env)
          case _ =>
            val FunType(argTys, funReturnTy) = Util
              .getCallType(id)
              .getOrElse(throw UnboundRemoteId(funExpr.anno.line, id))
            (args zip argTys).foreach {
              case (argExpr, expectedArgTy) =>
                check.checkExpr(argExpr, expectedArgTy, env)
            }
            (funReturnTy, env)
        }
      case CCase(_, sel, clauses) =>
        val (argTys, env2) = elabExpr(sel, env) match {
          case (CValuesType(tys), e) => (tys, e)
          case (ty, e)               => (List(ty), e)
        }
        if (clauses.isEmpty) (NoneType, env)
        else {
          val (ts, envs) = clauses.map(elabClause(_, argTys, env2)).unzip
          (
            ts reduceLeft Subtype.join,
            Approx.combineEnvs(env2, Subtype.join, envs)
          )
        }
      case CCatch(_, body) =>
        check.checkExpr(body, AnyType, env)
        (AnyType, env)
      case CCons(_, hd, tl) =>
        val (hdType, env1) = elabExpr(hd, env)
        val (tlType, env2) = elabExpr(tl, env1)
        (consType(expr, hdType, tlType), env2)
      case CLet(_, vars, arg, body) =>
        val (argTy, env1) = elabExpr(arg, env)
        argTy match {
          /**
            * type the Let as NoneType if the arg is NoneType.
            * This is so we don't end up with nonsense like:
            * CValuesType(List(NoneType, NilType))
            *
            * For example:
            * `case self() of A when is_atom(A)-> ok end, A`
            * produces Core Erlang like this:
            * let <_2, A> = ....
            *     let <_1> = primop 'match_fail' ({'case_clause',_0}) in  <_1,[]>`
            * The `[]` seems to be CE's choice of dummy value here
            */
          case NoneType =>
            check.checkExpr(
              body,
              AnyType,
              env ++ (vars.map(_.name) zip vars.map(_ => NoneType)).toMap
            )
            (NoneType, env1)
          case _ =>
            val env2 = vars match {
              case (cvar @ CVar(_, _)) :: Nil =>
                env1 + (cvar.name -> argTy)
              case _ =>
                // assertion that the arity of the CValues matches that
                // of the pattern, which I suspect is an invariant of Core Erlang
                val Some(CValuesType(tys)) =
                  Approx.asCValuesType(argTy, vars.size)
                env ++ (vars.map(_.name) zip tys)
            }
            elabExpr(body, env2)
        }
      case CLetRec(_, defs, body) =>
        for ((cvar, fun) <- defs) {
          cvar match {
            case CVar(_, VarNameAtomInt(id)) =>
              val FunType(_, retTy) = BuiltIn.letRecSpecialFunToType(id)
              check.checkFunBody(fun.body, retTy, env)
            case _ => sys.error(s"unexpected fun in letrec $cvar")
          }
        }
        elabExpr(body, env)
      case lit: CLiteral =>
        (elabData(lit, lit.value, env), env)
      case primop: CPrimOp =>
        (BuiltIn.primOpToReturnType(primop), env)
      case CSeq(_, arg, body) =>
        val env1 = check.checkExpr(arg, AnyType, env)
        elabExpr(body, env1)
      case CTry(_, arg, bodyVars, body, evars, handler) =>
        val (argTy, env1) = elabExpr(arg, env)
        val (_, env2) = ElabPat.elabPats(bodyVars, List(argTy), env1)
        val (bodyTy, bodyEnv) = elabExpr(body, env2)
        val (handlerTy, handlerEnv) = elabTryHandler(evars, handler, env1)
        val ty = Subtype.join(bodyTy, handlerTy)
        // TODO: consider including env for `after` call as well. It's in a fun defined in a letrec
        (
          ty,
          Approx.combineEnvs(env, Subtype.join, bodyEnv :: handlerEnv :: Nil)
        )
      case CTuple(_, elems) =>
        var envAcc = env
        val elemTypes = elems.map { elem =>
          val (eType, env1) = elabExpr(elem, envAcc)
          envAcc = env1
          eType
        }
        (TupleType(elemTypes), envAcc)
      case CValues(_, elems) =>
        var envAcc = env
        val elemTypes = elems.map { elem =>
          val (eType, env1) = elabExpr(elem, envAcc)
          envAcc = env1
          eType
        }
        (CValuesType(elemTypes), envAcc)
      case CVar(Anno(line), VarNameAtomInt(id)) =>
        val ty =
          Util.getApplyType(module, id).getOrElse(throw UnboundId(line, id))
        (ty, env)
      case cvar: CVar =>
        val ty =
          env.getOrElse(cvar.name, throw UnboundVar(cvar.anno.line, cvar))
        (ty, env)
      case _: CFun =>
        throw SkippedConstructDiagnostics(
          expr.anno.line,
          WIPDiagnostics.ExpAnonFun
        )
      case _: CMap =>
        throw SkippedConstructDiagnostics(expr.anno.line, WIPDiagnostics.ExpMap)
      case _: CMapPair =>
        throw SkippedConstructDiagnostics(expr.anno.line, WIPDiagnostics.ExpMap)
      case _ => sys.error(s"Unexpected $expr")
    }

  def elabClause(clause: CClause, argTys: List[Type], env: Env): (Type, Env) = {
    val env1 = Util.initClauseEnv(env, Vars.clauseVars(clause))
    val env2 = ElabGuard(this).elabGuard(clause.guard, env1)
    val (_, env3) = ElabPat.elabPats(clause.pats, argTys, env2)
    elabExpr(clause.body, env3)
  }

  private def getStaticAtom(expr: CErl, env: Env): String = {
    elabExpr(expr, env)._1 match {
      case AtomLitType(module) => module
      case moduleExprTy =>
        throw TypeMismatch(expr, AtomLitType("<<known atom>>"), moduleExprTy)
    }
  }

  private def consType(expr: CErl, hdType: Type, tlType: Type): Type = {
    val tailElemType = tlType match {
      case NilType      => NoneType
      case ListType(ty) => ty
      case ty           => throw TypeMismatch(expr, ListType(hdType), ty)
    }
    ListType(Subtype.join(hdType, tailElemType))
  }

}
