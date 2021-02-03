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
import com.whatsapp.corq.ast.{Id, RemoteId, Vars, WIPDiagnostics}
import com.whatsapp.corq.tc.TcDiagnostics._
import erlang.CErl._
import erlang.Data._
import com.whatsapp.corq.ast.WIPDiagnostics.SkippedConstructDiagnostics
import com.whatsapp.corq.tc.BuiltIn

final class Elab(val module: String, check: Check) {

  def elabData(parent: CLiteral, data: EObject, env: Env): Type =
    data match {
      case EAtom(atom) =>
        AtomLitType(atom)
      case EExternalFun(remoteId) =>
        Util
          .getCallType(remoteId)
          .getOrElse(throw UnboundRemoteId(parent.anno.line, remoteId))
      case EList(Nil, _) =>
        NilType
      case EList(hd :: tl, _) =>
        val hdType = elabData(parent, hd, env)
        val tlType = elabData(parent, EList(tl, None), env)
        consType(hdType, tlType)
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

  def elabTryHandler(evars: List[CVar], body: CErl, env: Env): (Type, Env) = {
    val handlerTypes = evars zip (evars.size match {
      case 3 =>
        /* Class:Msg:stack */
        BuiltIn.catchMatchWithStackTypes
      case _ =>
        ElabPat.elabPats(evars, List(BuiltIn.catchMatchNoStackType), env)._1
    })
    val env1 = env ++ handlerTypes
    val handlerTy = elabExpr(body, env1)
    (handlerTy, env1)
  }

  def elabExpr(expr: CErl, env: Env): Type =
    expr match {
      case CApply(_, op, args) =>
        val CVar(_, VarNameAtomInt(id)) = op
        Util.getApplyType(module, id) match {
          case Some(FunType(argTys, fResTy)) =>
            check.checkExprs(args, argTys, env)
            fResTy
          case None =>
            throw UnboundId(expr.line, id)
        }
      case CBinary(_, _) =>
        BinaryType
      case CBitstr(_, _, _, _, _, _) =>
        BinaryType
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
            Util
              .getCallType(id2)
              .getOrElse(throw UnboundRemoteId(funExpr.anno.line, id2))
          case _ =>
            val FunType(argTys, funReturnTy) = Util
              .getCallType(id)
              .getOrElse(throw UnboundRemoteId(funExpr.anno.line, id))
            (args zip argTys).foreach {
              case (argExpr, expectedArgTy) =>
                check.checkExpr(argExpr, expectedArgTy, env)
            }
            funReturnTy
        }
      case ccase: CCase =>
        Subtype.joinAll(elabCaseBranches(ccase, env))
      case CCatch(_anno, _body) =>
        AnyType
      case CCons(_, hd, tl) =>
        val hdType = elabExpr(hd, env)
        val tlType = elabExpr(tl, env)
        consType(hdType, tlType)
      case CLet(_, vars, arg, body) =>
        val exprTy = elabExpr(arg, env)
        val env1 = vars match {
          case (cvar @ CVar(_, _)) :: Nil =>
            env + (cvar -> exprTy)
          case _ =>
            val CValuesType(tys) = exprTy
            assert(vars.size == tys.size)
            env ++ (vars zip tys)
        }
        elabExpr(body, env1)
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
        elabData(lit, lit.value, env)
      case primop: CPrimOp =>
        BuiltIn.primOpToReturnType(primop)
      case CSeq(_, arg, body) =>
        check.checkExpr(arg, AnyType, env)
        elabExpr(body, env)
      case ctry: CTry =>
        val (bodyTy, handlerTy) = elabTryBranches(ctry, env)
        Subtype.join(bodyTy, handlerTy)
      case CTuple(_, elems) =>
        val elemTypes = elems.map { elem =>
          elabExpr(elem, env)
        }
        TupleType(elemTypes)
      case CValues(_, exprs) =>
        val tys = exprs.map(elabExpr(_, env))
        CValuesType(tys)
      case cvar: CVar =>
        env(cvar)
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

  def elabCaseBranches(ccase: CCase, env: Env): List[Type] = {
    val CCase(_, sel, clauses) = ccase
    val argTys = elabExpr(sel, env) match {
      case CValuesType(tys) => tys
      case ty               => List(ty)
    }
    val clauseTypes = clauses.map(elabClause(_, argTys, env))
    clauseTypes
  }

  def elabClause(clause: CClause, argTys: List[Type], env: Env): Type = {
    val env1 = Util.initClauseEnv(env, Vars.clauseVars(clause))
    val env2 = ElabGuard(this).elabGuard(clause.guard, env1)
    val (_, env3) = ElabPat.elabPats(clause.pats, argTys, env2)
    elabExpr(clause.body, env3)
  }

  def elabTryBranches(c: CTry, env: Env): (Type, Type) = {
    val argTy = elabExpr(c.arg, env)
    val bodyEnv = ElabPat.elabPats(c.bodyVars, List(argTy), env)._2
    val bodyTy = elabExpr(c.body, bodyEnv)
    val handlerTy = elabTryHandler(c.evars, c.handler, env)._1
    (bodyTy, handlerTy)
  }

  private def getStaticAtom(expr: CErl, env: Env): String =
    elabExpr(expr, env) match {
      case AtomLitType(module) => module
      case moduleExprTy =>
        throw TypeMismatch(expr, AtomLitType("<<known atom>>"), moduleExprTy)
    }

  private def consType(hdType: Type, tlType: Type): Type = {
    val tailElemType = tlType match {
      case NilType      => NoneType
      case ListType(ty) => ty
      case _            => sys.error(s"unexpected $tlType")
    }
    ListType(Subtype.join(hdType, tailElemType))
  }

}
