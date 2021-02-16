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

package com.whatsapp.coralizer.tc

import com.whatsapp.coralizer.ast.Types._
import com.whatsapp.coralizer.ast.{RemoteId, Vars, WIPDiagnostics}
import com.whatsapp.coralizer.tc.TcDiagnostics._
import erlang.CErl._
import erlang.Data._
import com.whatsapp.coralizer.ast.WIPDiagnostics.SkippedConstructDiagnostics

final class Elab(val module: String, check: Check) {

  def elabData(parent: CLiteral, data: EObject, env: Env): Type =
    data match {
      case EAtom(atom) =>
        AtomLitType(atom)
      case EExternalFun(remoteId) =>
        Util
          .getCallType(remoteId)
          .getOrElse(throw UnboundRemoteId(parent.line, parent, remoteId))
      case EList(Nil) =>
        NilType
      case EList(hd :: tl) =>
        val hdType = elabData(parent, hd, env)
        val tlType = elabData(parent, EList(tl), env)
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
      case CApply(_, cvar @ CVar(_, varName), args) =>
        val FunType(funArgTys, funResTy) = varName match {
          case VarNameAtomInt(id) =>
            env
              .get(varName)
              .orElse(Util.getApplyType(module, id))
              .getOrElse(throw UnboundId(cvar.line, cvar, id))
          case _ =>
            env.getOrElse(cvar.name, throw UnboundVar(cvar.line, cvar))
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
              .getOrElse(throw UnboundRemoteId(funExpr.line, funExpr, id2))
            (ty, env)
          case _ =>
            val FunType(argTys, funReturnTy) = Util
              .getCallType(id)
              .getOrElse(throw UnboundRemoteId(funExpr.line, funExpr, id))
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
        var envAcc = env
        for ((cvar, fun) <- defs) {
          cvar match {
            case CVar(_, name @ VarNameAtomInt(id)) =>
              BuiltIn
                .parseLetRecId(id)
                // $COVERAGE-OFF$
                .getOrElse(sys.error(s"Unexpected letrec fun $id")) match {
                // $COVERAGE-ON$
                case BuiltIn.SpecialReceive(funTy) =>
                  envAcc += (name -> funTy)
                case BuiltIn.SpecialAfter(funTy) =>
                  envAcc += (name -> funTy)
                  envAcc = check.checkFunBody(fun.body, funTy.resTy, envAcc)
                case BuiltIn.SpecialBinaryComp =>
                  assert(defs.size == 1)
                  throw WIPDiagnostics.SkippedConstructDiagnostics(
                    fun.body.line,
                    WIPDiagnostics.BinaryComp
                  )
                case BuiltIn.SpecialListComp =>
                  assert(defs.size == 1)
                  return elabListComp(cvar, fun, body, env)
              }
            // $COVERAGE-OFF$
            case _ => sys.error(s"unexpected variable in letrec $cvar")
            // $COVERAGE-ON$
          }
        }
        elabExpr(body, envAcc)
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
      case CVar(_, VarNameAtomInt(id)) =>
        val ty =
          Util
            .getApplyType(module, id)
            .getOrElse(throw UnboundId(expr.line, expr, id))
        (ty, env)
      case cvar: CVar =>
        val ty =
          env.getOrElse(cvar.name, throw UnboundVar(cvar.line, cvar))
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
        throw TypeMismatch(
          expr.line,
          expr,
          AtomLitType("<<known atom>>"),
          moduleExprTy
        )
    }
  }

  /**
    * The Core Erlang for list comprehensions in roughly:
    *
    *  [doHeadStuff(H) || Pat1, Pat2 <- ListExpression]
    * letrec  lc$01 = fun (List) ->
    *                      case List of
    *                        [Pat1|T]  -> let NewHead = doHeadStuff(Pat1)
    *                                  in [H | lc$01(T)];
    *                        [Pat2|T]  -> let NewHead = doHeadStuff(Pat2)
    *                                  in [H | lc$01(T)];
    *                           [] -> []
    *                        end
    *           end
    *           in lc$01(ListExpression)
    */
  def elabListComp(
      lcVar: CVar,
      lcFun: CFun,
      letBody: CErl,
      env: Env
  ): (Type, Env) = {
    val CApply(_, recurseVar: CVar, arg :: Nil) = letBody
    assert(recurseVar.name == lcVar.name)
    val (genTy, env1) = elabExpr(arg, env)
    if (Subtype.subType(genTy, BinaryType)) {
      throw WIPDiagnostics.SkippedConstructDiagnostics(
        arg.line,
        WIPDiagnostics.BinaryGenerator
      )
    }
    if (!Subtype.subType(genTy, ListType(AnyType))) {
      throw TypeMismatch(arg.line, arg, ListType(AnyType), genTy)
    }
    val CFun(_, CVar(_, argName) :: Nil, funBody) = lcFun
    val env2 = env1 ++ Map(
      argName -> genTy,
      lcVar.name -> FunType(AnyType :: Nil, NilType)
    )
    elabExpr(funBody, env2)
  }

  private def consType(expr: CErl, hdType: Type, tlType: Type): Type = {
    val tailElemType = tlType match {
      case NilType      => NoneType
      case ListType(ty) => ty
      case ty           => throw TypeMismatch(expr.line, expr, ListType(AnyType), ty)
    }
    ListType(Subtype.join(hdType, tailElemType))
  }

}
