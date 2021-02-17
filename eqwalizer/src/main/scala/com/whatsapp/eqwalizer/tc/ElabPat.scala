package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.BinarySpecifiers
import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.ast.Types._

final class ElabPat(module: String) {
  def elabPats(pats: List[Pat], tys: List[Type], env: Env): (List[Type], Env) = {
    var envAcc = env
    val patTypes = (pats zip tys).map { case (pat, ty) =>
      val (patType, env1) = elabPat(pat, ty, envAcc)
      envAcc = env1
      patType
    }
    (patTypes, envAcc)
  }

  def elabPat(pat: Pat, t: Type, env: Env): (Type, Env) =
    pat match {
      case PatWild() =>
        (t, env)
      case PatVar(v) =>
        val patType = env.get(v) match {
          case Some(vt) => Subtype.meet(vt, t)
          case None     => t
        }
        (patType, env + (v -> patType))
      case PatAtom(s) =>
        val patType = Subtype.meet(AtomLitType(s), t)
        (patType, env)
      case PatNumber() =>
        val patType = Subtype.meet(NumberType, t)
        (patType, env)
      case PatTuple(elems) =>
        Approx.asTupleType(t, elems.size) match {
          case None =>
            var envAcc = env
            elems.foreach { elem =>
              val (_, env1) = elabPat(elem, NoneType, envAcc)
              envAcc = env1
            }
            (NoneType, envAcc)
          case Some(TupleType(elemTypes)) =>
            var envAcc = env
            val patTypes = elems.zip(elemTypes).map { case (elem, elemT) =>
              val (patType, env1) = elabPat(elem, elemT, envAcc)
              envAcc = env1
              patType
            }
            (TupleType(patTypes), envAcc)
        }
      case PatNil() =>
        val patType = Subtype.meet(NilType, t)
        (patType, env)
      case PatCons(hPat, tPat) =>
        Approx.asListType(t) match {
          case None =>
            val (_, env1) = elabPat(hPat, NoneType, env)
            val (_, env2) = elabPat(tPat, NoneType, env1)
            (NoneType, env2)
          case Some(ListType(elemType)) =>
            val (hType, env1) = elabPat(hPat, elemType, env)
            val (tType, env2) = elabPat(tPat, ListType(elemType), env1)
            Approx.asListType(tType) match {
              case None =>
                (NoneType, env2)
              case Some(refinedT) =>
                (ListType(Subtype.join(hType, refinedT)), env2)
            }
        }
      case PatMatch(p1, p2) =>
        val (t1, env1) = elabPat(p1, t, env)
        elabPat(p2, t1, env1)
      case unOp: PatUnOp =>
        elabUnOp(unOp, t, env)
      case binOp: PatBinOp =>
        elabBinOp(binOp, t, env)
      case PatBinary(elems) =>
        val patType = Subtype.meet(BinaryType, t)
        var envAcc = env
        for { elem <- elems } {
          envAcc = elabBinaryElem(elem, envAcc)
        }
        (patType, envAcc)
      case PatRecordIndex(_, _) =>
        val patType = Subtype.meet(integerType, t)
        (patType, env)
      case PatRecord(recName, fields) =>
        val recType = Subtype.meet(RecordType(recName), t)
        val recDecl = Util.getRecord(module, recName).get
        val fieldDecls = recDecl.fields.map(f => f.name -> f).toMap
        var envAcc = env
        for (field <- fields) {
          val fieldDecl = fieldDecls(field.name)
          val (_, env1) = elabPat(field.pat, fieldDecl.tp, envAcc)
          envAcc = env1
        }
        (recType, envAcc)
      case PatMap(kvs) =>
        val mapType = Approx.asMapType(t)
        var envAcc = env
        for ((keyPat, valPat) <- kvs) {
          keyPat match {
            case PatAtom(key) =>
              val (_, env1) = elabPat(valPat, Approx.getValType(key, mapType), envAcc)
              envAcc = env1
            case _ =>
              val (_, env1) = elabPat(keyPat, Approx.getKeyType(mapType), envAcc)
              val (_, env2) = elabPat(valPat, Approx.getValType(mapType), env1)
              envAcc = env2
          }
        }
        (mapType, envAcc)
    }

  private def elabBinaryElem(elem: PatBinaryElem, env: Env): Env = {
    val env1: Env = elem.size match {
      case PatBinSizeVar(pv) => elabPat(pv, integerType, env)._2
      case PatBinSizeConst   => env
    }
    val isStringLiteral = false
    val expType = BinarySpecifiers.expType(elem.specifier, isStringLiteral)
    val (_, env2) = elabPat(elem.pat, expType, env1)
    env2
  }

  private def elabUnOp(pat: PatUnOp, t: Type, env: Env): (Type, Env) = {
    val PatUnOp(op, arg) = pat
    op match {
      case "+" | "-" | "bnot" =>
        val (_, env1) = elabPat(arg, NumberType, env)
        (NumberType, env1)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }

  private def elabBinOp(binOp: PatBinOp, t: Type, env: Env): (Type, Env) = {
    val PatBinOp(op, arg1, arg2) = binOp
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
        val (_, env1) = elabPat(arg1, NumberType, env)
        val (_, env2) = elabPat(arg2, NumberType, env1)
        (NumberType, env2)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }
}
