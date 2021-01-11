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

import com.whatsapp.eqwalizer.ast.Diagnostics._
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.io.EData._

object Convert {
  def convertForm(term: EObject): Option[Form] =
    term match {
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("module"), EAtom(name))) =>
        Some(Module(name)(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("export"), EList(ids, None))) =>
        Some(Export(ids.map(convertId))(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("import"), ETuple(List(EAtom(m), EList(ids, None))))) =>
        Some(Import(m, ids.map(convertId))(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("export_type"), EList(typesIds, None))) =>
        Some(ExportType(typesIds.map(convertId))(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("record"), ETuple(List(EAtom(name), EList(_, None))))) =>
        Some(SkippedRecordDecl(name)(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("file"), ETuple(List(EString(file), ELong(start))))) =>
        Some(File(file, start.intValue)(line.intValue))
      case ETuple(List(EAtom("eof"), ELong(line))) =>
        Some(IgnoredForm()(line.intValue))
      case ETuple(List(EAtom("attribute"), ELong(l), EAtom("type"), ETuple(List(EAtom(n), body, EList(vs, None))))) =>
        val id = Id(n, vs.size)
        try Some(TypeDecl(id, vs.map(varString), convertType(body))(l.intValue))
        catch { case d: SkippedConstructDiagnostics => Some(SkippedTypeDecl(id, d)(l.intValue)) }
      case ETuple(List(EAtom("attribute"), ELong(line), EAtom("spec"), ETuple(List(eFunId, EList(eTypeList, None))))) =>
        try Some(FunSpec(convertId(eFunId), eTypeList.map(convertFunSpecType))(line.intValue))
        catch { case d: SkippedConstructDiagnostics => Some(SkippedFunSpec(convertId(eFunId), d)(line.intValue)) }
      case ETuple(EAtom("attribute") :: ELong(line) :: _) =>
        Some(IgnoredForm()(line.intValue))
      case ETuple(EAtom("attribute") :: _) =>
        None
      case ETuple(List(EAtom("function"), ELong(line), EAtom(name), ELong(arity), EList(clauseSeq, None))) =>
        val funId = Id(name, arity.intValue)
        try Some(FunDecl(funId, clauseSeq.map(convertClause))(line.intValue))
        catch { case d: SkippedConstructDiagnostics => Some(SkippedFunDecl(funId, d)(line.intValue)) }
      case _ =>
        sys.error(s"unexpected term: $term")
    }

  private def varString(term: EObject): String = {
    val ETuple(List(EAtom("var"), _, EAtom(name))) = term
    name
  }

  private def convertId(term: EObject): Id =
    term match {
      case ETuple(List(EAtom(name), ELong(arity)))    => Id(name, arity.intValue)
      case ETuple(List(_, EAtom(name), ELong(arity))) => Id(name, arity.intValue)
      case _                                          => sys.error(s"unexpected id: $term")
    }

  private def atomLit(term: EObject): String = {
    val ETuple(List(EAtom("atom"), _, EAtom(atomVal))) = term
    atomVal
  }

  private def convertType(term: EObject): Type =
    term match {
      case ETuple(List(EAtom("ann_type"), _, EList(List(af_anno, tp), None))) =>
        convertType(tp)
      case ETuple(List(EAtom("atom"), _, EAtom(atomVal))) =>
        AtomLitType(atomVal)
      case ETuple(List(EAtom("type"), ELong(line), EAtom("nil"), EList(List(), None))) =>
        throw SkippedConstructDiagnostics(line.intValue, SkippedNilType)
      case ETuple(List(EAtom("type"), ELong(line), EAtom("fun"), EList(List(), None))) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeAnyFun)
      case ETuple(
            List(
              EAtom("type"),
              ELong(line),
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("any"))), _), _),
            )
          ) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeFunAnyArg)
      case ETuple(
            List(
              EAtom("type"),
              _,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        FunType(args.map(convertType), convertType(resultType))
      case ETuple(List(EAtom("type"), ELong(line), EAtom("range"), EList(List(_, _), None))) =>
        NumberType
      case ETuple(List(EAtom("type"), ELong(line), EAtom("map"), EAtom("any"))) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeAnyMap)
      case ETuple(List(EAtom("type"), ELong(line), EAtom("map"), EList(assocTypes, _))) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeMap)
      case ETuple(List(EAtom("type"), ELong(line), EAtom("record"), EList(recordNameLit :: eFieldTypes, None))) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeRecord(atomLit(recordNameLit)))
      case ETuple(List(EAtom("remote_type"), _, EList(List(moduleLit, typeNameLit, EList(args, None)), None))) =>
        RemoteType(
          RemoteId(atomLit(moduleLit), atomLit(typeNameLit), args.size),
          args.map(convertType),
        )
      case ETuple(List(EAtom("user_type"), _, EAtom(name), EList(params, None))) =>
        LocalType(Id(name, params.size), params.map(convertType))
      case ETuple(List(EAtom("integer"), _, ELong(_))) =>
        NumberType
      case ETuple(List(EAtom("char"), _, ELong(_))) =>
        NumberType
      case ETuple(List(EAtom("op"), ELong(line), EAtom(op), _)) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeUnOp(op))
      case ETuple(List(EAtom("op"), ELong(line), EAtom(op), _, _)) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeBinOp(op))
      case ETuple(List(EAtom("type"), ELong(line), EAtom("tuple"), EAtom("any"))) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeAnyTuple)
      case ETuple(List(EAtom("type"), _, EAtom("tuple"), EList(types, None))) =>
        TupleType(types.map(convertType))
      case ETuple(List(EAtom("type"), _, EAtom("union"), EList(types, None))) =>
        UnionType(types.map(convertType))
      case ETuple(List(EAtom("var"), _, EAtom("_"))) =>
        AnyType
      case ETuple(List(EAtom("var"), _, EAtom(name))) =>
        VarType(name)
      case ETuple(List(EAtom("type"), ELong(line), EAtom("list"), _)) =>
        throw SkippedConstructDiagnostics(line.intValue, TypeAnyTuple)
      case ETuple(List(EAtom("type"), ELong(line), EAtom(name), EList(List(), None))) =>
        Types.builtinTypes.get(name) match {
          case Some(ty) => ty
          case None     => throw SkippedConstructDiagnostics(line.intValue, Diagnostics.TypePredefined(name))
        }
      case ETuple(List(EAtom("type"), ELong(line), EAtom(name), EList(_, None))) =>
        throw SkippedConstructDiagnostics(line.intValue, Diagnostics.TypePredefined(name))
      case _ =>
        sys.error(s"unexpected term for type: $term")
    }

  private def convertFunSpecType(term: EObject): ConstrainedFunType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              ELong(line),
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        ConstrainedFunType(FunType(args.map(convertType), convertType(resultType)), List.empty)
      case ETuple(
            List(EAtom("type"), _anno, EAtom("bounded_fun"), EList(List(eFunType, EList(constraints, None)), None))
          ) =>
        convertType(eFunType) match {
          case ft: FunType => ConstrainedFunType(ft, constraints.map(convertConstraint))
          case _           => throw new IllegalStateException()
        }
      case _ => throw new IllegalStateException()
    }

  private def convertConstraint(term: EObject): Constraint =
    term match {
      case ETuple(
            List(EAtom("type"), _, EAtom("constraint"), EList(List(isSubtypeLit, EList(List(eVar, t), None)), None))
          ) =>
        val "is_subtype" = atomLit(isSubtypeLit)
        Constraint(varString(eVar), convertType(t))
      case _ => throw new IllegalStateException()
    }

  private def convertClause(term: EObject): Clause = {
    val ETuple(List(EAtom("clause"), ELong(l), EList(ePats, None), EList(eGuards, None), EList(eExps, None))) = term
    if (eGuards.isEmpty) Clause(ePats.map(convertPat), eExps.map(convertExp))(l.intValue)
    else throw SkippedConstructDiagnostics(l.intValue, Diagnostics.SkippedGuard)
  }

  def convertExp(term: EObject): Expr =
    term match {
      case ETuple(List(EAtom("match"), ELong(l), ePat1, eExp)) =>
        Match(convertPat(ePat1), convertExp(eExp))(l.intValue)
      case ETuple(List(EAtom("var"), ELong(l), EAtom(name))) =>
        Var(name)(l.intValue)
      case ETuple(List(EAtom("tuple"), ELong(l), EList(eExps, None))) =>
        Tuple(eExps.map(convertExp))(l.intValue)
      case ETuple(List(EAtom("nil"), ELong(l))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpList)
      case ETuple(List(EAtom("cons"), ELong(l), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpList)
      case ETuple(List(EAtom("bin"), ELong(l), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpBin)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpBinOp(op))
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpUnOp(op))
      case ETuple(List(EAtom("record"), ELong(l), EAtom(recordName), EList(eRecordFieldExps, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpRecord(recordName))
      case ETuple(List(EAtom("record"), ELong(l), eExp, EAtom(recordName), EList(eRecordFieldExps, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpRecord(recordName))
      case ETuple(List(EAtom("record_index"), ELong(l), EAtom(recordName), eFieldName)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpRecord(recordName))
      case ETuple(List(EAtom("record_field"), ELong(l), eExp, EAtom(recordName), eFieldName)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpRecord(recordName))
      case ETuple(List(EAtom("map"), ELong(l), EList(eAssocs, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpMap)
      case ETuple(List(EAtom("map"), ELong(l), eExp, EList(eAssocs, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpMap)
      case ETuple(List(EAtom("catch"), ELong(l), eExp)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpCatch)
      case ETuple(List(EAtom("call"), ELong(l), eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), ELong(_), EAtom(m))),
                  ETuple(List(EAtom("atom"), ELong(_), EAtom(f))),
                )
              ) =>
            RemoteCall(RemoteId(m, f, eArgs.size), eArgs.map(convertExp))(l.intValue)
          case ETuple(List(EAtom("atom"), ELong(_), EAtom(fname))) =>
            LocalCall(Id(fname, eArgs.size), eArgs.map(convertExp))(l.intValue)
          case _ =>
            throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpDCall)
        }
      case ETuple(List(EAtom("lc"), ELong(l), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpLC)
      case ETuple(List(EAtom("bc"), ELong(l), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpBC)
      case ETuple(List(EAtom("block"), ELong(l), EList(eExps, None))) =>
        Block(eExps.map(convertExp))(l.intValue)
      case ETuple(List(EAtom("if"), ELong(l), EList(eClauses, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpIf)
      case ETuple(List(EAtom("case"), ELong(l), eExp, EList(eClauses, None))) =>
        Case(convertExp(eExp), eClauses.map(convertClause))(l.intValue)
      case ETuple(
            List(
              EAtom("try"),
              ELong(l),
              EList(eExps1, None),
              EList(eClauses1, None),
              EList(eClauses2, None),
              EList(eExps2, None),
            )
          ) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpTry)
      case ETuple(List(EAtom("receive"), ELong(l), EList(eClauses, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpReceive)
      case ETuple(List(EAtom("receive"), ELong(l), EList(eClauses, None), eTimeout, EList(defaults, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpReceive)
      case ETuple(List(EAtom("fun"), ELong(l), eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("clauses"), EList(eClauses, None))) =>
            throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpAnonFun)
          case ETuple(List(EAtom("function"), EAtom(name), ELong(arity))) =>
            LocalFun(Id(name, arity.intValue))(l.intValue)
          case ETuple(
                List(
                  EAtom("function"),
                  ETuple(List(EAtom("atom"), _, EAtom(module))),
                  ETuple(List(EAtom("atom"), _, EAtom(name))),
                  ETuple(List(EAtom("integer"), _, ELong(arity))),
                )
              ) =>
            RemoteFun(RemoteId(module, name, arity.intValue))(l.intValue)
          case ETuple(List(EAtom("function"), eModule, eName, eArity)) =>
            throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpDynFun)
          case _ => throw new IllegalStateException()
        }
      case ETuple(List(EAtom("named_fun"), ELong(l), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpNamedFun)
      case ETuple(List(EAtom("atom"), ELong(l), EAtom(value))) =>
        AtomLit(value)(l.intValue)
      case ETuple(List(EAtom("char" | "float" | "integer"), ELong(l), _)) =>
        NumberLit()(l.intValue)
      case ETuple(List(EAtom("string"), ELong(l), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.ExpString)
      case _ =>
        sys.error(s"cannot parse expr: $term")
    }

  private def convertPat(term: EObject): Pat =
    term match {
      case ETuple(List(EAtom("match"), ELong(l), ePat1, ePat2)) =>
        PatMatch(convertPat(ePat1), convertPat(ePat2))(l.intValue)
      case ETuple(List(EAtom("var"), ELong(l), EAtom("_"))) =>
        PatWild()(l.intValue)
      case ETuple(List(EAtom("var"), ELong(l), EAtom(name))) =>
        PatVar(name)(l.intValue)
      case ETuple(List(EAtom("tuple"), ELong(l), EList(ePats, None))) =>
        PatTuple(ePats.map(convertPat))(l.intValue)
      case ETuple(List(EAtom("nil"), ELong(l))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatList)
      case ETuple(List(EAtom("cons"), ELong(l), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatList)
      case ETuple(List(EAtom("atom"), ELong(l), EAtom(value))) =>
        PatAtom(value)(l.intValue)
      case ETuple(List(EAtom("char" | "float" | "integer"), ELong(l), _)) =>
        PatNumber()(l.intValue)
      case ETuple(List(EAtom("string"), ELong(l), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatString)
      case ETuple(List(EAtom("bin"), ELong(l), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatBin)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), _, _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatBinOp(op))
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatUnOp(op))
      case ETuple(List(EAtom("record"), ELong(l), EAtom(name), EList(eRecordFieldPatterns, None))) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatRecord(name))
      case ETuple(List(EAtom("record_index"), ELong(l), EAtom(name), eFieldName)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatRecord(name))
      case ETuple(List(EAtom("map"), ELong(l), _)) =>
        throw SkippedConstructDiagnostics(l.intValue, Diagnostics.PatMap)
      case _ =>
        sys.error(s"cannot parse pattern: $term")
    }
}
