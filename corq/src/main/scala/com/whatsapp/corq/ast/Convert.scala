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

import com.whatsapp.corq.ast.BinarySpecifiers._
import com.whatsapp.corq.ast.Exprs._
import com.whatsapp.corq.ast.Forms._
import com.whatsapp.corq.ast.Guards._
import com.whatsapp.corq.ast.Pats._
import com.whatsapp.corq.ast.Types._
import com.whatsapp.corq.io.EData._

object Convert {
  private val specifiers: Map[String, Specifier] =
    Map(
      "default" -> UnsignedIntegerSpecifier,
      "integer" -> UnsignedIntegerSpecifier,
      "float" -> FloatSpecifier,
      "binary" -> BinarySpecifier,
      "bytes" -> BytesSpecifier,
      "bitstring" -> BitstringSpecifier,
      "bits" -> BitsSpecifier,
      "utf8" -> Utf8Specifier,
      "utf16" -> Utf16Specifier,
      "utf32" -> Utf32Specifier
    )

  def convertForm(term: EObject): Option[Form] =
    term match {
      case ETuple(
            List(EAtom("attribute"), ELong(line), EAtom("module"), EAtom(name))
          ) =>
        Some(Module(name)(line.intValue))
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("export"),
              EList(ids, None)
            )
          ) =>
        Some(Export(ids.map(convertIdInAttr))(line.intValue))
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("import"),
              ETuple(List(EAtom(m), EList(ids, None)))
            )
          ) =>
        Some(Import(m, ids.map(convertIdInAttr))(line.intValue))
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("export_type"),
              EList(typesIds, None)
            )
          ) =>
        Some(ExportType(typesIds.map(convertIdInAttr))(line.intValue))
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("record"),
              ETuple(List(EAtom(name), EList(_, None)))
            )
          ) =>
        Some(SkippedRecordDecl(name)(line.intValue))
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("file"),
              ETuple(List(EString(file), ELong(start)))
            )
          ) =>
        Some(File(file, start.intValue)(line.intValue))
      case ETuple(List(EAtom("eof"), ELong(line))) =>
        None
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(l),
              EAtom("type"),
              ETuple(List(EAtom(n), body, EList(vs, None)))
            )
          ) =>
        val id = Id(n, vs.size)
        try Some(TypeDecl(id, vs.map(varString), convertType(body))(l.intValue))
        catch {
          case d: WIPDiagnostics.SkippedConstructDiagnostics =>
            Some(SkippedTypeDecl(id, d)(l.intValue))
        }
      case ETuple(
            List(
              EAtom("attribute"),
              ELong(line),
              EAtom("spec"),
              ETuple(List(eFunId, EList(eTypeList, None)))
            )
          ) =>
        try {
          if (eTypeList.size > 1)
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              line.intValue,
              WIPDiagnostics.TypeIntersection
            )
          Some(
            FunSpec(convertIdInAttr(eFunId), eTypeList.map(convertFunSpecType))(
              line.intValue
            )
          )
        } catch {
          case d: WIPDiagnostics.SkippedConstructDiagnostics =>
            Some(SkippedFunSpec(convertIdInAttr(eFunId), d)(line.intValue))
        }
      case ETuple(EAtom("attribute") :: _) =>
        None
      case ETuple(
            List(
              EAtom("function"),
              ELong(line),
              EAtom(name),
              ELong(arity),
              EList(clauseSeq, None)
            )
          ) =>
        val funId = Id(name, arity.intValue)
        try Some(FunDecl(funId, clauseSeq.map(convertClause))(line.intValue))
        catch {
          case d: WIPDiagnostics.SkippedConstructDiagnostics =>
            Some(SkippedFunDecl(funId, d)(line.intValue))
        }
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def varString(term: EObject): String = {
    val ETuple(List(EAtom("var"), _, EAtom(name))) = term
    name
  }

  private def convertIdInAttr(term: EObject): Id =
    term match {
      case ETuple(List(EAtom(name), ELong(arity))) =>
        Id(name, arity.intValue)
      case ETuple(List(EAtom(module), EAtom(name), ELong(arity))) =>
        // it should be the same module by construction, -> localizing
        Id(name, arity.intValue)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
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
      case ETuple(List(EAtom("type"), _, EAtom("nil"), EList(List(), None))) =>
        NilType
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom("fun"), EList(List(), None))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeAnyFun
        )
      case ETuple(
            List(
              EAtom("type"),
              ELong(line),
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("any"))), _), _)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeFunAnyArg
        )
      case ETuple(
            List(
              EAtom("type"),
              _,
              EAtom("fun"),
              EList(
                List(
                  ETuple(
                    List(EAtom("type"), _, EAtom("product"), EList(args, None))
                  ),
                  resultType
                ),
                None
              )
            )
          ) =>
        FunType(args.map(convertType), convertType(resultType))
      case ETuple(
            List(
              EAtom("type"),
              ELong(line),
              EAtom("range"),
              EList(List(_, _), None)
            )
          ) =>
        NumberType
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom("map"), EAtom("any"))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeAnyMap
        )
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom("map"), EList(assocTypes, _))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeMap
        )
      case ETuple(
            List(
              EAtom("type"),
              ELong(line),
              EAtom("record"),
              EList(recordNameLit :: eFieldTypes, None)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeRecord(atomLit(recordNameLit))
        )
      case ETuple(
            List(
              EAtom("remote_type"),
              _,
              EList(List(moduleLit, typeNameLit, EList(args, None)), None)
            )
          ) =>
        RemoteType(
          RemoteId(atomLit(moduleLit), atomLit(typeNameLit), args.size),
          args.map(convertType)
        )
      case ETuple(
            List(EAtom("user_type"), _, EAtom(name), EList(params, None))
          ) =>
        LocalType(Id(name, params.size), params.map(convertType))
      case ETuple(List(EAtom("integer"), _, ELong(_))) =>
        NumberType
      case ETuple(List(EAtom("char"), _, ELong(_))) =>
        NumberType
      case ETuple(List(EAtom("op"), ELong(line), EAtom(op), _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeUnOp(op)
        )
      case ETuple(List(EAtom("op"), ELong(line), EAtom(op), _, _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeBinOp(op)
        )
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom("tuple"), EAtom("any"))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypeAnyTuple
        )
      case ETuple(List(EAtom("type"), _, EAtom("tuple"), EList(types, None))) =>
        TupleType(types.map(convertType))
      case ETuple(List(EAtom("type"), _, EAtom("union"), EList(types, None))) =>
        UnionType(types.map(convertType))
      case ETuple(List(EAtom("var"), _, EAtom("_"))) =>
        AnyType
      case ETuple(List(EAtom("var"), _, EAtom(name))) =>
        VarType(name)
      case ETuple(
            List(EAtom("type"), _, EAtom("list"), EList(List(elemType), None))
          ) =>
        ListType(convertType(elemType))
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom(name), EList(List(), None))
          ) =>
        Types.builtinTypes.get(name) match {
          case Some(ty) => ty
          case None =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              line.intValue,
              WIPDiagnostics.TypePredefined(name)
            )
        }
      case ETuple(
            List(EAtom("type"), ELong(line), EAtom(name), EList(_, None))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          line.intValue,
          WIPDiagnostics.TypePredefined(name)
        )
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def convertFunSpecType(term: EObject): ConstrainedFunType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              _,
              EAtom("fun"),
              EList(
                List(
                  ETuple(
                    List(EAtom("type"), _, EAtom("product"), EList(args, None))
                  ),
                  resultType
                ),
                None
              )
            )
          ) =>
        ConstrainedFunType(
          FunType(args.map(convertType), convertType(resultType)),
          List.empty
        )
      case ETuple(
            List(
              EAtom("type"),
              _,
              EAtom("bounded_fun"),
              EList(List(ft, EList(constraints, None)), None)
            )
          ) =>
        ConstrainedFunType(
          convertType(ft).asInstanceOf[FunType],
          constraints.map(convertConstraint)
        )
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def convertConstraint(term: EObject): Constraint = {
    val ETuple(
      List(
        EAtom("type"),
        _,
        EAtom("constraint"),
        EList(List(isSub, EList(List(v, t), None)), None)
      )
    ) = term
    val "is_subtype" = atomLit(isSub)
    Constraint(varString(v), convertType(t))
  }

  private def convertClause(term: EObject): Clause = {
    val ETuple(
      List(
        EAtom("clause"),
        ELong(l),
        EList(ePats, None),
        EList(eGuards, None),
        EList(eExps, None)
      )
    ) = term
    Clause(
      ePats.map(convertPat),
      eGuards.map(convertGuard),
      eExps.map(convertExp)
    )(l.intValue)
  }

  private def convertExp(term: EObject): Expr =
    term match {
      case ETuple(List(EAtom("match"), ELong(l), ePat1, eExp)) =>
        Match(convertPat(ePat1), convertExp(eExp))(l.intValue)
      case ETuple(List(EAtom("var"), ELong(l), EAtom(name))) =>
        Var(name)(l.intValue)
      case ETuple(List(EAtom("tuple"), ELong(l), EList(eExps, None))) =>
        Tuple(eExps.map(convertExp))(l.intValue)
      case ETuple(List(EAtom("nil"), ELong(l))) =>
        NilLit()(l.intValue)
      case ETuple(List(EAtom("cons"), ELong(l), hE, tE)) =>
        Cons(convertExp(hE), convertExp(tE))(l.intValue)
      case ETuple(List(EAtom("bin"), ELong(l), EList(binElems, None))) =>
        Binary(binElems.map(convertBinaryElem))(l.intValue)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), exp1, exp2)) =>
        op match {
          case "++" =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.ExpListConcat
            )
          case "--" =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.ExpListSubtract
            )
          case _ => BinOp(op, convertExp(exp1), convertExp(exp2))(l.intValue)
        }
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), exp)) =>
        UnOp(op, convertExp(exp))(l.intValue)
      case ETuple(
            List(
              EAtom("record"),
              ELong(l),
              EAtom(recordName),
              EList(eRecordFieldExps, None)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpRecord(recordName)
        )
      case ETuple(
            List(
              EAtom("record"),
              ELong(l),
              eExp,
              EAtom(recordName),
              EList(eRecordFieldExps, None)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpRecord(recordName)
        )
      case ETuple(
            List(EAtom("record_index"), ELong(l), EAtom(recordName), eFieldName)
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpRecord(recordName)
        )
      case ETuple(
            List(
              EAtom("record_field"),
              ELong(l),
              eExp,
              EAtom(recordName),
              eFieldName
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpRecord(recordName)
        )
      case ETuple(List(EAtom("map"), ELong(l), EList(eAssocs, None))) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpMap
        )
      case ETuple(List(EAtom("map"), ELong(l), eExp, EList(eAssocs, None))) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpMap
        )
      case ETuple(List(EAtom("catch"), ELong(l), eExp)) =>
        Catch(convertExp(eExp))(l.intValue)
      case ETuple(List(EAtom("call"), ELong(l), eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), ELong(_), EAtom(m))),
                  ETuple(List(EAtom("atom"), ELong(_), EAtom(f)))
                )
              ) =>
            RemoteCall(RemoteId(m, f, eArgs.size), eArgs.map(convertExp))(
              l.intValue
            )
          case ETuple(List(EAtom("atom"), ELong(_), EAtom(fname))) =>
            LocalCall(Id(fname, eArgs.size), eArgs.map(convertExp))(l.intValue)
          case _ =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.ExpDCall
            )
        }
      case ETuple(List(EAtom("lc"), ELong(l), _, _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpLC
        )
      case ETuple(List(EAtom("bc"), ELong(l), _, _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpBC
        )
      case ETuple(List(EAtom("block"), ELong(l), EList(eExps, None))) =>
        Block(eExps.map(convertExp))(l.intValue)
      case ETuple(List(EAtom("if"), ELong(l), EList(eClauses, None))) =>
        If(eClauses.map(convertClause))(l.intValue)
      case ETuple(List(EAtom("case"), ELong(l), eExp, EList(eClauses, None))) =>
        Case(convertExp(eExp), eClauses.map(convertClause))(l.intValue)
      case ETuple(
            List(
              EAtom("try"),
              ELong(l),
              EList(eTryBody, None),
              EList(eTryClauses, None),
              EList(eCatchClauses, None),
              EList(eAfter, None)
            )
          ) =>
        val tryBody = eTryBody.map(convertExp)
        val tryClauses = eTryClauses.map(convertClause)
        val catchClauses = eCatchClauses.map(convertClause)
        val afterBody =
          if (eAfter.isEmpty) None else Some(eAfter.map(convertExp))
        if (tryClauses.isEmpty)
          TryCatchExpr(tryBody, catchClauses, afterBody)(l.intValue)
        else
          TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody)(
            l.intValue
          )
      case ETuple(List(EAtom("receive"), ELong(l), EList(eClauses, None))) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpReceive
        )
      case ETuple(
            List(
              EAtom("receive"),
              ELong(l),
              EList(eClauses, None),
              eTimeout,
              EList(defaults, None)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpReceive
        )
      case ETuple(List(EAtom("fun"), ELong(l), eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("clauses"), EList(eClauses, None))) =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.ExpAnonFun
            )
          case ETuple(List(EAtom("function"), EAtom(name), ELong(arity))) =>
            LocalFun(Id(name, arity.intValue))(l.intValue)
          case ETuple(
                List(
                  EAtom("function"),
                  ETuple(List(EAtom("atom"), _, EAtom(module))),
                  ETuple(List(EAtom("atom"), _, EAtom(name))),
                  ETuple(List(EAtom("integer"), _, ELong(arity)))
                )
              ) =>
            RemoteFun(RemoteId(module, name, arity.intValue))(l.intValue)
          case ETuple(List(EAtom("function"), eModule, eName, eArity)) =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.ExpDynFun
            )
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException()
          // $COVERAGE-ON$
        }
      case ETuple(List(EAtom("named_fun"), ELong(l), _, _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpNamedFun
        )
      case ETuple(List(EAtom("atom"), ELong(l), EAtom(value))) =>
        AtomLit(value)(l.intValue)
      case ETuple(List(EAtom("char" | "float" | "integer"), ELong(l), _)) =>
        NumberLit()(l.intValue)
      case ETuple(List(EAtom("string"), ELong(l), _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.ExpString
        )
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def convertBinaryElem(e: EObject): BinaryElem = {
    val ETuple(List(EAtom("bin_element"), ELong(l), elem, eSize, eSpecifier)) =
      e
    val size = eSize match {
      case EAtom("default") => None
      case _                => Some(convertExp(eSize))
    }
    val specifier = convertSpecifier(eSpecifier)
    BinaryElem(convertExp(elem), size, specifier)(l.intValue)
  }

  private def convertGuard(term: EObject): Guard = {
    val EList(tests, None) = term
    Guard(tests.map(convertTest))
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
        PatNil()(l.intValue)
      case ETuple(List(EAtom("cons"), ELong(l), hPat, tPat)) =>
        PatCons(convertPat(hPat), convertPat(tPat))(l.intValue)
      case ETuple(List(EAtom("atom"), ELong(l), EAtom(value))) =>
        PatAtom(value)(l.intValue)
      case ETuple(List(EAtom("char" | "float" | "integer"), ELong(l), _)) =>
        PatNumber()(l.intValue)
      case ETuple(List(EAtom("string"), ELong(l), _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.PatString
        )
      case ETuple(List(EAtom("bin"), ELong(l), EList(binElems, None))) =>
        PatBinary(binElems.map(convertPatBinaryElem))(l.intValue)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), p1, p2)) =>
        op match {
          case "++" =>
            throw WIPDiagnostics.SkippedConstructDiagnostics(
              l.intValue,
              WIPDiagnostics.PatListConcat
            )
          case _ => PatBinOp(op, convertPat(p1), convertPat(p2))(l.intValue)
        }
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), p)) =>
        PatUnOp(op, convertPat(p))(l.intValue)
      case ETuple(
            List(
              EAtom("record"),
              ELong(l),
              EAtom(name),
              EList(eRecordFieldPatterns, None)
            )
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.PatRecord(name)
        )
      case ETuple(
            List(EAtom("record_index"), ELong(l), EAtom(name), eFieldName)
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.PatRecord(name)
        )
      case ETuple(List(EAtom("map"), ELong(l), _)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.PatMap
        )
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def convertPatBinaryElem(e: EObject): PatBinaryElem = {
    val ETuple(List(EAtom("bin_element"), ELong(l), elem, eSize, eSpecifier)) =
      e
    val size = eSize match {
      case EAtom("default") => PatBinSizeConst
      case _ =>
        convertPat(eSize) match {
          case v: PatVar => PatBinSizeVar(v)
          case _         => PatBinSizeConst
        }
    }
    val specifier = convertSpecifier(eSpecifier)
    PatBinaryElem(convertPat(elem), size, specifier)(l.intValue)
  }

  private def convertTest(term: EObject): Test =
    term match {
      case ETuple(List(EAtom("var"), ELong(l), EAtom(name))) =>
        TestVar(name)(l.intValue)
      case ETuple(List(EAtom("tuple"), ELong(l), EList(eTests, None))) =>
        TestTuple(eTests.map(convertTest))(l.intValue)
      case ETuple(List(EAtom("nil"), ELong(l))) =>
        TestNil()(l.intValue)
      case ETuple(List(EAtom("cons"), ELong(l), h, t)) =>
        TestCons(convertTest(h), convertTest(t))(l.intValue)
      case ETuple(List(EAtom("bin"), ELong(l), _)) =>
        TestBinaryLit()(l.intValue)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), t1, t2)) =>
        TestBinOp(op, convertTest(t1), convertTest(t2))(l.intValue)
      case ETuple(List(EAtom("op"), ELong(l), EAtom(op), t)) =>
        TestUnOp(op, convertTest(t))(l.intValue)
      case ETuple(
            List(EAtom("record"), ELong(l), EAtom(recordName), EList(_, None))
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestRecord(recordName)
        )
      case ETuple(
            List(EAtom("record_index"), ELong(l), EAtom(recordName), _)
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestRecord(recordName)
        )
      case ETuple(
            List(EAtom("record_field"), ELong(l), eTest, EAtom(recordName), _)
          ) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestRecord(recordName)
        )
      case ETuple(List(EAtom("map"), ELong(l), EList(_, None))) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestMap
        )
      case ETuple(List(EAtom("map"), ELong(l), _, EList(_, None))) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestMap
        )
      case ETuple(List(EAtom("call"), ELong(l), eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), ELong(_), EAtom("erlang"))),
                  ETuple(List(EAtom("atom"), ELong(_), EAtom(fname)))
                )
              ) =>
            TestLocalCall(Id(fname, eArgs.size), eArgs.map(convertTest))(
              l.intValue
            )
          case ETuple(List(EAtom("atom"), ELong(_), EAtom(fname))) =>
            TestLocalCall(Id(fname, eArgs.size), eArgs.map(convertTest))(
              l.intValue
            )
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException()
          // $COVERAGE-ON$
        }
      case ETuple(List(EAtom("atom"), ELong(l), EAtom(value))) =>
        TestAtom(value)(l.intValue)
      case ETuple(
            List(EAtom("char" | "float" | "integer"), ELong(l), _value)
          ) =>
        TestNumber()(l.intValue)
      case ETuple(List(EAtom("string"), ELong(l), _value)) =>
        throw WIPDiagnostics.SkippedConstructDiagnostics(
          l.intValue,
          WIPDiagnostics.TestString
        )
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def convertSpecifier(eSpecifier: EObject): Specifier = {
    val unsignedSpec = eSpecifier match {
      case EList(specs, None) =>
        specs
          .collect { case EAtom(s) => s }
          .flatMap(specifiers.get)
          .headOption
          .getOrElse(UnsignedIntegerSpecifier)
      case _ =>
        UnsignedIntegerSpecifier
    }
    val signed = eSpecifier match {
      case EList(specs, None) => specs.contains(EAtom("signed"))
      case _                  => false
    }

    val spec =
      if (signed && unsignedSpec == UnsignedIntegerSpecifier)
        SignedIntegerSpecifier
      else unsignedSpec
    spec
  }
}
