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

package com.whatsapp.sterlang.forms

import com.whatsapp.sterlang.etf._
import com.whatsapp.sterlang.forms.Exprs._

object ExprsConvert {
  def convertClause(term: ETerm): Clause =
    term match {
      case ETuple(List(EAtom("clause"), anno, EList(ePats), EList(eGuards), EList(eExps))) =>
        val pats = ePats.map(PatternsConvert.convertPat)
        val guards = eGuards.map(GuardsConvert.convertGuard)
        val exps = eExps.map(convertExp)
        Clause(sp(anno), pats, guards, exps)
    }

  def convertIfClause(term: ETerm): IfClause =
    term match {
      case ETuple(List(EAtom("clause"), _anno, EList(List()), EList(eGuards), EList(eExps))) =>
        val guards = eGuards.map(GuardsConvert.convertGuard)
        val exps = eExps.map(convertExp)
        IfClause(guards, exps)
    }

  def convertExp(term: ETerm): Expr =
    term match {
      case ETuple(List(EAtom("match"), anno, ePat1, eExp)) =>
        Match(sp(anno), PatternsConvert.convertPat(ePat1), convertExp(eExp))
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        Variable(sp(anno), name)
      case ETuple(List(EAtom("tuple"), anno, EList(eExps))) =>
        Tuple(sp(anno), eExps.map(convertExp))
      case ETuple(List(EAtom("nil"), anno)) =>
        Nil(sp(anno))
      case ETuple(List(EAtom("cons"), anno, eExp1, eExp2)) =>
        val hd = convertExp(eExp1)
        val tl = convertExp(eExp2)
        Cons(sp(anno), hd, tl)
      case ETuple(List(EAtom("bin"), anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertBinElement)
        Bin(sp(anno), binElements)
      case ETuple(List(EAtom("op"), anno, EAtom(op), eExp1, eExp2)) =>
        BinaryOp(sp(anno), op, convertExp(eExp1), convertExp(eExp2))
      case ETuple(List(EAtom("op"), anno, EAtom(op), eExp1)) =>
        UnaryOp(sp(anno), op, convertExp(eExp1))
      case ETuple(List(EAtom("record"), anno, EAtom(recordName), EList(eRecordFieldExps))) =>
        RecordCreate(sp(anno), recordName, eRecordFieldExps.map(structField))
      case ETuple(List(EAtom("record"), anno, eExp, EAtom(recordName), EList(eRecordFieldExps))) =>
        RecordUpdate(sp(anno), convertExp(eExp), recordName, eRecordFieldExps.map(structField))
      case ETuple(List(EAtom("struct_field"), anno, eExp, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(p, fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        StructFieldAccess(sp(anno), convertExp(eExp), recordName, fieldName)
      case ETuple(List(EAtom("map"), anno, EList(eAssocs))) =>
        MapCreate(sp(anno), eAssocs.map(convertAssoc))
      case ETuple(List(EAtom("map"), anno, eExp, EList(eAssocs))) =>
        MapUpdate(sp(anno), convertExp(eExp), eAssocs.map(convertAssoc))
      case ETuple(List(EAtom("map_field"), anno, eExp, eFieldName)) =>
        val Some(AtomLiteral(p, fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        MapFieldAccess(sp(anno), convertExp(eExp), fieldName)
      case ETuple(List(EAtom("catch"), anno, eExp)) =>
        Catch(sp(anno), convertExp(eExp))
      case ETuple(List(EAtom("call"), anno, eExp, EList(eArgs))) =>
        eExp match {
          case ETuple(List(EAtom("remote"), _, eExp1, eExp2)) =>
            RemoteCall(sp(anno), convertExp(eExp1), convertExp(eExp2), eArgs.map(convertExp))
          case _ =>
            LocalCall(sp(anno), convertExp(eExp), eArgs.map(convertExp))
        }
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        LocalEnumCtr(sp(anno), enum, ctr, eArgs.map(convertExp))
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(enum))),
                )
              ),
              ETuple(List(EAtom("atom"), _anno3, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        RemoteEnumCtr(sp(anno), module, enum, ctr, eArgs.map(convertExp))
      case ETuple(List(EAtom("lc"), anno, eTemplate, EList(eQualifiers))) =>
        ListComprehension(sp(anno), convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("bc"), anno, eTemplate, EList(eQualifiers))) =>
        BinaryComprehension(sp(anno), convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("block"), anno, EList(eExps))) =>
        Block(sp(anno), eExps.map(convertExp))
      case ETuple(List(EAtom("if"), anno, EList(eClauses))) =>
        If(sp(anno), eClauses.map(convertIfClause))
      case ETuple(List(EAtom("case"), anno, eExp, EList(eClauses))) =>
        Case(sp(anno), convertExp(eExp), eClauses.map(convertClause))
      case ETuple(
            List(
              EAtom("try"),
              anno,
              EList(eExps1),
              EList(eClauses1),
              EList(eClauses2),
              EList(eExps2),
            )
          ) =>
        Try(
          sp(anno),
          eExps1.map(convertExp),
          eClauses1.map(convertClause),
          eClauses2.map(convertClause),
          eExps2.map(convertExp),
        )

      case ETuple(List(EAtom("receive"), anno, EList(eClauses))) =>
        Receive(sp(anno), eClauses.map(convertClause))
      case ETuple(List(EAtom("receive"), anno, EList(eClauses), eTimeout, EList(defaults))) =>
        ReceiveWithTimeout(sp(anno), eClauses.map(convertClause), convertExp(eTimeout), defaults.map(convertExp))

      case ETuple(List(EAtom("fun"), anno, eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("function"), EAtom(name), ELong(arity))) =>
            LocalFun(sp(anno), name, arity.intValue)
          case ETuple(List(EAtom("function"), eModule, eName, eArity)) =>
            RemoteFun(sp(anno), convertExp(eModule), convertExp(eName), convertExp(eArity))
          case ETuple(List(EAtom("clauses"), EList(eClauses))) =>
            Fun(sp(anno), eClauses.map(convertClause))
        }

      case ETuple(List(EAtom("named_fun"), anno, EAtom(fName), EList(eClauses))) =>
        NamedFun(sp(anno), fName, eClauses.map(convertClause))
      case _ =>
        maybeLiteral(term) match {
          case Some(exp) => exp
          case None      => sys.error(s"cannot parse exp: $term")
        }

    }

  def maybeLiteral(term: ETerm): Option[Literal] =
    term match {
      case ETuple(List(EAtom("atom"), anno, EAtom(value))) =>
        Some(AtomLiteral(sp(anno), value))
      case ETuple(List(EAtom("char"), anno, ELong(value))) =>
        Some(CharLiteral(sp(anno), value.charValue))
      case ETuple(List(EAtom("float"), anno, EDouble(value))) =>
        Some(FloatLiteral(sp(anno), value))
      case ETuple(List(EAtom("integer"), anno, ELong(value))) =>
        Some(IntLiteral(sp(anno), value.intValue))
      case ETuple(List(EAtom("string"), anno, EString(value))) =>
        Some(StringLiteral(sp(anno), Some(value)))
      case ETuple(List(EAtom("string"), anno, EList(List()))) =>
        Some(StringLiteral(sp(anno), Some("")))
      case ETuple(List(EAtom("string"), anno, EList(_))) =>
        Some(StringLiteral(sp(anno), None))
      case _ => None
    }

  def convertBinElement(term: ETerm): BinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, eExp, eSize, eTypeSpecifiers)) =>
        val size = eSize match {
          case EAtom("default") => None
          case other            => Some(convertExp(other))
        }
        BinElement(convertExp(eExp), size, ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertTypeSpecifiers(term: ETerm): TypeSpecifiers =
    term match {
      case EAtom("default") =>
        DefaultTypeSpecifier
      case EList(specifiers) =>
        TypeSpecifierList(specifiers.map(convertTypeSpecifier))
      case _ =>
        ???
    }

  def convertTypeSpecifier(term: ETerm): TypeSpecifier =
    term match {
      case EAtom(spec) =>
        TypeSpecifierId(spec)
      case ETuple(List(EAtom("unit"), ELong(value))) =>
        TypeSpecifierUnit(value.intValue)
    }

  def structField(term: ETerm): StructField =
    term match {
      case ETuple(List(EAtom("struct_field"), anno, eName, exp)) =>
        val name = eName match {
          case ETuple(List(EAtom("atom"), _anno, EAtom(value))) =>
            value
          case ETuple(List(EAtom("var"), _anno, EAtom("_"))) =>
            // TODO
            "_"
        }
        StructField(sp(anno), name, convertExp(exp))
    }

  def convertAssoc(term: ETerm): MapField =
    term match {
      case ETuple(List(EAtom("map_field"), anno, eExp1, eExp2)) =>
        MapField(sp(anno), convertExp(eExp1), convertExp(eExp2))
    }

  def convertQualifier(term: ETerm): Qualifier =
    term match {
      case ETuple(List(EAtom("generate"), _anno, ePat, eExp)) =>
        LGenerate(PatternsConvert.convertPat(ePat), convertExp(eExp))
      case ETuple(List(EAtom("b_generate"), _anno, ePat, eExp)) =>
        BGenerate(PatternsConvert.convertPat(ePat), convertExp(eExp))
      case _ =>
        Filter(convertExp(term))
    }

}
