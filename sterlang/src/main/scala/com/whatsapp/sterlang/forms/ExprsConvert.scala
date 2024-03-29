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

import com.whatsapp.sterlang.Etf._
import com.whatsapp.sterlang.forms.Exprs._

object ExprsConvert {
  def convertClause(term: ETerm): Clause =
    term match {
      case ETuple(List(EAtom("clause"), anno, EList(ePats), EList(eGuards), EList(eExps))) =>
        val pats = ePats.map(PatternsConvert.convertPat)
        val guards = eGuards.map(convertGuard)
        val exps = eExps.map(convertExp)
        Clause(r(anno), pats, guards, exps)
    }

  private def convertIfClause(term: ETerm): IfClause =
    term match {
      case ETuple(List(EAtom("clause"), _anno, EList(List()), EList(eGuards), EList(eExps))) =>
        val guards = eGuards.map(convertGuard)
        val exps = eExps.map(convertExp)
        IfClause(guards, exps)
    }

  def convertExp(term: ETerm): Expr =
    term match {
      case ETuple(List(EAtom("match"), anno, ePat1, eExp)) =>
        Match(r(anno), PatternsConvert.convertPat(ePat1), convertExp(eExp))
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        Variable(r(anno), name)
      case ETuple(List(EAtom("tuple"), anno, EList(eExps))) =>
        Tuple(r(anno), eExps.map(convertExp))
      case ETuple(List(EAtom("nil"), anno)) =>
        Nil(r(anno))
      case ETuple(List(EAtom("cons"), anno, eExp1, eExp2)) =>
        val hd = convertExp(eExp1)
        val tl = convertExp(eExp2)
        Cons(r(anno), hd, tl)
      case ETuple(List(EAtom("bin"), anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertBinElement)
        Bin(r(anno), binElements)
      case ETuple(List(EAtom("op"), anno, EAtom(op), eExp1, eExp2)) =>
        BinaryOp(r(anno), op, convertExp(eExp1), convertExp(eExp2))
      case ETuple(List(EAtom("op"), anno, EAtom(op), eExp1)) =>
        UnaryOp(r(anno), op, convertExp(eExp1))
      case ETuple(List(EAtom("struct"), anno, ETuple(List(EAtom("atom"), _, EAtom(name))), EList(fields))) =>
        LocalStructCreate(r(anno), name, fields.map(field))
      case ETuple(
            List(
              EAtom("struct"),
              anno,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(structName))),
                )
              ),
              EList(fields),
            )
          ) =>
        RemoteStructCreate(r(anno), module, structName, fields.map(field))
      case ETuple(List(EAtom("struct"), anno, eExp, ETuple(List(EAtom("atom"), _, EAtom(name))), EList(fields))) =>
        LocalStructUpdate(r(anno), convertExp(eExp), name, fields.map(field))
      case ETuple(
            List(
              EAtom("struct"),
              anno,
              eExp,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(structName))),
                )
              ),
              EList(fields),
            )
          ) =>
        RemoteStructUpdate(r(anno), convertExp(eExp), module, structName, fields.map(field))
      case ETuple(
            List(
              EAtom("struct_field"),
              anno,
              eExp,
              ETuple(List(EAtom("atom"), _, EAtom(structName))),
              eIndex,
            )
          ) =>
        LocalStructSelect(r(anno), convertExp(eExp), structName, convertIndex(eIndex))
      case ETuple(
            List(
              EAtom("struct_field"),
              anno,
              eExp,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(structName))),
                )
              ),
              eIndex,
            )
          ) =>
        RemoteStructSelect(r(anno), convertExp(eExp), module, structName, convertIndex(eIndex))
      case ETuple(List(EAtom("shape"), anno, EList(eAssocs))) =>
        ShapeCreate(r(anno), eAssocs.map(convertShapeField))
      case ETuple(List(EAtom("shape_update"), anno, eExp, EList(eAssocs))) =>
        ShapeUpdate(r(anno), convertExp(eExp), eAssocs.map(convertShapeField))
      case ETuple(List(EAtom("shape_field"), anno, eExp, eFieldName)) =>
        val AtomLiteral(p, fieldName) = ExprsConvert.literal(eFieldName)
        ShapeSelect(r(anno), convertExp(eExp), fieldName)
      case ETuple(List(EAtom("call"), anno, eExp, EList(eArgs))) =>
        eExp match {
          case ETuple(List(EAtom("remote"), _, eExp1, eExp2)) =>
            RemoteCall(r(anno), convertExp(eExp1), convertExp(eExp2), eArgs.map(convertExp))
          case _ =>
            LocalCall(r(anno), convertExp(eExp), eArgs.map(convertExp))
        }
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              eFields,
            )
          ) =>
        (eFields: @unchecked) match {
          case EAtom("none") =>
            LocalEnum(r(anno), enum, ctr, List.empty)
          case EList(fields) =>
            LocalEnum(r(anno), enum, ctr, fields.map(field))
        }
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
              eFields,
            )
          ) =>
        (eFields: @unchecked) match {
          case EAtom("none") =>
            RemoteEnum(r(anno), module, enum, ctr, List.empty)
          case EList(fields) =>
            RemoteEnum(r(anno), module, enum, ctr, fields.map(field))
        }
      case ETuple(List(EAtom("lc"), anno, eTemplate, EList(eQualifiers))) =>
        ListComprehension(r(anno), convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("bc"), anno, eTemplate, EList(eQualifiers))) =>
        BinaryComprehension(r(anno), convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("block"), anno, EList(eExps))) =>
        Block(r(anno), eExps.map(convertExp))
      case ETuple(List(EAtom("if"), anno, EList(eClauses))) =>
        If(r(anno), eClauses.map(convertIfClause))
      case ETuple(List(EAtom("case"), anno, eExp, EList(eClauses))) =>
        Case(r(anno), convertExp(eExp), eClauses.map(convertClause))
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
          r(anno),
          eExps1.map(convertExp),
          eClauses1.map(convertClause),
          eClauses2.map(convertClause),
          eExps2.map(convertExp),
        )

      case ETuple(List(EAtom("receive"), anno, EList(eClauses))) =>
        Receive(r(anno), eClauses.map(convertClause))
      case ETuple(List(EAtom("receive"), anno, EList(eClauses), eTimeout, EList(defaults))) =>
        ReceiveWithTimeout(r(anno), eClauses.map(convertClause), convertExp(eTimeout), defaults.map(convertExp))

      case ETuple(List(EAtom("fun"), anno, eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("function"), EAtom(name), ELong(arity))) =>
            LocalFun(r(anno), name, arity.intValue)
          case ETuple(List(EAtom("function"), eModule, eName, eArity)) =>
            RemoteFun(r(anno), convertExp(eModule), convertExp(eName), convertExp(eArity))
          case ETuple(List(EAtom("clauses"), EList(eClauses))) =>
            Fun(r(anno), eClauses.map(convertClause))
        }

      case ETuple(List(EAtom("named_fun"), anno, EAtom(fName), EList(eClauses))) =>
        NamedFun(r(anno), fName, eClauses.map(convertClause))
      case _ =>
        literal(term)
    }

  private def convertGuard(term: ETerm): Guard = {
    val EList(tests) = term
    Guard(tests.map(ExprsConvert.convertExp))
  }

  private def convertIndex(eIndex: ETerm): Index =
    (literal(eIndex): @unchecked) match {
      case AtomLiteral(_, lbl) => LblIndex(lbl)
      case IntLiteral(_, pos)  => PosIndex(pos)
    }

  def literal(term: ETerm): Literal =
    term match {
      case ETuple(List(EAtom("atom_expr"), anno, EAtom(value))) =>
        AtomLiteral(r(anno), value)
      case ETuple(List(EAtom("atom"), anno, EAtom(value))) =>
        AtomLiteral(r(anno), value)
      case ETuple(List(EAtom("char"), anno, ELong(value))) =>
        CharLiteral(r(anno), value.charValue)
      case ETuple(List(EAtom("integer"), anno, ELong(value))) =>
        IntLiteral(r(anno), value.intValue)
      case ETuple(List(EAtom("string"), anno, EString(value))) =>
        StringLiteral(r(anno), value)
      case ETuple(List(EAtom("string"), anno, EList(_))) =>
        // empty strings, unicode strings and "string 1" "string 2" are parsed this way,
        // we just ignore this for now
        StringLiteral(r(anno), "COMPLEX ERLANG STRING")
      case ETuple(List(EAtom("float"), anno, ELong(value))) =>
        FloatLiteral(r(anno), value)
    }

  private def convertBinElement(term: ETerm): BinElement =
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
      case EList(specifiers) =>
        TypeSpecifierList(specifiers.map(convertTypeSpecifier))
      case _ =>
        DefaultTypeSpecifier
    }

  private def convertTypeSpecifier(term: ETerm): TypeSpecifier = {
    val EAtom(spec) = term
    TypeSpecifier(spec)
  }

  private def field(term: ETerm): Field =
    term match {
      case ETuple(List(EAtom("field"), anno, EAtom("positional"), exp)) =>
        PosField(r(anno), convertExp(exp))
      case ETuple(List(EAtom("field"), anno, ETuple(List(EAtom("atom"), _, EAtom(name))), exp)) =>
        LblField(r(anno), name, convertExp(exp))
    }

  private def convertShapeField(term: ETerm): ShapeField =
    term match {
      case ETuple(List(EAtom("field"), anno, eExp1, eExp2)) =>
        ShapeField(r(anno), convertExp(eExp1), convertExp(eExp2))
    }

  private def convertQualifier(term: ETerm): Qualifier =
    term match {
      case ETuple(List(EAtom("generate"), _anno, ePat, eExp)) =>
        LGenerate(PatternsConvert.convertPat(ePat), convertExp(eExp))
      case ETuple(List(EAtom("b_generate"), _anno, ePat, eExp)) =>
        BGenerate(PatternsConvert.convertPat(ePat), convertExp(eExp))
      case _ =>
        Filter(convertExp(term))
    }

}
