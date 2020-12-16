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

package erlang.forms

import erlang.Data._
import erlang.forms.AbstractExpr._

object AbstractExprConvert {
  def convertClause(term: EObject): AF_Clause =
    term match {
      case ETuple(List(EAtom("clause"), _anno, EList(ePats, None), EList(eGuards, None), EList(eExps, None))) =>
        val pats = ePats.map(AbstractPatternConvert.convertPat)
        val guards = eGuards.map(AbstractGuardConvert.convertGuard)
        val exps = eExps.map(convertExp)
        AF_Clause(pats, guards, exps)
    }

  def convertExp(term: EObject): AbstractExpr =
    term match {
      case ETuple(List(EAtom("match"), _anno, ePat1, eExp)) =>
        AF_Match(AbstractPatternConvert.convertPat(ePat1), convertExp(eExp))
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        AF_Variable(name)
      case ETuple(List(EAtom("tuple"), _anno, EList(eExps, None))) =>
        AF_Tuple(eExps.map(convertExp))
      case ETuple(List(EAtom("nil"), _anno)) =>
        AF_Nil
      case ETuple(List(EAtom("cons"), _anno, eExp1, eExp2)) =>
        val hd = convertExp(eExp1)
        val tl = convertExp(eExp2)
        AF_Cons(hd, tl)
      case ETuple(List(EAtom("bin"), _anno, EList(eBinElements, None))) =>
        val binElements = eBinElements.map(convertBinElement)
        AF_Bin(binElements)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eExp1, eExp2)) =>
        AF_BinaryOp(op, convertExp(eExp1), convertExp(eExp2))
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eExp1)) =>
        AF_UnaryOp(op, convertExp(eExp1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldExps, None))) =>
        AF_RecordCreation(recordName, eRecordFieldExps.map(convertRecordField))
      case ETuple(List(EAtom("record"), _anno, eExp, EAtom(recordName), EList(eRecordFieldExps, None))) =>
        AF_RecordUpdate(convertExp(eExp), recordName, eRecordFieldExps.map(convertRecordField))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AF_LiteralAtom(fieldName)) = AbstractExprConvert.maybeLiteral(eFieldName)
        AF_RecordIndex(recordName, fieldName)
      case ETuple(List(EAtom("record_field"), _anno, eExp, EAtom(recordName), eFieldName)) =>
        val Some(AF_LiteralAtom(fieldName)) = AbstractExprConvert.maybeLiteral(eFieldName)
        AF_RecordFieldAccess(convertExp(eExp), recordName, fieldName)
      case ETuple(List(EAtom("map"), _anno, EList(eAssocs, None))) =>
        AF_MapCreation(eAssocs.map(convertAssoc))
      case ETuple(List(EAtom("map"), _anno, eExp, EList(eAssocs, None))) =>
        AF_MapUpdate(convertExp(eExp), eAssocs.map(convertAssoc))
      case ETuple(List(EAtom("catch"), _anno, eExp)) =>
        AF_Catch(convertExp(eExp))
      case ETuple(List(EAtom("call"), _anno, eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(List(EAtom("remote"), _anno, eExp1, eExp2)) =>
            AF_RemoteCall(convertExp(eExp1), convertExp(eExp2), eArgs.map(convertExp))
          case _ =>
            AF_LocalCall(convertExp(eExp), eArgs.map(convertExp))
        }
      case ETuple(List(EAtom("lc"), _anno, eTemplate, EList(eQualifiers, None))) =>
        AF_ListComprehension(convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("bc"), _anno, eTemplate, EList(eQualifiers, None))) =>
        AF_BinaryComprehension(convertExp(eTemplate), eQualifiers.map(convertQualifier))
      case ETuple(List(EAtom("block"), _anno, EList(eExps, None))) =>
        AF_Block(eExps.map(convertExp))
      case ETuple(List(EAtom("if"), _anno, EList(eClauses, None))) =>
        AF_If(eClauses.map(convertClause))
      case ETuple(List(EAtom("case"), _anno, eExp, EList(eClauses, None))) =>
        AF_Case(convertExp(eExp), eClauses.map(convertClause))
      case ETuple(
        List(
          EAtom("try"),
          _anno,
          EList(eExps1, None),
          EList(eClauses1, None),
          EList(eClauses2, None),
          EList(eExps2, None),
        )
      ) =>
        AF_Try(
          eExps1.map(convertExp),
          eClauses1.map(convertClause),
          eClauses2.map(convertClause),
          eExps2.map(convertExp),
        )

      case ETuple(List(EAtom("receive"), _anno, EList(eClauses, None))) =>
        AF_Receive(eClauses.map(convertClause))
      case ETuple(List(EAtom("receive"), _anno, EList(eClauses, None), eTimeout, EList(defaults, None))) =>
        AF_ReceiveWithTimeout(eClauses.map(convertClause), convertExp(eTimeout), defaults.map(convertExp))

      case ETuple(List(EAtom("fun"), _anno, eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("function"), EAtom(name), ELong(arity))) =>
            AF_LocalFun(name, arity.intValue)
          case ETuple(List(EAtom("function"), EAtom(module), EAtom(name), ELong(arity))) =>
            AF_RemoteFun(module, name, arity.intValue)
          // TODO
          case ETuple(List(EAtom("function"), eModule, eName, eArity)) =>
            //        val Some(AF_LiteralAtom(module)) = maybeLiteral(moduleLit)
            //        val Some(AF_LiteralAtom(name)) = maybeLiteral(nameLit)
            //        val Some(AF_LiteralInteger(arity)) = maybeLiteral(arityLit)
            AF_RemoteFunDynamic(eModule, eName, eArity)

          case ETuple(List(EAtom("clauses"), EList(eClauses, None))) =>
            AF_Fun(eClauses.map(convertClause))
        }

      case ETuple(List(EAtom("named_fun"), _anno, EAtom(fName), EList(eClauses, None))) =>
        AF_NamedFun(fName, eClauses.map(convertClause))
      case _ =>
        maybeLiteral(term) match {
          case Some(exp) => exp
          case None      => sys.error(s"cannot parse exp: $term")
        }

    }

  def maybeLiteral(term: EObject): Option[AF_Literal] =
    term match {
      case ETuple(List(EAtom("atom"), _anno, EAtom(value))) =>
        Some(AF_LiteralAtom(value))
      case ETuple(List(EAtom("char"), _anno, ELong(value))) =>
        Some(AF_LiteralCharacter(value.charValue))
      case ETuple(List(EAtom("float"), _anno, EDouble(value))) =>
        Some(AF_LiteralFloat(value))
      case ETuple(List(EAtom("integer"), _anno, ELong(value))) =>
        Some(AF_LiteralInteger(value.intValue))
      case ETuple(List(EAtom("string"), _anno, EString(value))) =>
        Some(AF_LiteralString(value))
      case ETuple(List(EAtom("string"), _anno, EList(List(), None))) =>
        Some(AF_LiteralString(""))
      case ETuple(List(EAtom("string"), _anno, EList(_, None))) =>
        Some(AF_LiteralStringList())
      case _ => None
    }

  def convertBinElement(term: EObject): AF_BinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, eExp, eSize, eTypeSpecifiers)) =>
        AF_BinElement(convertExp(eExp), eSize, AbstractExprConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertTypeSpecifiers(term: EObject): TypeSpecifiers =
    term match {
      case EAtom("default") =>
        DefaultTypeSpecifier
      case EList(specifiers, None) =>
        TypeSpecifierList(specifiers.map(convertTypeSpecifier))
    }

  def convertTypeSpecifier(term: EObject): TypeSpecifier =
    term match {
      case EAtom(spec) =>
        TypeSpecifierId(spec)
      case ETuple(List(EAtom("unit"), ELong(value))) =>
        TypeSpecifierUnit(value.intValue)
    }

  def convertRecordField(term: EObject): AF_RecordField =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val name = eName match {
          case ETuple(List(EAtom("atom"), _anno, EAtom(value))) =>
            value
          case ETuple(List(EAtom("var"), _anno, EAtom("_"))) =>
            // TODO
            "_"
        }
        AF_RecordField(name, convertExp(ePat))
    }

  def convertAssoc(term: EObject): AF_Assoc =
    term match {
      // map_field_assoc
      case ETuple(List(EAtom("map_field_assoc"), _anno, eExp1, eExp2)) =>
        AF_FieldAssoc(convertExp(eExp1), convertExp(eExp2))
      case ETuple(List(EAtom("map_field_exact"), _anno, eExp1, eExp2)) =>
        AF_FieldExact(convertExp(eExp1), convertExp(eExp2))
    }

  def convertQualifier(term: EObject): AF_Qualifier =
    term match {
      case ETuple(List(EAtom("generate"), _anno, ePat, eExp)) =>
        AF_Generate(AbstractPatternConvert.convertPat(ePat), convertExp(eExp))
      case ETuple(List(EAtom("b_generate"), _anno, ePat, eExp)) =>
        AF_BGenerate(AbstractPatternConvert.convertPat(ePat), convertExp(eExp))
      case _ =>
        AF_Filter(convertExp(term))
    }

}