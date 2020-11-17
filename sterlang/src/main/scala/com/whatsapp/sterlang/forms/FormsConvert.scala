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

import com.whatsapp.sterlang.Doc
import com.whatsapp.sterlang.Etf._
import com.whatsapp.sterlang.forms.Forms._

object FormsConvert {
  def fromEtf(term: ETerm): List[Form] = {
    val EList(eforms) = term
    eforms.map(convertForm)
  }

  private def convertForm(term: ETerm): Form =
    term match {
      case ETuple(List(EAtom("attribute"), _anno, EAtom("module"), EAtom(name))) =>
        Module(name)
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export"), EList(ids))) =>
        Export(ids.map(convertIdWithArity))
      case ETuple(List(EAtom("attribute"), _anno, EAtom("import"), ETuple(List(EAtom(module), EList(ids))))) =>
        Import(module, ids.map(convertIdWithArity))
      case ETuple(List(EAtom("attribute"), _anno, EAtom("import_type"), ETuple(List(EAtom(module), EList(ids))))) =>
        ImportType(module, ids.map(convertIdWithArity))
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export_type"), EList(typesIds))) =>
        ExportType(typesIds.map(convertIdWithArity))
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom(attr @ ("type" | "opaque")),
              ETuple(List(EAtom(typeName), absType, EList(vars))),
            )
          ) =>
        val typeAttr: TypeAttr = attr match {
          case "type"   => Type
          case "opaque" => Opaque
        }
        val abstractType = TypesConvertErlt.convertType(absType)
        val params = vars.map(TypesConvertErlt.convertVar)
        TypeDecl(r(anno), typeAttr, typeName, params, abstractType)
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("struct"),
              ETuple(List(EAtom(name), ETuple(List(_, _, _, _, EList(fields))), EList(vars))),
            )
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvertErlt.convertVar), fields.map(fieldDecl), StrStruct)
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("exception"),
              ETuple(List(EAtom(name), ETuple(List(_, _, _, _, EList(fields))), EList(vars))),
            )
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvertErlt.convertVar), fields.map(fieldDecl), ExnStruct)
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("message"),
              ETuple(List(EAtom(name), ETuple(List(_, _, _, _, EList(fields))), EList(vars))),
            )
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvertErlt.convertVar), fields.map(fieldDecl), MsgStruct)
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("enum"),
              ETuple(List(EAtom(name), ETuple(List(_, _, _, _, EList(enumVariants))), EList(vars))),
            )
          ) =>
        EnumDecl(r(anno), name, vars.map(TypesConvertErlt.convertVar), enumVariants.map(enumVariantDecl))
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("spec"),
              ETuple(List(eFunId, EList(eTypeList))),
            )
          ) =>
        val funId = convertSpecFunId(eFunId)
        val typeList = eTypeList.map(TypesConvertErlt.convertFunSpecType)
        Spec(r(anno), funId, typeList)
      case ETuple(List(EAtom("function"), anno, EAtom(name), ELong(arity), EList(clauseSeq))) =>
        val clauses = clauseSeq.map(ExprsConvert.convertClause)
        Function(r(anno), name, arity.intValue, clauses)
      case ETuple(EAtom("unchecked_function") :: _anno :: EAtom(name) :: ELong(arity) :: _) =>
        UncheckedFunction(name, arity.intValue)
      case ETuple(List(EAtom("eof"), _anno)) =>
        EOF
      case ETuple(List(EAtom("error"), ETuple(ETuple(List(ETuple(List(ELong(line), ELong(column))), _)) :: _))) =>
        Error(Doc.Pos(line.toInt, column.toInt))
      case ETuple(List(EAtom("error"), ETuple(ETuple(List(ELong(line), ELong(column))) :: _))) =>
        Error(Doc.Pos(line.toInt, column.toInt))
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("unchecked_opaque"),
              ETuple(List(EAtom(typeName), _, EList(vars))),
            )
          ) =>
        val params = vars.map(TypesConvertErlt.convertVar)
        UncheckedTypeDecl(r(anno), typeName, params)
      case ETuple(EAtom("attribute") :: anno :: EAtom(attr_name) :: _) =>
        WildAttribute(r(anno), attr_name)
    }

  private def convertIdWithArity(term: ETerm): IdWithArity =
    term match {
      case ETuple(List(EAtom(name), ELong(arity))) =>
        (name, arity.intValue)
    }

  private def fieldDecl(term: ETerm): FieldDecl =
    term match {
      case ETuple(List(EAtom("field_definition"), anno, EAtom("positional"), _, eType)) =>
        PosFieldDecl(r(anno), TypesConvertErlt.convertType(eType))
      case ETuple(List(EAtom("field_definition"), anno, fieldNameLit, dValue, eType)) =>
        val defaultValue = dValue match {
          case EAtom("undefined") =>
            None
          case expr =>
            Some(ExprsConvert.convertExp(expr))
        }
        LblFieldDecl(r(anno), convertAtomLit(fieldNameLit), defaultValue, TypesConvertErlt.convertType(eType))
    }

  private def enumVariantDecl(term: ETerm): EnumVariantDecl = {
    val ETuple(List(EAtom("variant"), anno, name, eFields)) = term
    (eFields: @unchecked) match {
      case EAtom("none") => EnumVariantDecl(r(anno), convertAtomLit(name), List.empty)
      case EList(fields) => EnumVariantDecl(r(anno), convertAtomLit(name), fields.map(fieldDecl))
    }
  }

  def convertAtomLit(term: ETerm): String =
    term match {
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        atomVal
    }

  private def convertSpecFunId(term: ETerm): IdWithArity = {
    val ETuple(List(EAtom(fName), ELong(value))) = term
    (fName, value.intValue)
  }
}
