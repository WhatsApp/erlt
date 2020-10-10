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
import com.whatsapp.sterlang.etf._
import com.whatsapp.sterlang.forms.Forms._

object FormsConvert {
  def fromEtf(term: ETerm): List[Form] = {
    val EList(eforms) = term
    eforms.map(convertForm)
  }

  def convertForm(term: ETerm): Form =
    term match {
      case ETuple(List(EAtom("attribute"), _anno, EAtom("ffi"))) =>
        FFI
      // af_module
      case ETuple(List(EAtom("attribute"), _anno, EAtom("module"), EAtom(name))) =>
        Module(name)
      // af_export
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export"), EList(ids))) =>
        Export(ids.map(convertIdWithArity))
      // af_import
      case ETuple(List(EAtom("attribute"), _anno, EAtom("import"), ETuple(List(EAtom(module), EList(ids))))) =>
        Import(module, ids.map(convertIdWithArity))
      case ETuple(List(EAtom("attribute"), _anno, EAtom("import_type"), ETuple(List(EAtom(module), EList(ids))))) =>
        ImportType(module, ids.map(convertIdWithArity))
      // export_type
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export_type"), EList(typesIds))) =>
        ExportType(typesIds.map(convertIdWithArity))
      // af_type_decl
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom(attr @ ("type" | "opaque" | "enum")),
              ETuple(List(EAtom(typeName), absType, EList(vars))),
            )
          ) =>
        val typeAttr: TypeAttr = attr match {
          case "type"   => Type
          case "opaque" => Opaque
          case "enum"   => Enum
        }
        val abstractType = TypesConvert.convertType(absType)
        val params = vars.map(TypesConvert.convertVar)
        TypeDecl(r(anno), typeAttr, typeName, params, abstractType)
      case ETuple(
            List(EAtom("attribute"), anno, EAtom("struct"), ETuple(List(EAtom(name), EList(vars), EList(fields))))
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvert.convertVar), fields.map(structFieldDecl), StrStruct)
      case ETuple(
            List(EAtom("attribute"), anno, EAtom("exception"), ETuple(List(EAtom(name), EList(vars), EList(fields))))
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvert.convertVar), fields.map(structFieldDecl), ExnStruct)
      case ETuple(
            List(EAtom("attribute"), anno, EAtom("message"), ETuple(List(EAtom(name), EList(vars), EList(fields))))
          ) =>
        StructDecl(r(anno), name, vars.map(TypesConvert.convertVar), fields.map(structFieldDecl), MsgStruct)
      // af_function_spec
      case ETuple(
            List(
              EAtom("attribute"),
              anno,
              EAtom("spec"),
              ETuple(List(eFunId, EList(eTypeList))),
            )
          ) =>
        val funId = convertSpecFunId(eFunId)
        val typeList = eTypeList.map(TypesConvert.convertFunSpecType)
        FunctionSpec(r(anno), funId, typeList)
      // af_function_decl
      case ETuple(List(EAtom("function"), anno, EAtom(name), ELong(arity), EList(clauseSeq))) =>
        val clauses = clauseSeq.map(ExprsConvert.convertClause)
        FunctionDecl(r(anno), name, arity.intValue, clauses)
      case ETuple(List(EAtom("eof"), _anno)) =>
        EOF
      case ETuple(List(EAtom("error"), ETuple(ETuple(List(ETuple(List(ELong(line), ELong(column))), _)) :: _))) =>
        Error(Doc.Pos(line.toInt, column.toInt))
      case ETuple(List(EAtom("error"), ETuple(ETuple(List(ELong(line), ELong(column))) :: _))) =>
        Error(Doc.Pos(line.toInt, column.toInt))
    }

  def convertIdWithArity(term: ETerm): IdWithArity =
    term match {
      case ETuple(List(EAtom(name), ELong(arity))) =>
        (name, arity.intValue)
    }

  def structFieldDecl(term: ETerm): StructFieldDecl =
    term match {
      case ETuple(List(EAtom("struct_field"), anno, fieldNameLit, eType)) =>
        StructFieldDecl(r(anno), convertAtomLit(fieldNameLit), None, TypesConvert.convertType(eType))
    }

  def convertAtomLit(term: ETerm): String =
    term match {
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        atomVal
    }

  def convertSpecFunId(term: ETerm): IdWithArity = {
    val ETuple(List(EAtom(fName), ELong(value))) = term
    (fName, value.intValue)
  }
}
