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

import com.whatsapp.sterlang.forms.Forms._

object FormsConvert {
  val reservedAttrNames = Set(
    "module",
    "behavior",
    "behaviour",
    "export",
    "import",
    "export_type",
    "import_type",
    "compile",
    "type",
    "opaque",
    "record",
    "spec",
    "callback",
  )

  def fromEtf(term: ETerm): List[Form] =
    term match {
      case EList(eforms) =>
        eforms.map(convertForm)
      case _ =>
        sys.error(s"Unexpected: $term")
    }

  def convertForm(term: ETerm): Form =
    term match {
      case ETuple(List(EAtom("attribute"), _anno, EAtom("lang"), EList(List(EAtom(mod1), EAtom(mod2))))) =>
        Lang(List(mod1, mod2))
      case ETuple(List(EAtom("attribute"), _anno, EAtom("lang"), EList(List(EAtom(mod1))))) =>
        Lang(List(mod1))
      // af_module
      case ETuple(List(EAtom("attribute"), _anno, EAtom("module"), EAtom(name))) =>
        Module(name)
      // af_behaviour
      case ETuple(List(EAtom("attribute"), _anno, EAtom("behavior" | "behaviour"), EAtom(name))) =>
        Behaviour(name)
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
      // af_compile - TODO - clarify options
      case ETuple(List(EAtom("attribute"), _anno, EAtom("compile"), options)) =>
        Compile(options)
      // af_file
      case ETuple(List(EAtom("attribute"), _anno1, EAtom("file"), ETuple(List(EString(file), _anno2)))) =>
        File(file)
      case ETuple(List(EAtom("attribute"), _anno1, EAtom("require" | "depends_on"), EList(modAtoms))) =>
        val modules = modAtoms.map {
          case EAtom(module) => module
          case modAtom       => sys.error(s"Unexpected: $modAtom")
        }
        Require(modules)
      // af_type_decl
      case ETuple(
            List(
              EAtom("attribute"),
              _anno,
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
        val params = vars.map(convertVar)
        TypeDecl(typeAttr, typeName, params, abstractType)
      // af_record_decl
      case ETuple(
            List(EAtom("attribute"), _anno, EAtom("record"), ETuple(List(EAtom(recordName), EList(eRecFields))))
          ) =>
        RecordDecl(recordName, eRecFields.map(convertFieldDecl))
      // af_function_spec
      case ETuple(
            List(
              EAtom("attribute"),
              _anno,
              EAtom(attr @ ("spec" | "callback")),
              ETuple(List(eFunId, EList(eTypeList))),
            )
          ) =>
        val specAttr: SpecAttr = attr match {
          case "spec"     => Spec
          case "callback" => Callback
        }
        val funId = convertSpecFunId(eFunId)
        val typeList = eTypeList.map(TypesConvert.convertFunSpecType)
        FunctionSpec(specAttr, funId, typeList)
      // af_function_decl
      case ETuple(List(EAtom("function"), _anno, EAtom(name), ELong(arity), EList(clauseSeq))) =>
        val clauses = clauseSeq.map(ExprsConvert.convertClause)
        FunctionDecl(name, arity.intValue, clauses)
      case ETuple(List(EAtom("eof"), _anno)) =>
        EOF
      case _ =>
        sys.error(s"unexpected term: $term")
    }

  def convertVar(term: ETerm): String =
    term match {
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        name
    }

  def convertIdWithArity(term: ETerm): IdWithArity =
    term match {
      case ETuple(List(EAtom(name), ELong(arity))) =>
        (name, arity.intValue)
    }

  def convertFieldDecl(term: ETerm): RecordFieldDecl =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, fieldNameLit)) =>
        RecordFieldUntyped(convertAtomLit(fieldNameLit))
      case ETuple(List(EAtom("record_field"), _anno, fieldNameLit, _expr)) =>
        RecordFieldUntyped(convertAtomLit(fieldNameLit))
      case ETuple(List(EAtom("typed_record_field"), eUntypedField, eType)) =>
        val RecordFieldUntyped(name) = convertFieldDecl(eUntypedField)
        val tp = TypesConvert.convertType(eType)
        RecordFieldTyped(name, tp)
    }

  def convertAtomLit(term: ETerm): String =
    term match {
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        atomVal
    }

  def convertSpecFunId(term: ETerm): IdWithArity =
    term match {
      case ETuple(List(EAtom(fName), ELong(value))) =>
        (fName, value.intValue)
      case ETuple(List(EAtom(_mName), EAtom(fName), ELong(value))) =>
        (fName, value.intValue)
    }
}
