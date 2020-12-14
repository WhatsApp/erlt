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
import erlang.forms.AbstractForm._

object AbstractFormConvert {
  val reservedAttrNames = Set(
    "module",
    "behavior",
    "behaviour",
    "export",
    "import",
    "export_type",
    "compile",
    "type",
    "opaque",
    "record",
    "spec",
    "callback",
  )

  // if `lite=true` it returns nulls for functions
  // nulls (instead of Option) are used for performance reasons
  def convertForm(term: EObject, lite: Boolean): AbstractForm =
    term match {
      // af_module
      case ETuple(List(EAtom("attribute"), _anno, EAtom("module"), EAtom(name))) =>
        AF_Module(name)
      // af_behaviour
      case ETuple(List(EAtom("attribute"), _anno, EAtom("behavior" | "behaviour"), EAtom(name))) =>
        AF_Behaviour(name)
      // af_export
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export"), EList(ids, None))) =>
        AF_Export(ids.map(convertIdWithArity))
      // af_import
      case ETuple(List(EAtom("attribute"), _anno, EAtom("import"), ETuple(List(EAtom(module), EList(ids, None))))) =>
        AF_Import(module, ids.map(convertIdWithArity))
      // export_type
      case ETuple(List(EAtom("attribute"), _anno, EAtom("export_type"), EList(typesIds, None))) =>
        AF_ExportType(typesIds.map(convertIdWithArity))
      // af_compile - TODO - clarify options
      case ETuple(List(EAtom("attribute"), _anno, EAtom("compile"), options)) =>
        AF_Compile(options)
      // af_file
      case ETuple(List(EAtom("attribute"), _anno1, EAtom("file"), ETuple(List(EString(file), _anno2)))) =>
        AF_File(file)
      // af_type_decl
      case ETuple(
            List(
              EAtom("attribute"),
              _anno,
              EAtom(attr @ ("type" | "opaque")),
              ETuple(List(EAtom(typeName), absType, EList(vars, None))),
            )
          ) =>
        val typeAttr: TypeAttr = attr match {
          case "type"   => Type
          case "opaque" => Opaque
        }
        val abstractType = AbstractTypeConvert.convertType(absType)
        val params = vars.map(convertVar)
        AF_TypeDecl(typeAttr, typeName, abstractType, params)
      // af_record_decl
      case ETuple(
            List(EAtom("attribute"), _anno, EAtom("record"), ETuple(List(EAtom(recordName), EList(eRecFields, None))))
          ) =>
        AF_RecordDecl(recordName, eRecFields.map(convertFieldDecl))
      // af_function_spec
      case ETuple(
            List(
              EAtom("attribute"),
              _anno,
              EAtom(attr @ ("spec" | "callback")),
              ETuple(List(eFunId, EList(eTypeList, None))),
            )
          ) =>
        val specAttr: SpecAttr = attr match {
          case "spec"     => Spec
          case "callback" => Callback
        }
        val funId = convertSpecFunId(eFunId)
        val typeList = eTypeList.map(AbstractTypeConvert.convertFunSpecType)
        AF_FunctionSpec(specAttr, funId, typeList)
      // af_wild_attribute
      case ETuple(List(EAtom("attribute"), _anno, EAtom(attrName), attrValue)) =>
        assert(!reservedAttrNames(attrName), s"reserved attr: $attrName, term: $term")
        AF_WildAttribute(attrName, attrValue)
      // af_function_decl
      case ETuple(List(EAtom("function"), _anno, EAtom(name), ELong(arity), EList(clauseSeq, None))) =>
        if (lite) null
        else {
          val clauses = clauseSeq.map(AbstractExprConvert.convertClause)
          AF_FunctionDecl(name, arity.intValue, clauses)
        }
      case ETuple(List(EAtom("eof"), _anno)) =>
        AF_EOF
      case _ =>
        sys.error(s"unexpected term: $term")
    }

  def convertVar(term: EObject): String =
    term match {
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        name
    }

  def convertIdWithArity(term: EObject): IdWithArity =
    term match {
      case ETuple(List(EAtom(name), ELong(arity))) =>
        (name, arity.intValue)
    }

  def convertFieldDecl(term: EObject): AF_FieldDecl =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, fieldNameLit)) =>
        AF_FieldUntyped(convertAtomLit(fieldNameLit))
      case ETuple(List(EAtom("record_field"), _anno, fieldNameLit, _expr)) =>
        AF_FieldUntyped(convertAtomLit(fieldNameLit))
      case ETuple(List(EAtom("typed_record_field"), eUntypedField, eType)) =>
        val AF_FieldUntyped(name) = convertFieldDecl(eUntypedField)
        val tp = AbstractTypeConvert.convertType(eType)
        AF_FieldTyped(name, tp)
    }

  def convertAtomLit(term: EObject): String =
    term match {
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        atomVal
    }

  def convertSpecFunId(term: EObject): IdWithArity =
    term match {
      case ETuple(List(EAtom(fName), ELong(value))) =>
        (fName, value.intValue)
      case ETuple(List(EAtom(_mName), EAtom(fName), ELong(value))) =>
        (fName, value.intValue)
    }
}
