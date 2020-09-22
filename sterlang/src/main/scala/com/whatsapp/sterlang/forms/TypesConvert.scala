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
import com.whatsapp.sterlang.forms.Types._

object TypesConvert {
  def convertType(term: ETerm): Type =
    term match {
      // af_annotated_type
      case ETuple(List(EAtom("ann_type"), anno, EList(List(af_anno, tp)))) =>
        AnnotatedType(r(anno), convertVar(af_anno), convertType(tp))
      // af_atom
      case ETuple(List(EAtom("atom"), anno, EAtom(atomVal))) =>
        AtomType(r(anno), atomVal)
      // af_bitstring_type
      case ETuple(List(EAtom("type"), anno, EAtom("binary"), EList(List()))) =>
        BitstringType(r(anno))
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("enum"),
              ETuple(List(EAtom("atom"), _anno1, EAtom(ctrName))),
              EList(args),
            )
          ) =>
        EnumCtr(r(anno), ctrName, args.map(convertType))
      // af_function_type
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunType(r(anno), args.map(convertType), convertType(resultType))
      // af_map_type
      case ETuple(List(EAtom("type"), anno, EAtom("map"), EAtom("any"))) =>
        AnyMap(r(anno))
      // af_map_type
      case ETuple(List(EAtom("type"), anno, EAtom("map"), EList(assocTypes))) =>
        AssocMap(r(anno), assocTypes.map(convertAssocType))
      case ETuple(List(EAtom("type"), anno, EAtom("open_map"), EList(assocTypes), restType)) =>
        OpenAssocMap(r(anno), assocTypes.map(convertAssocType), convertType(restType))
      // af_record_type
      case ETuple(List(EAtom("type"), anno, EAtom("struct"), EList(List(structName)))) =>
        StructType(r(anno), FormsConvert.convertAtomLit(structName))
      // af_remote_type
      case ETuple(List(EAtom("remote_type"), anno, EList(List(moduleLit, typeNameLit, EList(args))))) =>
        RemoteType(
          r(anno),
          FormsConvert.convertAtomLit(moduleLit),
          FormsConvert.convertAtomLit(typeNameLit),
          args.map(convertType),
        )
      // af_tuple_type
      case ETuple(List(EAtom("type"), anno, EAtom("tuple"), EAtom("any"))) =>
        TupleTypeAny(r(anno))
      case ETuple(List(EAtom("type"), anno, EAtom("tuple"), EList(types))) =>
        TupleTypeTyped(r(anno), types.map(convertType))
      // af_type_union
      case ETuple(List(EAtom("type"), anno, EAtom("union"), EList(types))) =>
        UnionType(r(anno), types.map(convertType))
      // af_type_variable
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        TypeVariable(r(anno), name)
      // af_user_defined_type
      case ETuple(List(EAtom("user_type"), anno, EAtom(name), EList(params))) =>
        UserType(r(anno), name, params.map(convertType))
      //  af_predefined_type -- should be matched very last!!!
      case ETuple(List(EAtom("type"), anno, EAtom(name), EList(types))) =>
        assert(predefinedTypes(name), s"bad name: $name")
        PredefinedType(r(anno), name, types.map(convertType))
    }

  def convertVar(term: ETerm): Types.TypeVariable =
    term match {
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        Types.TypeVariable(r(anno), name)
    }

  def convertAssocType(term: ETerm): Assoc =
    term match {
      case ETuple(List(EAtom("type"), anno, EAtom("map_field"), EList(List(kType, vType)))) =>
        Assoc(r(anno), convertType(kType), convertType(vType))
    }

  def convertFunSpecType(term: ETerm): FunType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunType(r(anno), args.map(convertType), convertType(resultType))
    }

}
