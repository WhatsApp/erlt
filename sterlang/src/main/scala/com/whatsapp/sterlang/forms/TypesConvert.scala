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
      case ETuple(List(EAtom("type"), anno, EAtom("shape"), EList(assocTypes))) =>
        Shape(r(anno), assocTypes.map(convertShapeField))
      case ETuple(List(EAtom("type"), anno, EAtom("open_shape"), EList(assocTypes), restType)) =>
        OpenShape(r(anno), assocTypes.map(convertShapeField), convertType(restType))
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

  def convertShapeField(term: ETerm): ShapeField =
    term match {
      case ETuple(List(EAtom("type"), anno, EAtom("shape_field"), EList(List(kType, vType)))) =>
        ShapeField(r(anno), convertType(kType), convertType(vType))
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
