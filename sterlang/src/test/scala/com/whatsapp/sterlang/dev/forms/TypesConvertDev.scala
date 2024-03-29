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

package com.whatsapp.sterlang.dev.forms

import com.whatsapp.sterlang.Etf._
import com.whatsapp.sterlang.forms.Types._
import com.whatsapp.sterlang.forms.{Types, r}

object TypesConvertDev {
  def convertType(term: ETerm): Type =
    term match {
      // af_annotated_type
      case ETuple(List(EAtom("ann_type"), anno, EList(List(af_anno, tp)))) =>
        AnnotatedType(r(anno), convertVar(af_anno), convertType(tp))
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
      case ETuple(List(EAtom("type"), anno, EAtom("shape"), EList(assocTypes))) =>
        Shape(r(anno), assocTypes.map(convertShapeField))
      case ETuple(List(EAtom("type"), anno, EAtom("open_shape"), EList(assocTypes), restType)) =>
        OpenShape(r(anno), assocTypes.map(convertShapeField), convertVar(restType))
      // af_remote_type
      case ETuple(List(EAtom("remote_type"), anno, EList(List(moduleLit, typeNameLit, EList(args))))) =>
        RemoteType(
          r(anno),
          FormsConvertDev.convertAtomLit(moduleLit),
          FormsConvertDev.convertAtomLit(typeNameLit),
          args.map(convertType),
        )
      case ETuple(List(EAtom("type"), anno, EAtom("tuple"), EList(types))) =>
        TupleType(r(anno), types.map(convertType))
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

  private def convertShapeField(term: ETerm): ShapeField =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("shape_field"),
              EList(List(ETuple(List(EAtom("atom"), _, EAtom(field))), vType)),
            )
          ) =>
        ShapeField(r(anno), field, convertType(vType))
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
