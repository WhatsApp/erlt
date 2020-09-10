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
        AnnotatedType(sp(anno), convertVar(af_anno), convertType(tp))
      // af_atom
      case ETuple(List(EAtom("atom"), anno, EAtom(atomVal))) =>
        AtomType(sp(anno), atomVal)
      // af_bitstring_type
      case ETuple(List(EAtom("type"), anno, EAtom("binary"), EList(eTypes))) =>
        val types = eTypes.map(convertType)
        val intTypes: List[SingletonIntegerType] = types.map {
          case intType: SingletonIntegerType => intType
          case other                         => sys.error(s"cannot parse: $other")
        }
        BitstringType(sp(anno), intTypes)
      // af_empty_list_type
      case ETuple(List(EAtom("type"), anno, EAtom("nil"), EList(List()))) =>
        EmptyListType(sp(anno))
      // af_fun_type
      case ETuple(List(EAtom("type"), anno, EAtom("fun"), EList(List()))) =>
        FunTypeAny(sp(anno))
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("enum"),
              ETuple(List(EAtom("atom"), _anno1, EAtom(ctrName))),
              EList(args),
            )
          ) =>
        EnumCtr(sp(anno), ctrName, args.map(convertType))
      // af_fun_type
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("any"))), resultType)),
            )
          ) =>
        FunTypeAnyArgs(sp(anno), convertType(resultType))
      // af_function_type
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(sp(anno), args.map(convertType), convertType(resultType))
      // af_integer_range_type
      case ETuple(List(EAtom("type"), anno, EAtom("range"), EList(List(eType1, eType2)))) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        val intType2 = type2.asInstanceOf[SingletonIntegerType]
        IntegerRangeType(sp(anno), intType1, intType2)
      // af_map_type
      case ETuple(List(EAtom("type"), anno, EAtom("map"), EAtom("any"))) =>
        AnyMap(sp(anno))
      // af_map_type
      case ETuple(List(EAtom("type"), anno, EAtom("map"), EList(assocTypes))) =>
        AssocMap(sp(anno), assocTypes.map(convertAssocType))
      case ETuple(List(EAtom("type"), anno, EAtom("open_map"), EList(assocTypes), restType)) =>
        OpenAssocMap(sp(anno), assocTypes.map(convertAssocType), convertType(restType))
      // af_record_type
      case ETuple(List(EAtom("type"), anno, EAtom("struct"), EList(structName :: fieldTypes))) =>
        StructType(sp(anno), FormsConvert.convertAtomLit(structName), fieldTypes.map(structFieldType))
      // af_remote_type
      case ETuple(List(EAtom("remote_type"), anno, EList(List(moduleLit, typeNameLit, EList(args))))) =>
        RemoteType(
          sp(anno),
          FormsConvert.convertAtomLit(moduleLit),
          FormsConvert.convertAtomLit(typeNameLit),
          args.map(convertType),
        )
      // af_singleton_integer_type
      case ETuple(List(EAtom("integer"), anno, ELong(value))) =>
        SinlgeInteger(sp(anno), value.intValue)
      case ETuple(List(EAtom("char"), anno, ELong(value))) =>
        SingleCharacter(sp(anno), value.charValue)
      case ETuple(List(EAtom("op"), anno, EAtom(op), eType1)) =>
        val type1 = convertType(eType1)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        UnaryOpIntegerType(sp(anno), op, intType1)
      case ETuple(List(EAtom("op"), anno, EAtom(op), eType1, eType2)) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        val intType2 = type2.asInstanceOf[SingletonIntegerType]
        BinaryOpIntegerType(sp(anno), op, intType1, intType2)
      // af_tuple_type
      case ETuple(List(EAtom("type"), anno, EAtom("tuple"), EAtom("any"))) =>
        TupleTypeAny(sp(anno))
      case ETuple(List(EAtom("type"), anno, EAtom("tuple"), EList(types))) =>
        TupleTypeTyped(sp(anno), types.map(convertType))
      // af_type_union
      case ETuple(List(EAtom("type"), anno, EAtom("union"), EList(types))) =>
        UnionType(sp(anno), types.map(convertType))
      // af_type_variable
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        TypeVariable(sp(anno), name)
      // af_user_defined_type
      case ETuple(List(EAtom("user_type"), anno, EAtom(name), EList(params))) =>
        UserType(sp(anno), name, params.map(convertType))
      //  af_predefined_type -- should be matched very last!!!
      case ETuple(List(EAtom("type"), anno, EAtom(name), EList(types))) =>
        assert(predefinedTypes(name), s"bad name: $name")
        PredefinedType(sp(anno), name, types.map(convertType))
      case _ =>
        sys.error(s"unexpected term for type: $term")
    }

  def convertVar(term: ETerm): Types.TypeVariable =
    term match {
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        Types.TypeVariable(sp(anno), name)
    }

  def convertAssocType(term: ETerm): Assoc =
    term match {
      case ETuple(List(EAtom("type"), anno, EAtom("map_field"), EList(List(kType, vType)))) =>
        Assoc(sp(anno), convertType(kType), convertType(vType))
    }

  def structFieldType(term: ETerm): StructFieldType =
    term match {
      case ETuple(List(EAtom("type"), _anno, EAtom("field_type"), EList(List(nameLit, eType)))) =>
        StructFieldType(FormsConvert.convertAtomLit(nameLit), convertType(eType))
    }

  def convertFunctionType(term: ETerm): FunctionType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(sp(anno), args.map(convertType), convertType(resultType))
    }

  def convertFunSpecType(term: ETerm): FunSpecType =
    term match {
      // function type
      case ETuple(
            List(
              EAtom("type"),
              anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(sp(anno), args.map(convertType), convertType(resultType))
      case ETuple(
            List(EAtom("type"), _anno, EAtom("bounded_fun"), EList(List(eFunType, EList(constraints))))
          ) =>
        AF_ContrainedFunctionType(convertFunctionType(eFunType), constraints.map(convertConstraint))
    }

  def convertConstraint(term: ETerm): Constraint =
    term match {
      case ETuple(
            List(EAtom("type"), _anno, EAtom("constraint"), EList(List(isSubtypeLit, EList(List(eVar, t)))))
          ) =>
        val v = convertVar(eVar)
        val "is_subtype" = FormsConvert.convertAtomLit(isSubtypeLit)
        val tp = convertType(t)
        Constraint(v, tp)
    }

}
