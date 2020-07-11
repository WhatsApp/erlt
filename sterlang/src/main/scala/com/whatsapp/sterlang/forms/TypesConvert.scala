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
      case ETuple(List(EAtom("ann_type"), _anno, EList(List(af_anno, tp)))) =>
        AnnotatedType(FormsConvert.convertVar(af_anno), convertType(tp))
      // af_atom
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        AtomType(atomVal)
      // af_bitstring_type
      case ETuple(List(EAtom("type"), _anno, EAtom("binary"), EList(eTypes))) =>
        val types = eTypes.map(convertType)
        val intTypes: List[SingletonIntegerType] = types.map {
          case intType: SingletonIntegerType => intType
          case other                         => sys.error(s"cannot parse: $other")
        }
        val result = BitstringType(intTypes)
        result
      // af_empty_list_type
      case ETuple(List(EAtom("type"), _anno, EAtom("nil"), EList(List()))) =>
        EmptyListType
      // af_fun_type
      case ETuple(List(EAtom("type"), _anno, EAtom("fun"), EList(List()))) =>
        FunTypeAny
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("enum"),
              ETuple(List(EAtom("atom"), _anno1, EAtom(ctrName))),
              EList(args),
            )
          ) =>
        EnumCtr(ctrName, args.map(convertType))
      // af_fun_type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("any"))), resultType)),
            )
          ) =>
        FunTypeAnyArgs(convertType(resultType))
      // af_function_type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(args.map(convertType), convertType(resultType))
      // af_integer_range_type
      case ETuple(List(EAtom("type"), _anno, EAtom("range"), EList(List(eType1, eType2)))) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        val intType2 = type2.asInstanceOf[SingletonIntegerType]
        IntegerRangeType(intType1, intType2)
      // af_map_type
      case ETuple(List(EAtom("type"), _anno, EAtom("map"), EAtom("any"))) =>
        AnyMap
      // af_map_type
      case ETuple(List(EAtom("type"), _anno, EAtom("map"), EList(assocTypes))) =>
        AssocMap(assocTypes.map(convertAssocType))
      // af_record_type
      case ETuple(List(EAtom("type"), _anno, EAtom("record"), EList(recordNameLit :: eFieldTypes))) =>
        RecordType(FormsConvert.convertAtomLit(recordNameLit), eFieldTypes.map(convertRecordFieldType))
      // af_remote_type
      case ETuple(List(EAtom("remote_type"), _anno, EList(List(moduleLit, typeNameLit, EList(args))))) =>
        RemoteType(
          FormsConvert.convertAtomLit(moduleLit),
          FormsConvert.convertAtomLit(typeNameLit),
          args.map(convertType),
        )
      // af_singleton_integer_type
      case ETuple(List(EAtom("integer"), _anno, ELong(value))) =>
        SinlgeInteger(value.intValue)
      case ETuple(List(EAtom("char"), _anno, ELong(value))) =>
        SingleCharacter(value.charValue)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eType1)) =>
        val type1 = convertType(eType1)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        UnaryOpIntegerType(op, intType1)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eType1, eType2)) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[SingletonIntegerType]
        val intType2 = type2.asInstanceOf[SingletonIntegerType]
        BinaryOpIntegerType(op, intType1, intType2)
      // af_tuple_type
      case ETuple(List(EAtom("type"), _anno, EAtom("tuple"), EAtom("any"))) =>
        TupleTypeAny
      case ETuple(List(EAtom("type"), _anno, EAtom("tuple"), EList(types))) =>
        TupleTypeTyped(types.map(convertType))
      // af_type_union
      case ETuple(List(EAtom("type"), _anno, EAtom("union"), EList(types))) =>
        UnionType(types.map(convertType))
      // af_type_variable
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        TypeVariable(name)
      // af_user_defined_type
      case ETuple(List(EAtom("user_type"), _anno, EAtom(name), EList(params))) =>
        UserType(name, params.map(convertType))
      //  af_predefined_type -- should be matched very last!!!
      case ETuple(List(EAtom("type"), _anno, EAtom(name), EList(types))) =>
        assert(predefinedTypes(name), s"bad name: $name")
        PredefinedType(name, types.map(convertType))
      case _ =>
        sys.error(s"unexpected term for type: $term")
    }

  def convertAssocType(term: ETerm): AssocType =
    term match {
      // map_field_assoc
      case ETuple(List(EAtom("type"), _anno, EAtom("map_field_assoc"), EList(types))) =>
        MapFieldOpt(types.map(convertType))
      case ETuple(List(EAtom("type"), _anno, EAtom("map_field_exact"), EList(types))) =>
        MapFieldExact(types.map(convertType))
    }

  def convertRecordFieldType(term: ETerm): RecordFieldType =
    term match {
      case ETuple(List(EAtom("type"), _anno, EAtom("field_type"), EList(List(nameLit, eType)))) =>
        RecordFieldType(FormsConvert.convertAtomLit(nameLit), convertType(eType))
    }

  def convertFunctionType(term: ETerm): FunctionType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(args.map(convertType), convertType(resultType))

    }

  def convertFunSpecType(term: ETerm): FunSpecType =
    term match {
      // function type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args))), resultType)),
            )
          ) =>
        FunctionType(args.map(convertType), convertType(resultType))
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
        val v = FormsConvert.convertVar(eVar)
        val "is_subtype" = FormsConvert.convertAtomLit(isSubtypeLit)
        val tp = convertType(t)
        Constraint(v, tp)
    }

}