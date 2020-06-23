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
import erlang.forms.AbstractType._

object AbstractTypeConvert {
  def convertType(term: EObject): AbstractType =
    term match {
      // af_annotated_type
      case ETuple(List(EAtom("ann_type"), _anno, EList(List(af_anno, tp), None))) =>
        AF_AnnotatedType(AbstractFormConvert.convertVar(af_anno), convertType(tp))
      // af_atom
      case ETuple(List(EAtom("atom"), _anno, EAtom(atomVal))) =>
        AF_AtomType(atomVal)
      // af_bitstring_type
      case ETuple(List(EAtom("type"), _anno, EAtom("binary"), EList(eTypes, None))) =>
        val types = eTypes.map(convertType)
        val intTypes: List[AF_SingletonIntegerType] = types.map {
          case intType: AF_SingletonIntegerType => intType
        }
        val result = AF_BitstringType(intTypes)
        result
      // af_empty_list_type
      case ETuple(List(EAtom("type"), _anno, EAtom("nil"), EList(List(), None))) =>
        AF_EmptyListType
      // af_fun_type
      case ETuple(List(EAtom("type"), _anno, EAtom("fun"), EList(List(), None))) =>
        AF_FunTypeAny
      // af_fun_type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("any"))), resultType), None),
            )
          ) =>
        AF_FunTypeAnyArgs(convertType(resultType))
      // af_function_type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        AF_FunctionType(args.map(convertType), convertType(resultType))
      // af_integer_range_type
      case ETuple(List(EAtom("type"), _anno, EAtom("range"), EList(List(eType1, eType2), None))) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[AF_SingletonIntegerType]
        val intType2 = type2.asInstanceOf[AF_SingletonIntegerType]
        AF_IntegerRangeType(intType1, intType2)
      // af_map_type
      case ETuple(List(EAtom("type"), _anno, EAtom("map"), EAtom("any"))) =>
        AF_AnyMap
      // af_map_type
      case ETuple(List(EAtom("type"), _anno, EAtom("map"), EList(assocTypes, None))) =>
        AF_AssocMap(assocTypes.map(convertAssocType))
      // af_record_type
      case ETuple(List(EAtom("type"), _anno, EAtom("record"), EList(recordNameLit :: eFieldTypes, None))) =>
        AF_RecordType(AbstractFormConvert.convertAtomLit(recordNameLit), eFieldTypes.map(convertRecordFieldType))
      // af_remote_type
      case ETuple(List(EAtom("remote_type"), _anno, EList(List(moduleLit, typeNameLit, EList(args, None)), None))) =>
        AF_RemoteType(
          AbstractFormConvert.convertAtomLit(moduleLit),
          AbstractFormConvert.convertAtomLit(typeNameLit),
          args.map(convertType),
        )
      // af_singleton_integer_type
      case ETuple(List(EAtom("integer"), _anno, ELong(value))) =>
        AF_Integer(value.intValue)
      case ETuple(List(EAtom("char"), _anno, ELong(value))) =>
        AF_Character(value.charValue)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eType1)) =>
        val type1 = convertType(eType1)
        val intType1 = type1.asInstanceOf[AF_SingletonIntegerType]
        AF_UnaryOp(op, intType1)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eType1, eType2)) =>
        val type1 = convertType(eType1)
        val type2 = convertType(eType2)
        val intType1 = type1.asInstanceOf[AF_SingletonIntegerType]
        val intType2 = type2.asInstanceOf[AF_SingletonIntegerType]
        AF_BinaryOp(op, intType1, intType2)
      // af_tuple_type
      case ETuple(List(EAtom("type"), _anno, EAtom("tuple"), EAtom("any"))) =>
        AF_TupleTypeAny
      case ETuple(List(EAtom("type"), _anno, EAtom("tuple"), EList(types, None))) =>
        AF_TupleTypeTyped(types.map(convertType))
      // af_type_union
      case ETuple(List(EAtom("type"), _anno, EAtom("union"), EList(types, None))) =>
        AF_TypeUnion(types.map(convertType))
      // af_type_variable
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        AF_TypeVariable(name)
      // af_user_defined_type
      case ETuple(List(EAtom("user_type"), _anno, EAtom(name), EList(params, None))) =>
        AF_UserDefinedType(name, params.map(convertType))
      //  af_predefined_type -- should be matched very last!!!
      case ETuple(List(EAtom("type"), _anno, EAtom(name), EList(types, None))) =>
        assert(predefinedTypes(name))
        AF_PredefinedType(name, types.map(convertType))
      case _ =>
        sys.error(s"unexpected term for type: $term")
    }

  def convertAssocType(term: EObject): AF_AssocType =
    term match {
      // map_field_assoc
      case ETuple(List(EAtom("type"), _anno, EAtom("map_field_assoc"), EList(types, None))) =>
        MapFieldAssoc(types.map(convertType))
      case ETuple(List(EAtom("type"), _anno, EAtom("map_field_exact"), EList(types, None))) =>
        MapFieldExact(types.map(convertType))
    }

  def convertRecordFieldType(term: EObject): AF_RecordFieldType =
    term match {
      case ETuple(List(EAtom("type"), _anno, EAtom("field_type"), EList(List(nameLit, eType), None))) =>
        AF_RecordFieldType(AbstractFormConvert.convertAtomLit(nameLit), convertType(eType))
    }

  def convertFunctionType(term: EObject): AF_FunctionType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        AF_FunctionType(args.map(convertType), convertType(resultType))

    }

  def convertFunSpecType(term: EObject): FunSpecType =
    term match {
      // function type
      case ETuple(
            List(
              EAtom("type"),
              _anno,
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _anno1, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        AF_FunctionType(args.map(convertType), convertType(resultType))
      case ETuple(
            List(EAtom("type"), _anno, EAtom("bounded_fun"), EList(List(eFunType, EList(constraints, None)), None))
          ) =>
        AF_ContrainedFunctionType(convertFunctionType(eFunType), constraints.map(convertConstraint))
    }

  def convertConstraint(term: EObject): Constraint =
    term match {
      case ETuple(
            List(EAtom("type"), _anno, EAtom("constraint"), EList(List(isSubtypeLit, EList(List(eVar, t), None)), None))
          ) =>
        val v = AbstractFormConvert.convertVar(eVar)
        val "is_subtype" = AbstractFormConvert.convertAtomLit(isSubtypeLit)
        val tp = convertType(t)
        Constraint(v, tp)
    }

}
