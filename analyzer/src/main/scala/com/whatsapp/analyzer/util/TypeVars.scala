package com.whatsapp.analyzer.util

import erlang.forms.AbstractForm.AF_FunctionSpec
import erlang.forms.AbstractType._

object TypeVars {
  def vars(t: AbstractType): Set[String] =
    t match {
      case AF_AnnotatedType(anno, tp) =>
        vars(tp)
      case AF_AtomType(atom) =>
        Set.empty
      case AF_BitstringType(types) =>
        types.flatMap(vars).toSet
      case AF_EmptyListType =>
        Set.empty
      case AF_FunTypeAny =>
        Set.empty
      case AF_FunTypeAnyArgs(tp) =>
        vars(tp)
      case AF_FunctionType(args, resType) =>
        vars(resType) ++ args.flatMap(vars)
      case AF_IntegerRangeType(t1, t2) =>
        vars(t1) ++ vars(t2)
      case AF_AnyMap =>
        Set.empty
      case AF_AssocMap(assocs) =>
        assocs.flatMap(_.types.flatMap(vars)).toSet
      case AF_PredefinedType(typeName, params) =>
        params.flatMap(vars).toSet
      case AF_RecordType(recordName, fieldTypes) =>
        fieldTypes.flatMap(f => vars(f.tp)).toSet
      case AF_RemoteType(module, typeName, params) =>
        params.flatMap(vars).toSet
      case _: AF_SingletonIntegerType =>
        Set.empty
      case AF_TupleTypeAny =>
        Set.empty
      case AF_TupleTypeTyped(params) =>
        params.flatMap(vars).toSet
      case AF_TypeUnion(params) =>
        params.flatMap(vars).toSet
      case AF_TypeVariable(v) =>
        Set(v)
      case AF_UserDefinedType(typeName, params) =>
        params.flatMap(vars).toSet
    }

  def freeVars(spec: AF_FunctionSpec): Set[String] = {
    val funTypes = spec.types.map(_.functionType)
    val specVars: Set[String] =
      funTypes.flatMap(vars).toSet
    val constraintVars: Set[String] =
      spec.types.flatMap(_.constraints.map(_.tVar)).toSet
    specVars -- constraintVars
  }

}
