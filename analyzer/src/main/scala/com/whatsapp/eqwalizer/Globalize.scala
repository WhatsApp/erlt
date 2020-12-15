package com.whatsapp.eqwalizer

import erlang.forms.AbstractForm._
import erlang.forms.AbstractType._

object Globalize {
  // turns all "local" types into FQ remote types
  def globalize(module: String, t: AbstractType): AbstractType =
    t match {
      case AF_AnnotatedType(anno, tp) =>
        AF_AnnotatedType(anno, globalize(module, tp))
      case AF_FunTypeAnyArgs(tp) =>
        AF_FunTypeAnyArgs(globalize(module, tp))
      case AF_FunctionType(args, resType) =>
        AF_FunctionType(args.map(globalize(module, _)), globalize(module, resType))
      case AF_PredefinedType(name, params) =>
        AF_PredefinedType(name, params.map(globalize(module, _)))
      case AF_AssocMap(assocs) =>
        AF_AssocMap(assocs.map(globalizeAssoc(module, _)))
      case AF_RecordType(recordName, fieldTypes) =>
        AF_RecordType(recordName, fieldTypes.map(globalizeRecordField(module, _)))
      case AF_RemoteType(remModule, name, params) =>
        AF_RemoteType(remModule, name, params.map(globalize(module, _)))
      case AF_TupleTypeTyped(params) =>
        AF_TupleTypeTyped(params.map(globalize(module, _)))
      case AF_TypeUnion(params) =>
        AF_TypeUnion(params.map(globalize(module, _)))
      case AF_UserDefinedType(typeName, types) =>
        AF_RemoteType(module, typeName, types.map(globalize(module, _)))
      /// CANNOT HAVE USER_TYPES INSIDE
      case AF_TupleTypeAny | AF_AnyMap | AF_EmptyListType | AF_FunTypeAny | _: AF_IntegerRangeType | _: AF_AtomType |
          _: AF_TypeVariable | _: AF_BitstringType | _: AF_SingletonIntegerType =>
        t
    }

  private def globalizeAssoc(module: String, assoc: AF_AssocType): AF_AssocType =
    assoc match {
      case MapFieldExact(types) =>
        MapFieldExact(types.map(globalize(module, _)))
      case MapFieldAssoc(types) =>
        MapFieldAssoc(types.map(globalize(module, _)))
    }

  private def globalizeRecordField(module: String, field: AF_RecordFieldType): AF_RecordFieldType =
    field.copy(tp = globalize(module, field.tp))

  def globalizeSpec(module: String, spec: AF_FunctionSpec): AF_FunctionSpec = {
    val types = spec.types.map {
      case AF_ConstrainedFunctionType(AF_FunctionType(args, res), constraints) =>
        AF_ConstrainedFunctionType(
          AF_FunctionType(args.map(globalize(module, _)), globalize(module, res)),
          constraints.map { case Constraint(v, tp) => Constraint(v, globalize(module, tp)) },
        )
    }
    spec.copy(types = types)
  }

  def globalizeRecord(module: String, rec: AF_RecordDecl): AF_RecordDecl =
    rec.copy(fields = rec.fields.map(f => f.copy(tp = f.tp.map(globalize(module, _)))))

  def globalizeTypeDecl(module: String, decl: AF_TypeDecl): AF_TypeDecl =
    decl.copy(abstractType = globalize(module, decl.abstractType))
}
