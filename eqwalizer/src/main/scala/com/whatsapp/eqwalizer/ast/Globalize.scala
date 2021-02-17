package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._

object Globalize {
  def globalize(module: String, t: Type): Type =
    t match {
      case FunType(args, resType) =>
        FunType(args.map(globalize(module, _)), globalize(module, resType))
      case LocalType(Id(n, arity), params) =>
        RemoteType(RemoteId(module, n, arity), params.map(globalize(module, _)))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(globalize(module, _)))
      case TupleType(params) =>
        TupleType(params.map(globalize(module, _)))
      case ListType(et) =>
        ListType(globalize(module, et))
      case UnionType(params) =>
        UnionType(params.map(globalize(module, _)))
      case DictMap(kt, vt) =>
        DictMap(globalize(module, kt), globalize(module, vt))
      case ShapeMap(props) =>
        ShapeMap(props.map(globalizeProp(module, _)))
      case _: VarType | _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType | AnyTupleType =>
        t
    }

  def globalizeSpec(module: String, spec: FunSpec): FunSpec = {
    val types = spec.types.map { case ConstrainedFunType(FunType(args, res), constraints) =>
      ConstrainedFunType(
        FunType(args.map(globalize(module, _)), globalize(module, res)),
        constraints.map { case Constraint(v, tp) => Constraint(v, globalize(module, tp)) },
      )
    }
    spec.copy(types = types)(spec.line)
  }

  def globalizeTypeDecl(module: String, decl: TypeDecl): TypeDecl =
    decl.copy(body = globalize(module, decl.body))(decl.line)

  def globalizeRecDecl(module: String, decl: RecDecl): RecDecl =
    decl.copy(fields = decl.fields.map(globalizeRecField(module, _)))(decl.line)

  private def globalizeRecField(module: String, field: RecField): RecField =
    field.copy(tp = globalize(module, field.tp))(field.line)

  private def globalizeProp(module: String, prop: Prop): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, globalize(module, tp))
      case OptProp(key, tp) =>
        OptProp(key, globalize(module, tp))
    }
}
