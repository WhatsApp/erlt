package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._

object Globalize {
  private def toRemoteId(module: String, id: Id): RemoteId = RemoteId(module, id.name, id.arity)

  private def globalize(id: RemoteId, t: Type): Type =
    t match {
      case FunType(args, resType) =>
        FunType(args.map(globalize(id, _)), globalize(id, resType))
      case LocalType(localId, params) =>
        RemoteType(toRemoteId(id.module, localId), params.map(globalize(id, _)))
      case RemoteType(remoteId, params) =>
        RemoteType(remoteId, params.map(globalize(remoteId, _)))
      case TupleType(params) =>
        TupleType(params.map(globalize(id, _)))
      case ListType(et) =>
        ListType(globalize(id, et))
      case UnionType(params) =>
        UnionType(params.map(globalize(id, _)))
      case DictMap(kt, vt) =>
        DictMap(globalize(id, kt), globalize(id, vt))
      case ShapeMap(props) =>
        ShapeMap(props.map(globalizeProp(id, _)))
      case RawVarType(name) => VarType(name, id)
      case _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType | AnyTupleType |
           AnyFunType =>
        t
      // $COVERAGE-OFF$
      case _: VarType => throw new IllegalStateException()
      // $COVERAGE-OFF$
    }

  def globalizeSpec(module: String, spec: FunSpec): FunSpec = {
    val id = toRemoteId(module, spec.id)
    globalizeSpecWithId(id, spec)
  }

  private def globalizeSpecWithId(id: RemoteId, spec: FunSpec): FunSpec = {
    val types = spec.types.map { case ConstrainedFunType(FunType(args, res), constraints) =>
      ConstrainedFunType(
        FunType(args.map(ty => globalize(id, ty)), globalize(id, res)),
        constraints.map { case Constraint(v, tp) => Constraint(v, globalize(id, tp))},
      )
    }
    spec.copy(types = types)(spec.line)
  }

  def globalizeTypeDecl(module: String, decl: TypeDecl): TypeDecl = {
    val id = toRemoteId(module, decl.id)
    decl.copy(body = globalize(id, decl.body))(decl.line)
  }

  def globalizeRecDecl(module: String, decl: RecDecl): RecDecl = {
    // this id will never be used, since record declarations can't take type parameters
    val id = RemoteId(module, decl.name, arity = -1)
    globalizeRecDeclWithId(id, decl)
  }

  private def globalizeRecDeclWithId(id: RemoteId, decl: RecDecl): RecDecl =
    decl.copy(fields = decl.fields.map(globalizeRecField(id, _)))(decl.line)

  private def globalizeRecField(remoteId: RemoteId, field: RecField): RecField =
    field.copy(tp = globalize(remoteId, field.tp))(field.line)

  private def globalizeProp(id: RemoteId, prop: Prop): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, globalize(id, tp))
      case OptProp(key, tp) =>
        OptProp(key, globalize(id, tp))
    }

}
