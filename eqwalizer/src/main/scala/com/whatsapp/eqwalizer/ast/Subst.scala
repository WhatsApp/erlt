package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

object Subst {
  def subst(s: Map[String, Type], t: Type): Type =
    t match {
      case FunType(args, resType) =>
        FunType(args.map(subst(s, _)), subst(s, resType))
      case LocalType(id, params) =>
        LocalType(id, params.map(subst(s, _)))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(subst(s, _)))
      case TupleType(params) =>
        TupleType(params.map(subst(s, _)))
      case ListType(elemT) =>
        ListType(subst(s, elemT))
      case UnionType(params) =>
        UnionType(params.map(subst(s, _)))
        // TODO: treat VarType with distinct id differently T85206030
      case VarType(v, _idTodo) =>
        s.getOrElse(v, t)
      case DictMap(kt, vt) =>
        DictMap(subst(s, kt), subst(s, vt))
      case ShapeMap(props) =>
        ShapeMap(props.map(substProp(s, _)))
      case _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType | AnyTupleType | AnyFunType =>
        t
      // $COVERAGE-OFF$
      case _: RawVarType => throw new IllegalStateException(s"Unexpected RawVarType $t. Globalize should convert to VarType")
      // $COVERAGE-ON$
    }

  private def substProp(s: Map[String, Type], prop: Prop): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, subst(s, tp))
      case OptProp(key, tp) =>
        OptProp(key, subst(s, tp))
    }
}
