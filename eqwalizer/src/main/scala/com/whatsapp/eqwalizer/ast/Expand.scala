package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._

object Expand {
  private def expand(t: Type, stack: Set[RemoteId]): Type =
    t match {
      case RemoteType(id, params) =>
        if (stack(id)) {
          throw WIPDiagnostics.RecursiveType(id)
        } else {
          val stub = DB.getGlobalizedModuleStub(id.module).getOrElse(throw WIPDiagnostics.UnknownId(id))
          val localId = Id(id.name, id.arity)
          val typeDecl = stub.types.getOrElse(localId, throw WIPDiagnostics.UnknownId(id))
          val stack1 = stack + id
          val sub = typeDecl.params.zip(params.map(expand(_, stack1))).toMap
          val body = expand(typeDecl.body, stack1)
          Subst.subst(sub, body)
        }
      case FunType(args, resType) =>
        FunType(args.map(expand(_, stack)), expand(resType, stack))
      case TupleType(params) =>
        TupleType(params.map(expand(_, stack)))
      case ListType(et) =>
        ListType(expand(et, stack))
      case UnionType(params) =>
        UnionType(params.map(expand(_, stack)))
      case DictMap(kt, vt) =>
        DictMap(expand(kt, stack), expand(vt, stack))
      case ShapeMap(props) =>
        ShapeMap(props.map(expandProp(_, stack)))
      case _: VarType | _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType | AnyTupleType |
          AnyFunType =>
        t
      // $COVERAGE-OFF$
      case LocalType(_, _) => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def expandProp(prop: Prop, stack: Set[RemoteId]): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, expand(tp, stack))
      case OptProp(key, tp) =>
        OptProp(key, expand(tp, stack))
    }

  private def expandConstraints(t: Type, s: Map[String, Type], stack: Set[String]): Type =
    t match {
      case RemoteType(id, params) =>
        RemoteType(id, params.map(expandConstraints(_, s, stack)))
      case FunType(args, resType) =>
        FunType(args.map(expandConstraints(_, s, stack)), expandConstraints(resType, s, stack))
      case TupleType(params) =>
        TupleType(params.map(expandConstraints(_, s, stack)))
      case ListType(et) =>
        ListType(expandConstraints(et, s, stack))
      case UnionType(params) =>
        UnionType(params.map(expandConstraints(_, s, stack)))
      case DictMap(kt, vt) =>
        DictMap(expandConstraints(kt, s, stack), expandConstraints(vt, s, stack))
      case ShapeMap(props) =>
        ShapeMap(props.map(expandProp(_, s, stack)))
      case VarType(v) =>
        if (stack(v))
          throw WIPDiagnostics.RecursiveConstraint(v)
        else
          s.get(v) match {
            case Some(tp) => expandConstraints(tp, s, stack + v)
            case None     => t
          }
      case _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType | AnyTupleType | AnyFunType => t
      // $COVERAGE-OFF$
      case LocalType(_, _) => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def expandProp(prop: Prop, s: Map[String, Type], stack: Set[String]): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, expandConstraints(tp, s, stack))
      case OptProp(key, tp) =>
        OptProp(key, expandConstraints(tp, s, stack))
    }

  def expandFunSpec(funSpec: FunSpec): Form = {
    val sFunTypes: List[ConstrainedFunType] = funSpec.types
    try {
      val cfts = sFunTypes.map { cft =>
        val FunType(args, res) =
          if (cft.constraints.isEmpty) cft.ty
          else {
            val FunType(args, res) = cft.ty
            val subst = cft.constraints.map(c => c.tVar -> c.ty).toMap
            FunType(args.map(expandConstraints(_, subst, Set.empty)), expandConstraints(res, subst, Set.empty))
          }
        ConstrainedFunType(FunType(args.map(expand(_, Set.empty)), expand(res, Set.empty)), List.empty)
      }
      FunSpec(funSpec.id, cfts)(funSpec.line)
    } catch {
      case e: WIPDiagnostics.ExpansionFailure => FailedExpandFunSpec(funSpec.id, e)(funSpec.line)
    }
  }

  def expandTypeDecl(decl: TypeDecl): Form = {
    try decl.copy(body = expand(decl.body, Set.empty))(decl.line)
    catch {
      case e: WIPDiagnostics.ExpansionFailure => FailedExpandTypeDecl(decl.id, e)(decl.line)
    }
  }

  def expandRecDecl(decl: RecDecl): Form =
    try decl.copy(fields = decl.fields.map(expandRecField))(decl.line)
    catch {
      case e: WIPDiagnostics.ExpansionFailure => FailedExpandRecDecl(decl.name, e)(decl.line)
    }

  private def expandRecField(field: RecField): RecField =
    field.copy(tp = expand(field.tp, Set.empty))(field.line)
}
