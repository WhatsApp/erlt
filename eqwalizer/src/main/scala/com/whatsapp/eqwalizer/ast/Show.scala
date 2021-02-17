package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Types._

object Show {
  def show(tp: Type): String =
    tp match {
      case AtomLitType(atom) =>
        s"'$atom'"
      case FunType(argTys, resTy) =>
        s"""fun((${argTys.map(show).mkString(", ")}) -> ${show(resTy)})"""
      case TupleType(elems) =>
        elems.map(show).mkString("{", ", ", "}")
      case ListType(elemType) =>
        s"""[${show(elemType)}]"""
      case NilType =>
        "[]"
      case UnionType(elemTys) =>
        elemTys.map(show).mkString(" | ")
      case LocalType(Id(t, _), args) =>
        s"""$t(${args.map(show).mkString(", ")})"""
      case RemoteType(RemoteId(m, t, _), args) =>
        s"""$m:$t(${args.map(show).mkString(", ")})"""
      case VarType(name) =>
        name
      case DictMap(kt, vt) =>
        s"#D{${show(kt)} => ${show(vt)}}"
      case AnyType =>
        "term()"
      case AtomType =>
        "atom()"
      case NoneType =>
        "none()"
      case NumberType =>
        "number()"
      case PidType =>
        "pid()"
      case PortType =>
        "port()"
      case ReferenceType =>
        "reference()"
      case BinaryType =>
        "binary()"
      case RecordType(n) =>
        s"#$n{}"
      case ShapeMap(props) =>
        props.map(showProp).mkString("#S{", ", ", "}")
      case AnyTupleType =>
        "tuple()"
    }

  private def showProp(prop: Prop): String =
    prop match {
      case ReqProp(key, tp) =>
        s"$key := ${show(tp)}"
      case OptProp(key, tp) =>
        s"$key => ${show(tp)}"
    }

  def show(e: Expr): String = e match {
    case Var(n) =>
      n
    case Exprs.AtomLit(atom) =>
      s"'$atom'"
    case NumberLit() =>
      "number_expr"
    case Block(_) =>
      "block_expr"
    case LocalCall(Id(f, _), args) =>
      s"""$f(${args.map(show).mkString(", ")})"""
    case Exprs.RemoteCall(RemoteId(m, t, _), args) =>
      s"""$m:$t(${args.map(show).mkString(", ")})"""
    case LocalFun(id) =>
      id.toString
    case RemoteFun(id) =>
      id.toString
    case Tuple(elems) =>
      elems.map(show).mkString("{", ", ", "}")
    case Cons(h, t) =>
      s"[${show(h)} | ${show(t)}]"
    case NilLit() =>
      "[]"
    case Match(_, _) =>
      "match_expr"
    case Case(_, _) =>
      "case ..."
    case If(_) =>
      "if ..."
    case UnOp(op, _) =>
      s"$op _"
    case BinOp(op, _, _) =>
      s"_ $op _"
    case Binary(_) =>
      "<<..>>"
    case Catch(_) =>
      "catch"
    case TryCatchExpr(_, _, _) =>
      "try .. catch .."
    case TryOfCatchExpr(_, _, _, _) =>
      "try .. of .. catch .."
    case Receive(_) =>
      "receive .."
    case ReceiveWithTimeout(_, _, _) =>
      "receive .."
    case LComprehension(_, _) =>
      "[ || ]"
    case BComprehension(_, _) =>
      "<< || >>"
    case RecordCreate(recName, _) =>
      s"#$recName{...}"
    case RecordUpdate(_, recName, _) =>
      s"...#$recName{...}"
    case RecordSelect(_, recName, fieldName) =>
      s"...#$recName.$fieldName"
    case RecordIndex(recName, fieldName) =>
      s"#$recName.$fieldName"
    case MapCreate(_) =>
      "#{..}"
    case ReqMapUpdate(_, _) | GenMapUpdate(_, _) =>
      "..#{..}"
  }
}
