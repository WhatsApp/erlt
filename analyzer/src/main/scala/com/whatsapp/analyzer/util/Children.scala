package com.whatsapp.analyzer.util

import erlang.forms.AbstractExpr
import erlang.forms.AbstractExpr._
import erlang.forms.AbstractType._

object Children {
  def children(expr: AbstractExpr): List[AbstractExpr] =
    expr match {
      case AF_Match(_pat, arg)                         => arg :: Nil
      case AF_Tuple(elems)                             => elems
      case AF_Cons(hd, tl)                             => hd :: tl :: Nil
      case AbstractExpr.AF_BinaryOp(_op, exp1, exp2)   => exp1 :: exp2 :: Nil
      case AbstractExpr.AF_UnaryOp(_op, exp1)          => exp1 :: Nil
      case AF_RecordCreation(_recordName, fields)      => fields map (_.value)
      case AF_RecordUpdate(_exp1, _recordName, fields) => fields map (_.value)
      case AF_MapCreation(entries)                     => gatherEntries(entries)
      case AF_MapUpdate(exp, entries)                  => exp :: gatherEntries(entries)
      case AF_Catch(exp)                               => exp :: Nil
      case AF_LocalCall(fun, args)                     => fun :: args
      case AF_RemoteCall(module, fun, args)            => module :: fun :: args
      case AF_ListComprehension(template, qualifiers) =>
        template :: gatherQualifiers(qualifiers)
      case AF_BinaryComprehension(template, qualifiers) =>
        template :: gatherQualifiers(qualifiers)
      case AF_Block(exprs)        => exprs
      case AF_If(clauses)         => gatherClauses(clauses)
      case AF_Case(expr, clauses) => expr :: gatherClauses(clauses)
      case AF_Try(body, cl1, cl2, extra) =>
        body ++ gatherClauses(cl1 ++ cl2) ++ extra
      case AF_Receive(cl) => gatherClauses(cl)
      case AF_ReceiveWithTimeout(cl, _timeout, default) =>
        gatherClauses(cl) ++ default
      case AF_Fun(clauses)                => gatherClauses(clauses)
      case AF_NamedFun(_funName, clauses) => gatherClauses(clauses)
      case AF_Nil | _: AF_Literal | _: AF_Variable | _: AF_Bin | _: AF_RecordIndex | _: AF_RecordFieldAccess |
          _: AF_LocalFun | _: AF_RemoteFun | _: AF_RemoteFunDynamic =>
        Nil
    }

  def typeChildren(ty: AbstractType): List[AbstractType] =
    ty match {
      case AF_AnnotatedType(_anno, tp) => tp :: Nil
      case _: AF_AtomType | _: AF_BitstringType | AF_EmptyListType | AF_FunTypeAny | AF_AnyMap |
          _: AF_SingletonIntegerType | AF_TupleTypeAny | _: AF_TypeVariable =>
        Nil
      case AF_FunTypeAnyArgs(tp)          => tp :: Nil
      case AF_FunctionType(args, resType) => resType :: args
      case AF_IntegerRangeType(t1, t2)    => List(t1, t2)
      case AF_AssocMap(assocs) =>
        assocs.flatMap {
          case MapFieldExact(types) => types
          case MapFieldAssoc(types) => types
        }
      case AF_PredefinedType(_typeName, params)      => params
      case AF_RecordType(_recordName, fieldTypes)    => fieldTypes.map(x => x.tp)
      case AF_RemoteType(_module, _typeName, params) => params
      case AF_TupleTypeTyped(params)                 => params
      case AF_TypeUnion(params)                      => params
      case AF_UserDefinedType(_, types)              => types
    }

  def gatherQualifiers(qualifiers: List[AF_Qualifier]): List[AbstractExpr] =
    qualifiers flatMap {
      case AF_Generate(_pat, expr)  => expr :: Nil
      case AF_BGenerate(_pat, expr) => expr :: Nil
      case AF_Filter(expr)          => expr :: Nil
    }

  private def gatherClauses(clauses: List[AF_Clause]): List[AbstractExpr] = (clauses flatMap (_.body))

  private def gatherEntries(entries: List[AF_Assoc]): List[AbstractExpr] =
    entries flatMap {
      case AF_FieldAssoc(k, v) => k :: v :: Nil
      case AF_FieldExact(k, v) => k :: v :: Nil
    }
}
