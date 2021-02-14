package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._

object Recursion {

  def rec(clauses: List[Clause], funId: Id): Option[List[Clause]] = {

    def baseClause(c: Clause): Option[Clause] =
      mapBase(c.body) map (
        Clause(c.pats, c.guards, _)(c.l)
      )

    def baseClauses[T](clauses: List[Clause], f: List[Clause] => T): Option[T] =
      clauses.map(baseClause).flatten match {
        case Nil => None
        case clauses => Some(f(clauses))
      }

//    def baseField(field: RecordField): Option[RecordField] = base(field.value).map(RecordField(field.name, _))
//
//    def baseKvs(kvs: List[(Expr, Expr)]): List[(Expr, Expr)] = {
//      val handleKv = {
//         case (k, v) => for (k1 <- base(k); v1 <- base(v)) yield (k1, v1)
//      }
//      kvs.map(handleKv).flatten
//    }

    def allOrNone[A](l: List[Option[A]]): Option[List[A]] =
      if (l.forall(_.isDefined)) Some(l map (_.get))
      else None

    def mapBase(es: List[Expr]): Option[List[Expr]] = allOrNone(es map base)

    def base(e: Expr): Option[Expr] = e match {
      case Block(exprs0) =>
        mapBase(exprs0) map (Block(_)(e.l))
      case Match(pat, expr) =>
        base(expr) map (Match(pat, _)(e.l))
      case Tuple(elems) =>
        mapBase(elems) map (Tuple(_)(e.l))
      case Cons(h, t) =>
        for (
          h1 <- base(h);
          h2 <- base(t)
        ) yield Cons(h1, h2)(e.l)
      case Case(expr, clauses) =>
        base(expr).flatMap(expr1 => baseClauses(clauses, Case(expr1, _)(e.l)))
      case If(clauses) => baseClauses(clauses, If(_)(e.l))
      case LocalCall(id, args) =>
        if (id == funId) None
        else mapBase(args) map (LocalCall(id, _)(e.l))
      case RemoteCall(id, args) =>
        if (id == funId) None
        else mapBase(args) map (RemoteCall(id, _)(e.l))
      case FunCall(expr, args) =>
        for (
          expr1 <- base(expr);
          args1 <- mapBase(args)
        ) yield FunCall(expr1, args1)(e.l)
      case Fun(clauses) => baseClauses(clauses, Fun(_)(e.l))
      case UnOp(op, arg) => base(arg) map (UnOp(op, _)(e.l))
      case BinOp(op, argA, argB) =>
        for (
          argA1 <- base(argA);
          argB1 <- base(argB)
        ) yield BinOp(op, argA1, argB1)(e.l)
      case LComprehension(template, qualifiers) => base(template) map (LComprehension(_, qualifiers)(e.l))
      case BComprehension(template, qualifiers) => base(template) map (BComprehension(_, qualifiers)(e.l))
      case Catch(expr) => base(expr) map (Catch(_)(e.l))
      case TryCatchExpr(tryBody, catchClauses, after) =>
        val after1 = after flatMap mapBase
        for (
        tryBody1 <- mapBase(tryBody);
        res <- baseClauses(catchClauses, TryCatchExpr(tryBody1, _, after1)(e.l))
        ) yield res
      case TryOfCatchExpr(tryBody, tryClauses, catchClauses, after) =>
        val after1 = after flatMap mapBase
        for (
          tryBody1 <- mapBase(tryBody);
          tryClauses1 <- baseClauses(tryClauses, identity);
          res <- baseClauses(catchClauses, TryOfCatchExpr(tryBody1, tryClauses1, _, after1)(e.l))
        ) yield res
      case Receive(clauses) =>
        baseClauses(clauses, Receive(_)(e.l))
      case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
        for (
          timeout1 <- base(timeout);
          timeoutBlock1 <- mapBase(timeoutBlock);
          res <- baseClauses(clauses, ReceiveWithTimeout(_, timeout1, timeoutBlock1)(e.l))
        ) yield res
        // TODO!
//      case RecordCreate(recName, fields) =>
//        RecordCreate(recName, fields map baseField)(e.l)
//      case RecordUpdate(expr, recName, fields) =>
//        RecordUpdate(base(expr), recName, fields map baseField)(e.l)
//      case RecordSelect(expr, recName, fieldName) =>
//        RecordSelect(base(expr), recName, fieldName)
//      case MapCreate(kvs) => MapCreate(baseKvs(kvs))(e.l)
//      case ReqMapUpdate(map, kvs) => ReqMapUpdate(base(map), kvs map { case (k, v) => (k, base(v)) })(e.l)
//      case GenMapUpdate(map, kvs) => GenMapUpdate(base(map), baseKvs(kvs))(e.l)
      case _: Var | _: AtomLit | _: NumberLit | _: NilLit | _: LocalFun | _: RemoteFun |
           _: Binary | _: RecordIndex => Some(e)
    }

    baseClauses(clauses, identity)
  }

}
