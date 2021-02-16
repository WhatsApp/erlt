package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Types.FoonType
import com.whatsapp.eqwalizer.tc.Util

class RecursiveFuns(module: String) {

  val inProgress = collection.mutable.Set[FoonType]()

  /** Rewrite the abstract forms (if possible) so that only branches
    * without recursive calls remain.
    *
    * For example, given this list of clauses:
    *
    * List(
    * sum(0, Y) -> Y,
    * sum(X, Y) -> sum(X - 1, Y + 1)
    * )
    *
    * toRecursionNBaseCases returns Some(List(sum(0, Y) -> Y))
    *
    * But for `loop() -> loop()` returns None
    *
    * Recursive functions are handled in the same way.
    *
    *  The following input:
    *
    *  ```erl
    *  -spec init() -> boolean().
    *  init() -> odd(10).
    *
    *  even(0) -> true;
    *  even(-1) -> even(-1);
    *  even(X) -> not odd(X - 1).
    *
    *  odd(1) -> true;
    *  odd(X) -> not even(X - 1).
    *  ```
    *
    * Gets rewrtten to something like:
    * ```erl
    * init() -> fun odd(1) -> true;
    *                  (X) -> not (fun even(0) -> true)(X - 1)
    * ```
    */
  def baseFoon(f: FoonType): Option[FoonType] = {
    if (inProgress.contains(f)) {
      return None
    }
    inProgress += f
    val res = baseClauses(f.clauses, FoonType(_, f.module, f.env))
    inProgress -= f
    res
  }

  /** Especially important piece of logic.
    *
    * `None` indicates to the caller that they should trim this branch, because we
    * couldn't find a way to make it safe (removing all the recursive clauses left us with no clauses)
    *
    * A Some(T) is an expression (or list of clauses or whatever) that has had all the dangerous
    * branches removed.
    */
  private def baseClauses[T](clauses: List[Clause], f: List[Clause] => T): Option[T] =
    clauses.map(baseClause).flatten match {
      case Nil     => None /* There are no clauses without recursive calls.*/
      case clauses => Some(f(clauses)) /* sublist of clauses–rewritten to skip recursive calls */
    }

  private def allOrNone[A](l: List[Option[A]]): Option[List[A]] =
    if (l.forall(_.isDefined)) Some(l map (_.get))
    else None

  private def mapBase(es: List[Expr]): Option[List[Expr]] = allOrNone(es map base)

  private def baseClause(c: Clause): Option[Clause] =
    mapBase(c.body) map (
      Clause(c.pats, c.guards, _)(c.l)
    )

  private def baseKvs[T](kvs: List[(Expr, Expr)]): Option[List[(Expr, Expr)]] =
    allOrNone(kvs.map { case (k0, v0) =>
      for (k <- base(k0); v <- base(v0)) yield (k, v)
    })

  private def base(e: Expr): Option[Expr] = {
    e match {
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
      case LocalCall(id, args0) =>
        Util.getFunType(module, id) match {
          case Some(f0: FoonType) =>
            for (
              f <- baseFoon(f0);
              args <- mapBase(args0)
            )
              yield FunCall(Fun(f.clauses)(e.l), args)(e.l)
          case _ => mapBase(args0) map (LocalCall(id, _)(e.l))
        }
      case RemoteCall(id, args0) =>
        Util.getFunType(id) match {
          case Some(f0: FoonType) =>
            for (
              f <- baseFoon(f0);
              args <- mapBase(args0)
            ) yield FunCall(Fun(f.clauses)(e.l), args)(e.l)
          case _ => mapBase(args0) map (RemoteCall(id, _)(e.l))
        }
      case FunCall(expr, args) =>
        for (
          expr1 <- base(expr);
          args1 <- mapBase(args)
        ) yield FunCall(expr1, args1)(e.l)
      case Fun(clauses)  => baseClauses(clauses, Fun(_)(e.l))
      case UnOp(op, arg) => base(arg) map (UnOp(op, _)(e.l))
      case BinOp(op, argA, argB) =>
        for (
          argA1 <- base(argA);
          argB1 <- base(argB)
        ) yield BinOp(op, argA1, argB1)(e.l)
      case LComprehension(template, qualifiers) => base(template) map (LComprehension(_, qualifiers)(e.l))
      case BComprehension(template, qualifiers) => base(template) map (BComprehension(_, qualifiers)(e.l))
      case Catch(expr)                          => base(expr) map (Catch(_)(e.l))
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
      case RecordCreate(recName, fields) =>
        allOrNone(fields.map { r =>
          base(r.value).map(RecordField(r.name, _))
        }).map(fields => RecordCreate(recName, fields)(e.l))
      case RecordUpdate(expr0, recName, fields0) =>
        for (
          expr <- base(expr0);
          fields <- allOrNone(fields0.map { r =>
            base(r.value).map(RecordField(r.name, _))
          })
        ) yield RecordUpdate(expr, recName, fields)(e.l)
      case RecordSelect(expr, recName, fieldName) =>
        base(expr) map (RecordSelect(_, recName, fieldName)(e.l))
      case MapCreate(kvs0) =>
        baseKvs(kvs0) map (MapCreate(_)(e.l))
      case ReqMapUpdate(map0, kvs0) =>
        val kvsOpt = allOrNone(kvs0.map { case (k, v0) =>
          base(v0) map (v => (k, v))
        })
        for (map <- base(map0); kvs <- kvsOpt) yield ReqMapUpdate(map, kvs)(e.l)
      case GenMapUpdate(map0, kvs0) =>
        for (map <- base(map0); kvs <- baseKvs(kvs0)) yield GenMapUpdate(map, kvs)(e.l)
      case _: Var | _: AtomLit | _: NumberLit | _: NilLit | _: LocalFun | _: RemoteFun | _: Binary | _: RecordIndex =>
        Some(e)
    }
  }

}
