package com.whatsapp.analyzer

import erlang.forms.AbstractForm._
import erlang.forms.AbstractExpr._
import BeamDb.App
import erlang.forms.AbstractExpr

import scala.util.Random

/**
  * Analyze declarations for non-exported functions.
  *
  * This analysis ignores third-party and generated modules.
  */
object PrivateFunctions {

  sealed trait Id {
    val name: String
    val arity: Int
  }
  case class Pub(name: String, arity: Int) extends Id
  case class Priv(name: String, arity: Int) extends Id

  case class Row(
      sourcePath: String,
      module: String,
      privateFun: Priv,
      callSitesCnt: Int,
      calledByNPubFuns: Int,
      calledByNPrivFuns: Int,
      isSelfRecursive: Boolean,
      isMutuallyRecursive: Boolean,
      isUsed: Boolean,
      hasSpec: Boolean,
  )

  var rows = List[Row]()
  var totalPubFuns = 0

  def main(args: Array[String]): Unit = {
    println("Loading beam DB")
    val otp = args sameElements Array("-otp")
    assert(BeamDb.dupMod2App.isEmpty, s"Dupmodes: ${BeamDb.dupMod2App}")
    if (otp) {
      println("*** OTP")
      BeamDb.otpApps.values.toList.sortBy(_.name).foreach(analyzeApp)
    } else {
      println("*** PROJECT")
      BeamDb.projectApps.values.toList.sortBy(_.name).foreach(analyzeApp)
    }
    printStat()
  }

  private def analyzeApp(app: App): Unit = {
    println(s">>>> APP: ${app.name}")
    if (!CodeDirs.thirdParty.contains(app.name))
      app.modules.sorted.filterNot(CodeDirs.isGenerated(app.name, _)).foreach(analyzeModule(app, _))
  }

  def analyzeModule(app: App, module: String): Unit = {
    val api = BeamDb.getModuleApi(module).get
    val publicFunctionNameArities = api.exports.toSet

    def id(name: String, arity: Int): Id =
      if (publicFunctionNameArities.contains((name, arity))) Pub(name, arity) else Priv(name, arity)

    val speccedFuns = (api.specs collect {
      case AF_FunctionSpec(_, (name, arity), _) => id(name, arity)
    }).toSet

    val forms = BeamDb.loadModule(app, module, lite = false)

    var fun2callSitesCnt = Map[Priv, Int]().withDefaultValue(0)
    var fun2calledByFuns = Map[Priv, Set[Id]]().withDefaultValue(Set.empty)
    var fun2CalledPrivFuns = Map[Priv, Set[Priv]]().withDefaultValue(Set.empty)
    var usedFuns = Set[Priv]()
    var privFuns = List[Priv]()

    def walk(): Unit =
      forms foreach {
        case AF_FunctionDecl(callerName, callerArity, clauses) =>
          val caller = id(callerName, callerArity)
          caller match {
            case priv: Priv =>
              privFuns ::= priv
            case _: Pub => totalPubFuns += 1
          }

          def dfs(expr: AbstractExpr): Unit = {
            expr match {
              case AF_LocalCall(AF_LiteralAtom(calleeName), args) =>
                handleEdge(caller, id(calleeName, args.size))
              case _ => ()
            }
            children(expr) foreach (dfs(_))
          }

          clauses.flatMap(_.body).foreach(dfs)
        case _ => ()
      }

    def handleEdge(caller: Id, callee: Id): Unit = {
      callee match {
        case priv: Priv =>
          val cnt = (fun2callSitesCnt(priv) + 1)
          fun2callSitesCnt += (priv -> cnt)
          if (caller != callee) usedFuns += priv

          val funs = fun2calledByFuns(priv) + caller
          fun2calledByFuns += (priv -> funs)
        case _ => ()
      }
      (caller, callee) match {
        case (caller: Priv, callee: Priv) =>
          val funs = fun2CalledPrivFuns(caller) + callee
          fun2CalledPrivFuns += (caller -> funs)
        case _ => ()
      }
    }

    def addRows(): Unit =
      (privFuns foreach { (priv: Priv) =>
        val (privCallers, pubCallers) = fun2calledByFuns(priv) partition {
          case _: Priv => true
          case _: Pub  => false
        }

        val isMutuallyRecursive = {
          var result = false
          var visited = collection.mutable.Set[Priv]()

          def dfs(funs: Set[Priv]): Unit =
            funs.foreach { fun =>
              if (fun == priv) {
                result = true
                return
              }
              if (visited.contains(fun)) {
                return
              }
              visited += fun
              dfs(fun2CalledPrivFuns(fun))
            }

          dfs(fun2CalledPrivFuns(priv).filter(_ != priv))
          result
        }

        rows ::= Row(
          sourcePath = CodeDirs.sourcePath(app.name, module),
          module = module,
          privateFun = priv,
          callSitesCnt = fun2callSitesCnt(priv),
          calledByNPrivFuns = privCallers.size,
          calledByNPubFuns = pubCallers.size,
          isSelfRecursive = privCallers.contains(priv),
          isMutuallyRecursive = isMutuallyRecursive,
          hasSpec = speccedFuns.contains(priv),
          isUsed = usedFuns.contains(priv),
        )
      })

    walk()
    addRows()
  }

  private def children(expr: AbstractExpr): List[AbstractExpr] = {

    def gatherQualifiers(qualifiers: List[AF_Qualifier]): List[AbstractExpr] =
      qualifiers flatMap {
        case AF_Generate(_pat, expr)  => expr :: Nil
        case AF_BGenerate(_pat, expr) => expr :: Nil
        case AF_Filter(expr)          => expr :: Nil
      }

    def gatherClauses(clauses: List[AF_Clause]): List[AbstractExpr] = (clauses flatMap (_.body))

    def gatherEntries(entries: List[AF_Assoc]): List[AbstractExpr] =
      entries flatMap {
        case AF_FieldAssoc(k, v) => k :: v :: Nil
        case AF_FieldExact(k, v) => k :: v :: Nil
      }

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
  }

  private def printStat(): Unit = {
    val totalFuns = totalPubFuns + rows.size
    val uRows = rows.filter(_.isUsed)
    val usedPrivFunsCnt = uRows.size

    def safeDiv(numerator: Number, denominator: Float): Float =
      if (denominator == 0) 0f
      else
        numerator.floatValue / denominator

    def pct(numerator: Number, denominator: Float): Float =
      if (denominator == 0) 0f else (100 * safeDiv(numerator, denominator))

    val pctPub = pct(totalPubFuns, totalFuns)
    val pctPrivUsedAndUnused = pct(rows.size, totalFuns)
    val pctPriv = pct(usedPrivFunsCnt, totalFuns)

    val privFunSpeccedCnt = uRows.count(_.hasSpec)
    val privFunsUnspeccedCnt = usedPrivFunsCnt - privFunSpeccedCnt
    val pctPrivFunsSpecced = pct(privFunSpeccedCnt, usedPrivFunsCnt)
    val pctPrivFunsUnspecced = pct(privFunsUnspeccedCnt, usedPrivFunsCnt)
    val privSRecursiveCnt = uRows.count(_.isSelfRecursive)
    val pctPrivFunsSRecursive = pct(privSRecursiveCnt, usedPrivFunsCnt)
    val calledLeqOneTime = uRows.count(_.callSitesCnt <= 1)
    val calledOnlyByPrivCnt = uRows.count(r => r.calledByNPubFuns == 0 && r.calledByNPrivFuns > 0)
    val calledOnlyByPrivPct = pct(calledOnlyByPrivCnt, usedPrivFunsCnt)
    val calledOnlyByPubCnt = uRows.count(r => r.calledByNPrivFuns == 0 && r.calledByNPubFuns > 0)
    val calledOnlyByPubPct = pct(calledOnlyByPubCnt, usedPrivFunsCnt)
    val pctPrivFunsCalledOneTime = pct(calledLeqOneTime, usedPrivFunsCnt)
    val callSites = uRows.map(_.callSitesCnt).sum
    val callSitesAvg = safeDiv(callSites, usedPrivFunsCnt)
    val privFunSpeccedOrLeqOneUsageCnt = uRows.count(r => r.hasSpec || r.callSitesCnt <= 1)
    val pctPrivFunSpeccedOrLeqOneUsage = pct(privFunSpeccedOrLeqOneUsageCnt, usedPrivFunsCnt)

    val privSRecursiveFunsUnspeccedCnt = uRows.count(r => r.isSelfRecursive && !r.hasSpec)
    val pctSRecursivePrivFunsUnSpecced = pct(privSRecursiveFunsUnspeccedCnt, privSRecursiveCnt)

    val privFunSpeccedOrNonRecursiveCnt =
      uRows.count(r => r.hasSpec || (!r.isSelfRecursive && !r.isMutuallyRecursive))
    val pctPrivFunSpeccedOrNonRecursive = pct(privFunSpeccedOrNonRecursiveCnt, usedPrivFunsCnt)

    val withFanciestInferenceCnt = uRows.count(r => r.hasSpec || !r.isMutuallyRecursive)
    val pctWithFanciestInference = pct(withFanciestInferenceCnt, usedPrivFunsCnt)

    val privFunMRecursiveCnt = uRows.count(r => r.isMutuallyRecursive)
    val pctPrivFunMRecursive = pct(privFunMRecursiveCnt, usedPrivFunsCnt)

    val privFunMRecursiveUnSpeccedCnt = uRows.count(r => !r.hasSpec && r.isMutuallyRecursive)
    val pctPrivFunMRecursiveUnspecced = pct(privFunMRecursiveUnSpeccedCnt, privFunMRecursiveCnt)

    val privFunMRecursiveThriftyCnt =
      uRows.count(r => r.isMutuallyRecursive && r.privateFun.name.startsWith("thrift"))
    val pctPrivFunMRecursiveThrifty = pct(privFunMRecursiveThriftyCnt, privFunMRecursiveCnt)

    val formatter = java.text.NumberFormat.getIntegerInstance
    def fmt(n: Int): String = formatter.format(n)

    def p(msg: String, r: Row): Unit =
      println(s"file://${r.sourcePath} ${r.privateFun.name}/${r.privateFun.arity}  $msg")

    def sample(l: List[Row], msg: String, pred: Row => Boolean) = {
      val SAMPLE_SIZE = 30
      println(s"$msg SAMPLE\n")
      Random.shuffle(l.filter(pred)).take(SAMPLE_SIZE).foreach(p(msg, _))
      println(s"\n\n")
    }

    sample(rows, "unused", _.isUsed)
    sample(uRows, "self-recursive", _.isSelfRecursive)
    sample(uRows, "mutually recursive", _.isSelfRecursive)

    println(f"""
    | How many private function declarations?
    | ==========
    | Exported fun declarations:                                   $pctPub%.0f%% of all fun declarations (${fmt(
      totalPubFuns
    )})
    | Private fun declarations used and unused                     $pctPrivUsedAndUnused%.0f%% of all fun declarations (${fmt(
      rows.size
    )})
    | Used Private fun declarations                                $pctPriv%.0f%% of all fun declarations (${fmt(
      usedPrivFunsCnt
    )})

    | How are private function declarations used?
    | ========
    | Private fun declarations that are used exactly once:         $pctPrivFunsCalledOneTime%.0f%% of used private funs (${fmt(
      calledLeqOneTime
    )})
    | Avg usages for a private fun:                                $callSitesAvg%.2f
    | Private funs that are used ONLY by exported funs:            $calledOnlyByPubPct%.0f%% of used private funs (${fmt(
      calledOnlyByPubCnt
    )})
    | Private funs that are used ONLY by other priv funs:          $calledOnlyByPrivPct%.0f%% of used private funs (${fmt(
      calledOnlyByPrivCnt
    )})

    | When are used private function declarations unspecced?
    | =========
    | Private funs that are unspecced:                             $pctPrivFunsUnspecced%.0f%% of used private funs (${fmt(
      privFunsUnspeccedCnt
    )})
    | Private funs that are self-recursive:                        $pctPrivFunsSRecursive%.0f%% of used private funs (${fmt(
      privSRecursiveCnt
    )})
    | Recursive private funs that are unspecced:                   $pctSRecursivePrivFunsUnSpecced%.0f%% of used private self-recursive funs (${fmt(
      privSRecursiveFunsUnspeccedCnt
    )})
    | Private funs that are mutually-recursive:                    $pctPrivFunMRecursive%.0f%% of used private funs (${fmt(
      privFunMRecursiveCnt
    )})
    | Mutually-recursive private funs that are unspecced:          $pctPrivFunMRecursiveUnspecced%.0f%% of used private mutually-recursive funs (${fmt(
      privFunMRecursiveUnSpeccedCnt
    )})
    | Mutually-recursive priv funs starting with "thrift":         $pctPrivFunMRecursiveThrifty%.0f%% of used private mutually-recusive funs (${fmt(
      privFunMRecursiveThriftyCnt
    )})
    |
    | What %% of used private funs can we type-check IF
    | =========
    | We require specs for all private funs:                       $pctPrivFunsSpecced%.0f%% (${fmt(
      privFunSpeccedCnt
    )})
    | We require specs for all priv funs with > 1 usage:           $pctPrivFunSpeccedOrLeqOneUsage%.0f%% (${fmt(
      privFunSpeccedOrLeqOneUsageCnt
    )})
    | We require specs for m-recursive and s-recursive priv funs:  $pctPrivFunSpeccedOrNonRecursive%.0f%% (${fmt(
      privFunSpeccedOrNonRecursiveCnt
    )})
    | We require specs for mutually-recursive priv funs            $pctWithFanciestInference%.0f%% (${fmt(
      withFanciestInferenceCnt
    )})
    |""".stripMargin)

  }
}
