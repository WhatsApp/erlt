package com.whatsapp.analyzer

import com.whatsapp.analyzer.BeamDb.App
import com.whatsapp.analyzer.util.Children
import erlang.forms.AbstractExpr._
import erlang.forms.AbstractForm._
import erlang.forms.AbstractType._

import scala.util.Random

/**
  * Analyze calls of polymorphic functions with functions as arguments
  *
  * Simplifying assumptions/caveats:
  * - "polymorphic function" is a function with >= 1 type variable that is used
  * >= twice in the definition, where either there is no constraint (`when`) affecting the type var
  * or the var is constrained only to be `:: any()` or `:: term()`.
  * - We skip very dynamic code, such as calls M:F:A where one of `M`, `F`, or `A` is not a literal
  */
object PolyFunctions {

  case class Mfa(module: String, name: String, arity: Int)

  /**
    * mfa(fun foo/arity)
    * where `foo/arity` refers to a declared function.
    *
    * `isSpeccedPolymorphic` is true iff `foo/arity` has a spec and is polymorphic
    * (see above for definition of "polymorphic" for our purposes)
    */
  case class Row(module: String, caller: Mfa, callee: Mfa, isSpeccedPolymorphic: Boolean)

  /**
    * mfa(something)
    * where mfa is a polymorphic function
    */
  case class FunArg(module: String, caller: Mfa)

  var otpPolyFuns = Set[Mfa]()
  var projectPolyFuns = Set[Mfa]()
  var rows = List[Row]()

  /**
    * mfa(funExpression)
    * where mfa is a polymorphic function
    */
  var expressionCalls = List[FunArg]()

  /**
    * mfa(F)  (literally, the variable is "F")
    * where mfa is polymorphic)
    */
  var fCalls = List[FunArg]()

  def main(args: Array[String]): Unit = {
    println("Loading beam DB")
    val otp = args sameElements Array("-otp")
    assert(BeamDb.dupMod2App.isEmpty, s"Dupmodes: ${BeamDb.dupMod2App}")
    val otpApps = BeamDb.otpApps.values.toList.sortBy(_.name)
    val projectApps = BeamDb.projectApps.values.toList.sortBy(_.name)
    otpPolyFuns = otpApps.flatMap(findPolymorphicFuns).toSet
    if (otp) {
      println("*** OTP")
      projectPolyFuns = Set.empty
      otpApps.foreach(analyzeApp(_moduleName => true))
    } else {
      println("*** PROJECT")
      projectPolyFuns = projectApps.flatMap(findPolymorphicFuns).toSet
      projectApps.foreach(
        analyzeApp(m =>
          CodeDirs.toProvenance(m) match {
            case CodeDirs.FirstParty(_)        => true
            case CodeDirs.Generated(_)         => false
            case CodeDirs.OtpProvenance        => false
            case CodeDirs.ThirdParty           => false
            case CodeDirs.UnknownProvenance(x) => println(s"warn ${x} unknown provenance"); false
          }
        )
      )
    }
    printStat()
  }

  private def findPolymorphicFuns(app: App): Set[Mfa] =
    app.modules.sorted.flatMap(findPolymoprhicFunsInModule).toSet

  private def findPolymoprhicFunsInModule(module: String): List[Mfa] = {
    val api = BeamDb.getModuleApi(module).get
    api.specs collect {
      case spec: AF_FunctionSpec if isPolymorphicSpec(spec) =>
        val (name, arity) = spec.id
        Mfa(module, name, arity)
    }
  }

  private def analyzeApp(moduleFilter: String => Boolean)(app: App): Unit = {
    println(s">>>> APP: ${app.name}")
    app.modules.sorted.filter(moduleFilter).foreach(analyzeModule(app, _))
  }

  private def isUnconstrained(v: String, constraints: Map[String, AbstractType]): Boolean =
    constraints.get(v) match {
      // still considered a type var if of the form T ... when T :: any()
      case Some(AF_PredefinedType("any" | "term", _)) => true
      case None                                       => true
      case _                                          => false
    }

  private def tyToVarsList(ty: AbstractType, constraints: Map[String, AbstractType]): List[String] =
    ty match {
      case AF_TypeVariable(v) if isUnconstrained(v, constraints) => v :: Nil
      case AF_AnnotatedType(_v, ty)                              => tyToVarsList(ty, constraints)
      case t                                                     => Children.typeChildren(t).flatMap(tyToVarsList(_, constraints))
    }

  private def constrainedTyToVarsList(cfty: AF_ConstrainedFunctionType): List[String] = {
    val AF_ConstrainedFunctionType(AF_FunctionType(argTys, resTy), constraints) = cfty
    val constraintsMap = constraints
      .collect({
        case Constraint(tVar, ty) => (tVar, ty)
      })
      .toMap
    // We effectively inline constraints.
    // We do not treat constraints like bounded quantification.
    // For analysis purposes, we *do* treat T in foo(T) -> when T :: any()
    // as a type variable even though eqwalizer treats such a T as any()
    val fromConstraints = constraints.flatMap {
      case Constraint(_v, ty) => tyToVarsList(ty, constraintsMap)
    }
    val tys = resTy :: argTys
    tys.flatMap(tyToVarsList(_, constraintsMap)) ++ fromConstraints
  }

  private def isPolymorphicSpec(spec: AF_FunctionSpec): Boolean = {
    val varsList = spec.types.flatMap(constrainedTyToVarsList)
    val varsUsedMultipleTimes = varsList.groupBy(identity).valuesIterator.filter { _.sizeCompare(2) >= 0 }
    varsUsedMultipleTimes.nonEmpty
  }

  private def isPolyFun(mfa: Mfa): Boolean = otpPolyFuns.contains(mfa) || projectPolyFuns.contains(mfa)

  private def analyzeModule(app: App, module: String): Unit = {
    def argAsMfa(arg: AbstractExpr): Option[Mfa] =
      arg match {
        case AF_LocalFun(funName, arity) =>
          Some(Mfa(module, funName, arity))
        case AF_RemoteFun(remoteModule, funName, arity) =>
          val mfa = Mfa(remoteModule, funName, arity)
          Some(mfa)
        // simplifying assumptions
        case _: AF_RemoteFunDynamic | _: AF_RemoteFunDynamic | _: AF_Fun | _: AF_NamedFun => None
        case _                                                                            => None
      }

    def handlePolyFunArgs(fn: Mfa, args: List[AbstractExpr]): Unit = {
      val mfaArgs = args.map(argAsMfa).filterNot(_.isEmpty).flatten
      if (
        args.exists {
          case _: AF_Fun => true
          case _         => false
        }
      ) {
        expressionCalls ::= FunArg(module, fn)
      }
      if (
        args.exists {
          case AF_Variable("F" | "Fun") => true
          case _                => false
        }
      ) {
        fCalls ::= FunArg(module, fn)
      }
      val polyArg = mfaArgs.find(isPolyFun)
      if (polyArg.isDefined) {
        rows ::= Row(module, fn, polyArg.get, isSpeccedPolymorphic = true)
      } else if (mfaArgs.nonEmpty) {
        rows ::= Row(module, fn, mfaArgs.head, isSpeccedPolymorphic = false)
      }
    }

    def dfs(expr: AbstractExpr): Unit = {
      expr match {
        case AF_LocalCall(AF_LiteralAtom(calleeName), args) =>
          val fn = Mfa(module, calleeName, args.size)
          if (isPolyFun(fn)) handlePolyFunArgs(fn, args)
        case AF_RemoteCall(AF_LiteralAtom(m), AF_LiteralAtom(f), args) =>
          val fn = Mfa(m, f, args.size)
          if (isPolyFun(fn)) handlePolyFunArgs(fn, args)
        case _ => ()
      }
      Children.children(expr).foreach(dfs)
    }

    val forms = BeamDb.loadModule(app, module, lite = false)
    forms foreach {
      case AF_FunctionDecl(_, _, clauses) =>
        clauses.flatMap(_.body).foreach(dfs)
      case _ => ()
    }
  }

  private def printStat(): Unit = {
    val SAMPLE_SIZE = 50

    def sample[T](l: List[T]): List[T] =
      Random.shuffle(l).take(SAMPLE_SIZE)

    def show(mfa: Mfa): String =
      mfa match {
        case Mfa(m, f, a) => s"$m:$f/$a"
      }

    def printFunArgs(items: List[PolyFunctions.FunArg]): Unit = {
      println(s"${"location".padTo(25, ' ')} -- ${"caller".padTo(35, ' ')} -- calls --> callee")
      for (FunArg(module, caller) <- sample(items)) {
        println(s"${module.padTo(25, ' ')} -- ${show(caller).padTo(35, ' ')}")
      }
    }

    def printRows(rows: List[Row]): Unit = {
      println(s"${"location".padTo(25, ' ')} -- ${"caller".padTo(35, ' ')} -- calls --> callee")
      for (Row(module, caller, callee, _) <- sample(rows)) {
        println(s"${module.padTo(25, ' ')} -- ${show(caller).padTo(35, ' ')} -- calls --> ${show(callee)}")
      }
    }

    val listExprCallCnt = expressionCalls.count(_.caller.module == "lists")
    val listFCallCnt = fCalls.count(_.caller.module == "lists")
    rows = rows.sortBy(_.caller.module)
    val (definitelys, maybes) = rows.partition(_.isSpeccedPolymorphic)
    val unspeccedCnt = maybes.size - definitelys.size
    val listDefinitelysCnt = definitelys.count(_.caller.module == "lists")
    val listUnspeccedCnt = maybes.count(_.caller.module == "lists") - listDefinitelysCnt
    println(s"OTP polyFuns (sample) \n${sample(otpPolyFuns.toList).map(show).mkString("\n")}")
    println()
    println(s"Project polyFuns (sample) \n${sample(projectPolyFuns.toList).map(show).mkString("\n")}")
    println("\n\n\n\n")
    println("Polymorphic functions with a fun expression as an argument (sample)")
    println("=======================")
    printFunArgs(expressionCalls)
    println("\n\n\n\n")
    println("Polymorphic functions with variable `Fun` or `F` as an argument (sample)")
    println("=======================")
    printFunArgs(fCalls)
    println("\n\n\n\n")
    println("Polymorphic functions with a declared function as an argument (sample)")
    println("=======================")
    printRows(maybes.take(SAMPLE_SIZE))
    println("\n\n\n\n")
    println("Polymorphic functions with a specced polymorphic function as an argument (sample)")
    println("=======================")
    printRows(definitelys.take(SAMPLE_SIZE))
    println("\n\n\n\n")
    println(s"""
        | Summary
        | =======================
        | polymorphic func called with a fun expression as an arg: ${expressionCalls.size}
        | polymorphic func **in lists module** called with a fun expression as an arg: ${listExprCallCnt}
        \n\n
        | polymorphic func called with variable `F` as an arg: ${fCalls.size}
        | polymorphic func **in lists module** called with variable `F` as an arg: ${listFCallCnt}
        \n\n
        | polymorphic func called with an unspecced declared func as an arg: ${unspeccedCnt}
        | polymorphic func **in lists module** called with an unspecced declared func as an arg: ${listUnspeccedCnt}
        \n\n
        | polymorphic func called with a specced polymorphic func as an arg: ${definitelys.size}
        | polymorphic func **in lists module** called with a specced polymorphic func as arg: $listDefinitelysCnt
        |""".stripMargin)

  }
}
