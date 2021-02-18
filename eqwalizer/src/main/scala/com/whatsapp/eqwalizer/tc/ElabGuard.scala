package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.UndefinedField

final class ElabGuard(module: String) {
  private val elabPredicateType1: PartialFunction[String, Type] = {
    case "is_atom"      => AtomType
    case "is_binary"    => BinaryType
    case "is_bitstring" => BinaryType
    case "is_boolean"   => booleanType
    case "is_float"     => floatType
    case "is_function"  => AnyFunType
    case "is_integer"   => integerType
    case "is_list"      => ListType(AnyType)
    case "is_number"    => NumberType
    case "is_pid"       => PidType
    case "is_port"      => PortType
    case "is_reference" => ReferenceType
    case "is_map"       => DictMap(AnyType, AnyType)
    case "is_tuple"     => AnyTupleType
  }

  private val elabPredicateType21: PartialFunction[(String, Test), Type] = { case ("is_map_key", _) =>
    DictMap(AnyType, AnyType)
  }

  private val elabPredicateType22: PartialFunction[(String, Test), Type] = {
    case ("is_record", TestAtom(recName)) =>
      RecordType(recName)
    case ("is_function", _) =>
      AnyFunType
  }

  def elabGuards(guards: List[Guard], env: Env): Env =
    if (guards.isEmpty) env
    else Approx.joinEnvs(guards.map(elabGuard(_, env)))

  private def elabGuard(guard: Guard, env: Env): Env = {
    var envAcc = env
    guard.tests.foreach { test =>
      envAcc = elabTestT(test, trueType, envAcc)
    }
    envAcc
  }

  private def elabTest(test: Test, env: Env): Env =
    test match {
      case TestVar(_) | TestAtom(_) | TestNumber() | TestTuple(_) | TestNil() | TestCons(_, _) | TestMapCreate(_) =>
        env
      case TestLocalCall(_, _) =>
        env
      case unOp: TestUnOp =>
        elabUnOp(unOp, env)
      case binOp: TestBinOp =>
        elabBinOp(binOp, env)
      case TestBinaryLit() =>
        env
      case TestRecordIndex(_, _) =>
        env
      case TestRecordSelect(rec, recName, _) =>
        elabTestT(rec, RecordType(recName), env)
      case TestRecordCreate(recName, fields) =>
        val recDecl = Util.getRecord(module, recName).get
        val fieldDecls = recDecl.fields.map(f => f.name -> f).toMap
        val undefinedFields = fieldDecls.keySet -- fields.map(_.name)
        for (uField <- undefinedFields) {
          val fieldDecl = fieldDecls(uField)
          if (fieldDecl.defaultValue.isEmpty && !Subtype.subType(undefined, fieldDecl.tp)) {
            throw UndefinedField(test.l, recName, uField)
          }
        }
        var envAcc = env
        for (field <- fields) {
          val fieldDecl = fieldDecls(field.name)
          envAcc = elabTestT(field.value, fieldDecl.tp, envAcc)
        }
        envAcc
      case TestReqMapUpdate(map, _) =>
        elabTestT(map, DictMap(AnyType, AnyType), env)
      case TestGenMapUpdate(map, _) =>
        elabTestT(map, DictMap(AnyType, AnyType), env)
    }

  private def elabTestT(test: Test, t: Type, env: Env): Env =
    test match {
      case TestVar(v) =>
        val testType = env.get(v) match {
          case Some(vt) => Subtype.meet(vt, t)
          case None     => t
        }
        env + (v -> testType)
      case TestLocalCall(Id(pred, 1), List(arg)) if Subtype.eqv(trueType, t) && elabPredicateType1.isDefinedAt(pred) =>
        elabTestT(arg, elabPredicateType1(pred), env)
      case TestLocalCall(Id(pred, 2), List(arg1, arg2))
          if Subtype.eqv(trueType, t) && elabPredicateType22.isDefinedAt((pred, arg2)) =>
        elabTestT(arg1, elabPredicateType22(pred, arg2), env)
      case TestLocalCall(Id(pred, 2), List(arg1, arg2))
          if Subtype.eqv(trueType, t) && elabPredicateType21.isDefinedAt((pred, arg1)) =>
        elabTestT(arg2, elabPredicateType21(pred, arg1), env)
      case TestLocalCall(Id(pred, 3), List(arg1, arg2, _))
          if Subtype.eqv(trueType, t) && elabPredicateType22.isDefinedAt((pred, arg2)) =>
        elabTestT(arg1, elabPredicateType22(pred, arg2), env)
      case TestBinOp("andalso", arg1, arg2) =>
        val env1 = elabTestT(arg1, AtomLitType("true"), env)
        val env2 = elabTestT(arg2, t, env1)
        env2
      case TestBinOp("orelse", arg1, _) =>
        val env1 = elabTestT(arg1, booleanType, env)
        env1
      case _ =>
        elabTest(test, env)
    }

  def elabUnOp(unOp: TestUnOp, env: Env): Env = {
    val TestUnOp(op, arg) = unOp
    op match {
      case "not"              => elabTestT(arg, booleanType, env)
      case "bnot" | "+" | "-" => elabTestT(arg, NumberType, env)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }

  private def elabBinOp(binOp: TestBinOp, env: Env): Env = {
    val TestBinOp(op, arg1, arg2) = binOp
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
        val env1 = elabTestT(arg1, NumberType, env)
        val env2 = elabTestT(arg2, NumberType, env1)
        env2
      case "or" | "and" | "xor" =>
        val env1 = elabTestT(arg1, booleanType, env)
        val env2 = elabTestT(arg2, booleanType, env1)
        env2
      case _ =>
        val env1 = elabTest(arg1, env)
        val env2 = elabTest(arg2, env1)
        env2
    }
  }
}
