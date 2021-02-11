package com.whatsapp.coralizer.tc

import com.whatsapp.coralizer.ast.Forms.FunSpec
import com.whatsapp.coralizer.ast.{Id, RemoteId}
import com.whatsapp.coralizer.ast.Types._
import erlang.CErl._
import erlang.Data._

object BuiltIn {
  private def exceptionClassType: Type =
    UnionType(List("throw", "error", "exit") map AtomLitType)
  def catchMatchNoStackType: Type =
    TupleType(List(exceptionClassType, /* Msg */ AnyType))
  def catchMatchWithStackTypes: List[Type] =
    List(
      exceptionClassType, /* Msg */ AnyType, /* Dummy Var, never used */ AnyType
    )

  private def moduleInfoArg: Type =
    UnionType(
      List(
        AtomLitType("module"),
        AtomLitType("attributes"),
        AtomLitType("compile"),
        AtomLitType("md5"),
        AtomLitType("exports"),
        AtomLitType("functions"),
        AtomLitType("nifs"),
        AtomLitType("native")
      )
    )

  val MakeFun = RemoteId("erlang", "make_fun", 3)

  /**
    * these functions are automatically defined in each Core Erlang module
    */
  def moduleInfoSpecs: Map[Id, FunSpec] =
    List(
      ("module_info", 0, Nil, ListType(AnyType)),
      ("module_info", 1, List(moduleInfoArg), AnyType)
    ).map {
      case (name, arity, argTys: List[Type], retTy: Type) =>
        val id = Id(name, arity)
        id -> FunSpec(
          id,
          List(ConstrainedFunType(FunType(argTys, retTy), Nil))
        )(0)
    }.toMap

  case class unSpec(id: Id, types: List[ConstrainedFunType])(val line: Int)

  val afterId = Id("after$^0", 0)
  val receiveId = Id("recv$^0", 0)

  val letRecSpecialFunToType = Map(
    afterId -> FunType(Nil, AnyType),
    receiveId -> FunType(Nil, AnyType)
  )

  def primOpToReturnType(primOp: CPrimOp): Type = {
    primOp match {
      case CPrimOp(_, CLiteral(_, EAtom(name)), _args) =>
        name match {
          case "match_fail" | "raise" => NoneType
          case "recv_peek_message"    => CValuesType(List(booleanType, AnyType))
          case "dialyzer_unknown"     => AnyType
          case "remove_message"       => AnyType
          case "recv_wait_timeout"    => booleanType
          case "timeout"              => AnyType
          // dialyzer checks for these, but I haven't found any yet
          //  case "bs_init_writable" => BinaryType,
          //  case "build_stacktrace" => ListType(AnyType)
          // $COVERAGE-OFF$
          case _ => sys.error(s"unexpected $name")
          // $COVERAGE-ON$
        }
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $primOp")
      // $COVERAGE-ON$
    }
  }

}
