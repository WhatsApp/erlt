package com.whatsapp.eqwalizer.ast

object Types {
  sealed trait Type
  case class AtomLitType(atom: String) extends Type
  case object AnyFunType extends Type
  case class FunType(argTys: List[Type], resTy: Type) extends Type
  case object AnyTupleType extends Type
  case class TupleType(argTys: List[Type]) extends Type
  case object NilType extends Type
  case class ListType(t: Type) extends Type

  /** prefer `Subtype.join` over `UnionType.apply`
    */
  case class UnionType(tys: List[Type]) extends Type
  case class LocalType(id: Id, args: List[Type]) extends Type
  case class RemoteType(id: RemoteId, args: List[Type]) extends Type
  case class RawVarType(name: String) extends Type
  case class VarType(name: String, remoteId: RemoteId) extends Type
  case class RecordType(name: String) extends Type
  case class DictMap(kType: Type, vType: Type) extends Type
  case class ShapeMap(props: List[Prop]) extends Type
  case object BinaryType extends Type

  sealed trait BuiltinType extends Type
  case object AnyType extends BuiltinType
  case object AtomType extends BuiltinType
  case object NoneType extends BuiltinType
  case object NumberType extends BuiltinType
  case object PidType extends BuiltinType
  case object PortType extends BuiltinType
  case object ReferenceType extends BuiltinType

  case class ConstrainedFunType(ty: FunType, constraints: List[Constraint])
  case class Constraint(tVar: String, ty: Type)

  sealed trait Prop {
    val key: String
    val tp: Type
  }
  case class ReqProp(key: String, tp: Type) extends Prop
  case class OptProp(key: String, tp: Type) extends Prop

  val falseType: Type = AtomLitType("false")
  val trueType: Type = AtomLitType("true")
  val booleanType: Type = UnionType(List(falseType, trueType))
  val floatType: Type = NumberType
  val integerType: Type = NumberType
  val undefined: Type = AtomLitType("undefined")

  val builtinTypes: Map[String, Type] =
    Map(
      "any" -> AnyType,
      "atom" -> AtomType,
      "binary" -> BinaryType,
      "bitstring" -> BinaryType,
      "boolean" -> booleanType,
      "byte" -> NumberType,
      "char" -> NumberType,
      "float" -> floatType,
      "list" -> ListType(AnyType),
      "pos_integer" -> NumberType,
      "neg_integer" -> NumberType,
      "non_neg_integer" -> NumberType,
      "integer" -> integerType,
      "none" -> NoneType,
      "number" -> NumberType,
      "pid" -> PidType,
      "port" -> PortType,
      "reference" -> ReferenceType,
      "term" -> AnyType,
      "arity" -> NumberType,
      "identifier" -> UnionType(List(PidType, PortType, ReferenceType)),
      "mfa" -> TupleType(List(AtomType, AtomType, NumberType)),
      "module" -> AtomType,
      "node" -> AtomType,
      "no_return" -> NoneType,
      "timeout" -> UnionType(List(AtomLitType("infinity"), NumberType)),
    )
}
