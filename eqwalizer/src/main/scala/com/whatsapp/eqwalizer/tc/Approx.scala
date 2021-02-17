package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types._

// These operations are sound approximations...
// They should be used really carefully, - they can be sound in one context,
// but unsound in another context
object Approx {
  def asListType(t: Type): Option[ListType] =
    extractListElem(t) match {
      case Nil => None
      case ts  => Some(ListType(Subtype.join(ts)))
    }

  def asMapType(t: Type): Type =
    t match {
      case AnyType            => DictMap(AnyType, AnyType)
      case dictMap: DictMap   => dictMap
      case shapeMap: ShapeMap => shapeMap
      case UnionType(ts)      => Subtype.join(ts.map(asMapType))
      case _                  => NoneType
    }

  def getKeyType(t: Type): Type =
    t match {
      case DictMap(kt, _) => kt
      case ShapeMap(_)    => AtomType
      case UnionType(ts)  => Subtype.join(ts.map(getKeyType))
      case _ =>
        NoneType
    }

  def getValType(t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => Subtype.join(props.map(_.tp))
      case UnionType(ts)   => Subtype.join(ts.map(getValType))
      case _               => NoneType
    }

  def getValType(key: String, t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => props.find(_.key == key).map(_.tp).getOrElse(NoneType)
      case UnionType(ts)   => Subtype.join(ts.map(getValType(key, _)))
      case _               => NoneType
    }

  private def extractListElem(t: Type): List[Type] =
    t match {
      case AnyType =>
        List(AnyType)
      case UnionType(tys) =>
        tys.flatMap(extractListElem)
      case NilType =>
        List(NoneType)
      case ListType(elemType) =>
        List(elemType)
      case _ =>
        List()
    }

  def asTupleType(t: Type, arity: Int): Option[TupleType] =
    extractTupleElems(t, arity) match {
      case Nil => None
      case tss => Some(TupleType(tss.transpose.map(UnionType)))
    }

  private def extractTupleElems(t: Type, arity: Int): List[List[Type]] =
    t match {
      case AnyType =>
        List(List.fill(arity)(AnyType))
      case UnionType(tys) =>
        tys.flatMap(extractTupleElems(_, arity))
      case TupleType(argTys) =>
        if (argTys.size == arity) List(argTys) else List()
      case _ =>
        List()
    }

  def joinEnvs(envs: List[Env]): Env = {
    var envAcc: Env = envs.head
    val vars = envAcc.keySet
    for { env <- envs; v <- vars } envAcc = envAcc.updated(v, Subtype.join(envAcc(v), env(v)))
    envAcc
  }

  private def adjustShapeMap(t: ShapeMap, keyT: Type, valT: Type): Type =
    keyT match {
      case AtomLitType(key) =>
        val oldProps = t.props.filterNot(_.key == key)
        val newProps = ReqProp(key, valT) :: oldProps
        ShapeMap(newProps)
      case _ =>
        if (t.props.isEmpty)
          DictMap(keyT, valT)
        else
          DictMap(Subtype.join(AtomType, keyT), t.props.map(_.tp).fold(valT)(Subtype.join))
    }

  private def adjustDictMap(dictMap: DictMap, keyT: Type, valT: Type): Type =
    DictMap(Subtype.join(dictMap.kType, keyT), Subtype.join(dictMap.vType, valT))

  def adjustMapType(mapT: Type, keyT: Type, valT: Type): Type =
    mapT match {
      case shapeMap: ShapeMap => adjustShapeMap(shapeMap, keyT, valT)
      case dictMap: DictMap   => adjustDictMap(dictMap, keyT, valT)
      case UnionType(elems)   => Subtype.join(elems.map(adjustMapType(_, keyT, valT)))
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  def isShapeWithKey(mapT: Type, key: String): Boolean =
    mapT match {
      case shapeMap: ShapeMap =>
        shapeMap.props.exists {
          case ReqProp(k, _) => k == key
          case OptProp(_, _) => false
        }
      case UnionType(elems) => elems.forall(isShapeWithKey(_, key))
      case _                => false
    }

}
