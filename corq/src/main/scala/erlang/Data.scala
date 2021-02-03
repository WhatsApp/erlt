/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// copied from analyzer
package erlang

// Scala-idiomatic representation of Erlang Data
object Data {
  sealed trait EObject
  case class Anno() extends EObject
  case class EAtom(atom: String) extends EObject {
    def asBoolean(): Boolean =
      atom match {
        case "true"  => true
        case "false" => false
        case other   => sys.error(s"$other is not boolean")
      }
  }
  case class EBitStr(bin: Array[Byte], pad_bits: Int) extends EObject
  case class EDouble(d: Double) extends EObject
  case class EExternalFun(module: String, function: String, arity: Int)
      extends EObject
  case class EList(elems: List[EObject], lastTail: Option[EObject])
      extends EObject
  case class ELong(value: BigInt) extends EObject
  case class EMap(entries: List[(EObject, EObject)]) extends EObject
  case class EPid(node: String, id: Int, serial: Int, creation: Int)
      extends EObject
  case class EPort(node: String, id: Int, creation: Int) extends EObject
  case class ERef(node: String, creation: Int, ids: List[Int]) extends EObject
  case class EString(str: String) extends EObject
  case class ETuple(elems: List[EObject]) extends EObject
}
