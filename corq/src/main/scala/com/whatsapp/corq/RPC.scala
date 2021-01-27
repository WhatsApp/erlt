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

package com.whatsapp.corq

import com.ericsson.otp.erlang._
import erlang.{CErl, CErlConvert, Data}
import erlang.Data._

class RPC(val connection: OtpConnection) extends AutoCloseable {
  // switch to `get_core_forms` if you don't want to log core erlang textual format
  val FUN = "get_core_forms_pretty"

  def loadCoreForms(beamFilePath: String): Option[CErl.CModule] = {
    println("loading " + beamFilePath)

    connection.sendRPC("core_analyzer", FUN, new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    def convert(cerlEObj: Data.EObject) =
      Some(CErlConvert.convert(cerlEObj).asInstanceOf[CErl.CModule])

    eObject match {
      case ETuple(List(EAtom("ok"), cerlEObj, prettyForms)) =>
        println(prettyForms)
        convert(cerlEObj)
      case ETuple(List(EAtom("ok"), cerlEObj)) =>
        convert(cerlEObj)
      case other =>
        throw new Error(s"could not load core erlang $beamFilePath, got: $other")
    }
  }

  def close(): Unit = {
    connection.close()
  }
}

object RPC {
  def connect(): RPC = {
    val self = new OtpSelf("client")
    val other = new OtpPeer("core_analyzer@localhost")
    val connection = self.connect(other)
    new RPC(connection)
  }
}
