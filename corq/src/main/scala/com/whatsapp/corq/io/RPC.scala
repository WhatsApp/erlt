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

package com.whatsapp.corq.io

import com.ericsson.otp.erlang._
import erlang.{CErl, CErlConvert, Data}
import erlang.Data._

class RPC(val connection: OtpConnection) extends AutoCloseable {

  def loadCoreForms(beamFilePath: String): Option[CErl.CModule] = {
    println("loading " + beamFilePath)

    connection.sendRPC("core_analyzer", "get_core_forms", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case ETuple(List(EAtom("ok"), cerlEObj)) =>
        Some(CErlConvert.convert(cerlEObj).asInstanceOf[CErl.CModule])
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
