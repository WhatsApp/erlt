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

package com.whatsapp.analyzer

import com.ericsson.otp.erlang._
import erlang.Data._

class RPC(val connection: OtpConnection) {

  def getUsedFuns(beamFilePath: String): Option[List[(String, String, Int)]] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "used_funs", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        val funs = elems.collect {case ETuple(List(EAtom(mod), EAtom(fun), ELong(arity))) => (mod, fun, arity.toInt)}
        Some(funs)
      case _ =>
        println("not loaded")
        None
    }
  }

  def getExports(beamFilePath: String): Option[List[(String, Int)]] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "exports", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        val funs = elems.collect {case ETuple(List(EAtom(fun), ELong(arity))) => (fun, arity.toInt)}
        Some(funs)
      case _ =>
        println("not loaded")
        None
    }
  }

  def getBehaviours(beamFilePath: String): Option[List[String]] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "behaviours", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        val behaviours = elems.collect {case EAtom(behaviour) => behaviour}
        Some(behaviours)
      case _ =>
        println("not loaded")
        None
    }
  }

  def close(): Unit = {
    connection.close()
  }
}

object RPC {
  def connect(): RPC = {
    val self = new OtpSelf("client")
    val other = new OtpPeer("analyzer@localhost")
    val connection = self.connect(other)
    new RPC(connection)
  }
}
