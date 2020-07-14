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

class RPC(val connection: OtpConnection) extends AutoCloseable {

  def getForms(beamFilePath: String): EObject = {
    connection.sendRPC("analyzer", "forms", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    erlang.DataConvert.fromJava(received)
  }

  def getUsedFuns(beamFilePath: String): Option[List[(String, String, Int)]] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "used_funs", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        val funs = elems.collect { case ETuple(List(EAtom(mod), EAtom(fun), ELong(arity))) => (mod, fun, arity.toInt) }
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
        val funs = elems.collect { case ETuple(List(EAtom(fun), ELong(arity))) => (fun, arity.toInt) }
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
        val behaviours = elems.collect { case EAtom(behaviour) => behaviour }
        Some(behaviours)
      case _ =>
        println("not loaded")
        None
    }
  }

  def getBifClashes(beamFilePath: String): List[(String, String, Int)] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "bif_clashes", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        elems.collect {
          case ETuple(List(EAtom(module), EAtom(name), ELong(arity))) =>
            (module, name, arity.toInt)
        }
      case _ =>
        println("not loaded")
        List.empty
    }
  }

  def getReceives(beamFilePath: String): Int = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "receives", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case ELong(count) =>
        count.toInt
      case _ =>
        println("not loaded")
        0
    }
  }

  def getNonlinearPatterns(beamFilePath: String): List[(String, Int)] = {
    System.err.println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "nonlinear_patterns", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        elems.collect {
          case ETuple(List(EAtom(name), ELong(arity))) =>
            (name, arity.toInt)
        }
      case _ =>
        System.err.println("not loaded")
        List.empty
    }
  }

  def getFunctions(beamFilePath: String): Int = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "functions", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case ELong(count) =>
        count.toInt
      case _ =>
        println("not loaded")
        0
    }
  }

  def getPrimitives(beamFilePath: String, primCategory: String): List[String] = {
    println("loading " + beamFilePath)
    connection.sendRPC(
      "analyzer",
      "used_primitives",
      new OtpErlangList(Array[OtpErlangObject](new OtpErlangString(beamFilePath), new OtpErlangAtom(primCategory))),
    )
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        elems.collect { case EString(s) => s }
      case _ =>
        println("not loaded")
        List.empty
    }
  }

  def getDynamicCalls(beamFilePath: String): List[String] = {
    println("loading " + beamFilePath)
    connection.sendRPC(
      "analyzer",
      "dynamic_calls",
      new OtpErlangList(new OtpErlangString(beamFilePath)),
    )
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        elems.collect { case EString(s) => s }
      case _ =>
        println("not loaded")
        List.empty
    }
  }

  def getOtpEbinDirs(): List[String] = {
    connection.sendRPC(
      "code",
      "get_path",
      new OtpErlangList(),
    )
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case EList(elems, _) =>
        elems.collect { case EString(s) if s.endsWith("ebin") => s }
      case _ =>
        sys.error(s"can not get otp dirs: $eObject")
    }
  }

  def getErrorHandling(beamFilePath: String): Option[(Int, List[Int])] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "error_handling", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case ETuple(
            List(
              ETuple(List(EAtom("catches"), ELong(catches))),
              ETuple(List(EAtom("tries"), EList(rawTries, _))),
            )
          ) =>
        val tries = rawTries.map { case ETuple(List(ELong(catchClauses))) => catchClauses.toInt }
        val result = (catches.toInt, tries)
        Some(result)
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
