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

import java.io.File

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

  def getMultiSpecs(beamFilePath: String): Option[List[(String, Int)]] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "multi_specs", new OtpErlangList(new OtpErlangString(beamFilePath)))
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

  def getGenServerCalls(beamFilePath: String): Option[(Int, Int, Int)] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "gen_server_calls", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    eObject match {
      case ETuple(List(ELong(total), ELong(tagged), ELong(others))) =>
        Some((total.toInt, tagged.toInt, others.toInt))
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

  def getPatternMatches(beamFilePath: String): PatternMatches.PatternMatches = {
    System.err.println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "pattern_matching_constructs", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    val module = new File(beamFilePath).getName.split("""\.""")(0)

    eObject match {
      case ETuple(List(ELong(matches), ELong(clauses))) =>
        PatternMatches.PatternMatches(module, matches.toInt, clauses.toInt)
      case _ =>
        System.err.println("not loaded")
        PatternMatches.PatternMatches(module, 0, 0)
    }
  }

  def getNonlinearClauses(beamFilePath: String): List[NonlinearClauses.NonlinearClause] = {
    System.err.println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "nonlinear_clauses", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    val module = new File(beamFilePath).getName.split("""\.""")(0)

    eObject match {
      case EList(elems, _) =>
        elems.collect {
          case ETuple(List(ELong(line), isCovered: EAtom)) =>
            NonlinearClauses.NonlinearClause(module, line.toInt, isCovered.asBoolean())
        }
      case _ =>
        System.err.println("not loaded")
        List.empty
    }
  }

  def getGuardedClauses(beamFilePath: String): List[GuardedClauses.GuardedClause] = {
    System.err.println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "guarded_clauses", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    val module = new File(beamFilePath).getName.split("""\.""")(0)

    eObject match {
      case EList(elems, _) =>
        elems.collect {
          case ETuple(List(ELong(line), isCovered: EAtom, isGuardTypeTestOnly: EAtom)) =>
            GuardedClauses.GuardedClause(module, line.toInt, isCovered.asBoolean(), isGuardTypeTestOnly.asBoolean())
        }
      case _ =>
        System.err.println("not loaded")
        List.empty
    }
  }

  def getAndPatterns(beamFilePath: String): List[AndPatterns.AndPattern] = {
    System.err.println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "compound_patterns", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val eObject = erlang.DataConvert.fromJava(received)

    val module = new File(beamFilePath).getName.split("""\.""")(0)

    eObject match {
      case EList(elems, _) =>
        elems.collect {
          case ETuple(List(ELong(line), isRenameOnly: EAtom)) =>
            AndPatterns.AndPattern(module, line.toInt, isRenameOnly.asBoolean())
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

  def getRedefinedRecordTypes(beamFilePath: String): List[(Int, String)] = {
    println("loading " + beamFilePath)
    connection.sendRPC("analyzer", "redefined_record_types", new OtpErlangList(new OtpErlangString(beamFilePath)))
    val received = connection.receiveRPC
    val EList(data, _) = erlang.DataConvert.fromJava(received)
    data.map { case ETuple(List(ELong(line), EAtom(name))) => (line.toInt, name) }
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
