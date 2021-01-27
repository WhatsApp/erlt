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
import com.whatsapp.corq.io.EData.EObject

import java.io.DataInputStream
import java.nio.file.{Files, Paths}

object Beam {
  val Abst = 1096971124
  val Dbgi = 1147299689

  def loadAbstractForms(beamPath: String): Option[EObject] =
    Option(loadAbstractFormsJ(beamPath)).map(EData.fromJava)

  def loadAbstractFormsJ(beamPath: String): OtpErlangList = {
    val bytes = Files.readAllBytes(Paths.get(beamPath))
    val byteInputStream = new OtpInputStream(bytes)
    val input = new DataInputStream(byteInputStream)
    // "FOR1"
    input.readInt
    // length
    input.readInt
    // BEAM
    input.readInt
    var result: OtpErlangList = null
    while (result == null && input.available() > 0) {
      val intTag = input.readInt
      val length = input.readInt
      if (intTag == Dbgi || intTag == Abst) {
        val t1 = byteInputStream.read_any.asInstanceOf[OtpErlangTuple]
        val t2 = t1.elementAt(2).asInstanceOf[OtpErlangTuple]
        result = t2.elementAt(0).asInstanceOf[OtpErlangList]
      } else byteInputStream.skip((length + 3) & ~3)
    }
    result
  }
}
