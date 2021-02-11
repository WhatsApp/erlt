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

import com.ericsson.otp.erlang._
import com.whatsapp.coralizer.ast.RemoteId
import erlang.Data._

// Conversion from Java Erlang to Scala Erlang
object DataConvert {
  def fromJava(jObject: OtpErlangObject): EObject =
    jObject match {
      case otpAtom: OtpErlangAtom =>
        EAtom(otpAtom.atomValue())
      case otpBitstr: OtpErlangBitstr =>
        EBitStr(otpBitstr.binaryValue(), otpBitstr.pad_bits())
      case otpDouble: OtpErlangDouble =>
        EDouble(otpDouble.doubleValue())
      case otpList: OtpErlangList =>
        val elems = otpList.elements().toList.map(fromJava)
        EList(elems)
      case otpLong: OtpErlangLong =>
        ELong(otpLong.bigIntegerValue())
      case otpString: OtpErlangString =>
        EString(otpString.stringValue())
      case otpMap: OtpErlangMap =>
        val otpKeys = otpMap.keys()
        val otpValues = otpKeys.map(k => otpMap.get(k))
        val eKeys = otpKeys.toList.map(fromJava)
        val eValues = otpValues.toList.map(fromJava)
        EMap(eKeys.zip(eValues))
      case otpTuple: OtpErlangTuple =>
        val elems = otpTuple.elements().toList.map(fromJava)
        ETuple(elems)
      case _ =>
        // $COVERAGE-OFF$
        sys.error(s"unexpected data $jObject")
      // $COVERAGE-ON$
    }
}