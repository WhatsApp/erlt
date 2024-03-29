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

package com.whatsapp.sterlang.dev

import java.io.{PrintWriter, StringWriter}
import java.util.concurrent.{Executor, Executors}

import sys.process._
import com.ericsson.otp.erlang._
import com.whatsapp.sterlang._
import com.whatsapp.sterlang.Etf._

object SterlangD {
  def main(args: Array[String]): Unit =
    args match {
      case Array()    => serve(1)
      case Array(par) => serve(par.toIntOption.getOrElse(1))
      case _          => Console.out.println("StErlang. More info: https://github.com/WhatsApp/erlt")
    }

  def serve(par: Int): Unit = {
    "epmd -daemon".!!
    val node = new OtpNode("sterlangd@localhost")
    new Server(node.createMbox("api"), Executors.newFixedThreadPool(par)).serve()
    node.close()
  }

  private[SterlangD] class Server(mbox: OtpMbox, executor: Executor) {
    @scala.annotation.tailrec
    final def serve(): Unit = {
      val msg = Etf.fromJava(mbox.receive())
      msg match {
        case ETuple(List(EAtom("check"), EPid(from), ref: ERef, EString(etfFile))) =>
          executor.execute(() => handleCheck(from, ref, etfFile))
        case EAtom("exit") =>
          return
      }
      serve()
    }

    private def handleCheck(from: OtpErlangPid, ref: ERef, etfFile: String): Unit = {
      val start = System.currentTimeMillis()
      val result =
        try processFile(etfFile) match {
          case Left(error) =>
            convertError(error)
          case Right(hoverSpecs) =>
            ETuple(List(EAtom("ok"), EList(hoverSpecs.map(Etf.hoverTypeInfoToEMap))))
        } catch {
          case x: Throwable =>
            val msg =
              s"""***** Sterlang internal error *****
                 |${getStackTraceAsString(x)}
                 |""".stripMargin
            ETuple(List(EAtom("error"), EAtom("none"), EString(msg)))
        }
      val sterlangTime = System.currentTimeMillis() - start
      val response = ETuple(List(ref, result, ELong(sterlangTime)))
      mbox.send(from, Etf.toJava(response))
    }

    private def processFile(etfFile: String): Either[SterlangError, List[Doc.LspTypeInfo]] = {
      val rawProgram =
        try DriverErltc.loadProgram(etfFile)
        catch {
          case error: SterlangError => return Left(error)
        }
      val vars = new Vars()
      val program = AstUtil.normalizeTypes(rawProgram)
      val context = DriverErltc.loadContext(etfFile, program, vars).extend(program)
      try {
        val astChecks = new AstChecks(context)
        astChecks.check(program)
        val elaborate = new Elaborate(vars, context, program)
        val (annDefs, env) = elaborate.elaborate()
        val render = new Render(vars)
        val hoverSpecs = render.hoverSpecs(program, annDefs, env)
        val elabTypes = render.varTypes(annDefs)
        val elabVarHovers = elabTypes.map {
          case Doc.ElaboratedTypeInfo(_, range, typeRepr) => Doc.LspTypeInfo(range, typeRepr, kind = "hover")
        }
        Right(hoverSpecs ++ elabVarHovers)
      } catch {
        case error: SterlangError => Left(error)
      }
    }
  }

  private def convertPos(pos: Doc.Pos): ETerm =
    ETuple(List(ELong(pos.line.toLong), ELong(pos.column.toLong)))

  private def convertRange(range: Doc.Range): ETerm =
    ETuple(List(convertPos(range.start), convertPos(range.end)))

  private def convertError(error: SterlangError): ETerm =
    error match {
      case PosError(pos, title) =>
        ETuple(List(EAtom("error"), convertPos(pos), EString(title)))
      case RangeError(range, title, description) =>
        val msg = (List(title) ++ description.toList).mkString("\n")
        ETuple(List(EAtom("error"), convertRange(range), EString(msg)))
    }

  private def getStackTraceAsString(t: Throwable) = {
    val sw = new StringWriter
    t.printStackTrace(new PrintWriter(sw))
    sw.toString
  }
}
