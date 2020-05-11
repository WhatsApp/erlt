package com.whatsapp.sterlang.test.it

import java.io.File

import com.whatsapp.sterlang._

object SterlangTestUtil {
  import java.io.{BufferedWriter, FileWriter, StringWriter}

  val generateOut = false

  def processFile(path: String, mode: TypePrinter2.Mode, tmpExt: String, outExt: String): Unit = {
    val file = new File(path)
    val rawProgram = Main.loadProgram(file, false)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Main.loadContext(file, program, vars).extend(program)
    new AstChecks(context).check(program)
    val (annDefs, env) = new Elaborate(vars, context, program).elaborateFuns(program.funs)

    val sw = new StringWriter
    val printer = TypePrinter2(vars, Some(sw))
    mode match {
      case TypePrinter2.TypeSchemes =>
        printer.printFunsTypeSchemes(annDefs, env)
      case TypePrinter2.Types =>
        printer.printFuns(annDefs)
    }

    {
      val w2 = new BufferedWriter(new FileWriter(path + "." + tmpExt))
      w2.write(sw.toString)
      w2.close()
    }

    if (generateOut) {
      val w = new BufferedWriter(new FileWriter(path + "." + outExt))
      w.write(sw.toString)
      w.close()
    }
  }

  def processIllTyped(path: String): Boolean = {
    val file = new File(path)
    val rawProgram = Main.loadProgram(file, false)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = Main.loadContext(file, program, vars).extend(program)
      new AstChecks(context).check(program)
      new Elaborate(vars, context, program).elaborateFuns(program.funs)
      false
    } catch {
      case _:PositionedError => true
    }
  }
}
