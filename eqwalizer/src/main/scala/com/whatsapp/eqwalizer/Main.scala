package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.{DB, Id}
import com.whatsapp.eqwalizer.util.{TcDiagnosticsText, WIPDiagnosticsText}

object Main {
  sealed trait Cmd
  case object Check extends Cmd
  case object Debug extends Cmd

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      help()
      return
    }
    val cmd: Cmd = args(0) match {
      case "check" => Check
      case "debug" => Debug
      case _       => help(); return
    }

    val (module, idOpt) = mfa(args(1))
    DB.beamLocation(module) match {
      case None =>
        Console.err.println(s"Cannot locate beam file for module $module")
      case Some(beamFile) =>
        Console.println(s"Loading forms from $beamFile")

        val feedback = cmd match {
          case Check =>
            idOpt match {
              case Some(id) =>
                TcDiagnosticsText.checkFun(beamFile, id).mkString("", "\n", "\n")
              case None =>
                TcDiagnosticsText.checkFile(beamFile).mkString("", "\n", "\n")
            }

          case Debug =>
            WIPDiagnosticsText.loadForms(beamFile).mkString("", "\n", "\n")
        }

        Console.println(feedback)
    }
  }

  private def help(): Unit = {
    Console.println("com.whatsapp.eqwalizer.test.util.TypeCheckModule")
    Console.println("usage:")
    Console.println("  check <module_name>")
    Console.println("  debug <module_name>")
  }

  private def mfa(s: String): (String, Option[Id]) = {
    s.split(':') match {
      case Array(module) =>
        (module, None)
      case Array(module, fun) =>
        fun.split('/') match {
          case Array(f, arity) => (module, Some(Id(f, arity.toInt)))
          case _               => sys.error(s"Cannot parse $s")
        }
      case _ =>
        sys.error(s"Cannot parse $s")
    }
  }
}
