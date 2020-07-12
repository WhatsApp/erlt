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

package com.whatsapp.sterlang

import scala.language.postfixOps
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}

trait PTokens extends StdTokens {
  case class CharLit(char: Char) extends Token {
    override def toString = char.toString
    override val chars: String = char.toString
  }
}

class PLexical extends StdLexical with PTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh
  override def whitespace: Parser[Any] = rep(whitespaceChar | ('%' ~ rep(chrExcept(EofCh, '\n'))))
  override def token: Parser[Token] =
    '$' ~> letter ^^ CharLit |
      '\'' ~> rep(chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { chars =>
        Identifier(chars mkString "")
      } | (rep1(digit) ~ '.' ~ rep1(digit)) ^^ { case s1 ~ _ ~ _ => NumericLit(s1.mkString("")) } |
      super.token
}

object Parser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  // --------- LOW-LEVEL STUFF ------------
  override val lexical: PLexical = new PLexical
  def charLit: Parser[String] =
    elem("number", _.isInstanceOf[lexical.CharLit]) ^^ (_.chars)
  val S = Ast
  lexical.reserved ++=
    Seq(
      "_",
      "begin",
      "end",
      "true",
      "false",
      "when",
      "fun",
      "case",
      "of",
      "if",
      "then",
      "else",
    ) ++ S.unOps1.keys ++ S.binOps1.keys
  lexical.delimiters ++=
    Seq(
      "(",
      ")",
      "{",
      "}",
      ",",
      "=",
      "...",
      ";",
      ".",
      ":=",
      "=>",
      "<",
      ">",
      "[",
      "]",
      "::",
      "=>",
      "|",
      ":",
      "#",
      "->",
      "-",
    ) ++ S.unOps2.keys ++ S.binOps2.keys

  lazy val lident: PackratParser[String] =
    ident ^? { case id if id.startsWith("_") || id.charAt(0).isLower => id }
  lazy val uident: PackratParser[String] =
    ident ^? { case id if id.startsWith("_") || id.charAt(0).isUpper => id }
  private def pos[T](p: => Parser[Pos.P => T]): Parser[T] =
    Parser { in1 =>
      p(in1) match {
        case Success(t, in2) =>
          val pos1 = Pos.Loc(in1.pos.line, in1.pos.column)
          val pos2 = Pos.Loc(in2.pos.line, in2.pos.column)
          Success(t(Pos.SP(pos1, pos2)), in2)
        case Error(msg, next)   => Error(msg, next)
        case Failure(msg, next) => Failure(msg, next)
      }
    }

  // --------- TYPES ---------------
  lazy val wildTypeVar: PackratParser[S.WildTypeVar] =
    pos("_" ^^^ { S.WildTypeVar()(_) })
  lazy val typeVar: PackratParser[S.TypeVar] =
    pos(uident ^^ { vn => S.TypeVar(vn)(_) })
  lazy val tupleType: PackratParser[S.TupleType] =
    pos("{" ~> repsep(tp, ",") <~ "}" ^^ { elems => S.TupleType(elems)(_) })
  lazy val recordType: PackratParser[S.RecordType] =
    pos(("#" ~ "{") ~> repsep(recFieldType, ",") <~ "}" ^^ { fs => S.RecordType(fs)(_) })
  lazy val openRecordType: PackratParser[S.OpenRecordType] =
    pos(("#" ~ "{") ~> rep(recFieldType <~ ",") ~ pos(("_" ~ ":=" ~ "_") ^^^ { S.WildTypeVar()(_) }) <~ "}" ^^ {
      case fs ~ w => S.OpenRecordType(fs, w)(_)
    })
  lazy val mapType: PackratParser[S.UserType] =
    pos(("#" ~ "{") ~> tp ~ ("=>" ~> tp <~ "}") ^^ { case k ~ v => S.UserType(S.LocalName("map"), List(k, v))(_) })
  lazy val funType: PackratParser[S.FunType] =
    pos((("fun" ~ "(") ~> typeArgs) ~ ("->" ~> tp <~ ")") ^^ { case params ~ res => S.FunType(params, res)(_) })
  lazy val listType: PackratParser[S.ListType] =
    pos("[" ~> tp <~ "]" ^^ { elemTp => S.ListType(elemTp)(_) }) |
      pos(((lident ^? { case "list" => "list" }) ~ "(") ~> tp <~ ")" ^^ { elemTp => S.ListType(elemTp)(_) })
  lazy val userType: PackratParser[S.UserType] =
    pos(typeName ~ typeArgs ^^ { case n ~ args => S.UserType(n, args)(_) })

  lazy val typeName: PackratParser[S.Name] =
    (((lident <~ ":") ~ lident) ^^ S.RemoteName) | (lident ^^ S.LocalName)

  lazy val varName: PackratParser[S.VarName] =
    (lident <~ ":") ~ lident ~ (guard(args) ^^ { _.size }) ^^ {
      case id1 ~ id2 ~ a => new S.RemoteFunName(id1, id2, a)
    } |
      lident ~ (guard(args) ^^ { _.size }) ^^ { case id ~ a => new S.LocalFunName(id, a) } |
      uident ^^ { new S.LocalVarName(_) }

  lazy val remoteFunName: PackratParser[S.RemoteFunName] =
    ((lident <~ ":") ~ lident) ~ ("/" ~> numericLit ^^ { _.toInt }) ^^ {
      case id1 ~ id2 ~ a => new S.RemoteFunName(id1, id2, a)
    }
  lazy val localFunName: PackratParser[S.LocalFunName] =
    lident ~ ("/" ~> numericLit ^^ { _.toInt }) ^^ { case id ~ a => new S.LocalFunName(id, a) }
  lazy val varName2: PackratParser[S.VarName] =
    remoteFunName | localFunName
  lazy val funRef: PackratParser[S.VarExp] =
    pos("fun" ~> varName2 ^^ { vn => S.VarExp(vn)(_) })

  lazy val tp: PackratParser[S.Type] =
    "(" ~> tp <~ ")" | (uident ~ "::") ~> tp | listType | tupleType | recordType | openRecordType |
      mapType | funType | userType | wildTypeVar | typeVar

  lazy val specTp: PackratParser[S.Spec] =
    pos(lident ~ typeArgs ~ ("->" ~> tp) ^^ {
      case name ~ params ~ res =>
        (p: Pos.P) => S.Spec(new S.LocalFunName(name, params.size), S.FunType(params, res)(p))(p)
    })

  lazy val enumCon: PackratParser[S.EnumCon] =
    pos(lident ~ ("{" ~> repsep(tp, ",") <~ "}") ^^ { case n ~ args => S.EnumCon(n, args)(_) })

  lazy val enumCons: PackratParser[List[S.EnumCon]] =
    repsep(enumCon, "|")

  lazy val typeParams: PackratParser[List[S.TypeVar]] =
    "(" ~> repsep(typeVar, ",") <~ ")"

  lazy val typeArgs: PackratParser[List[S.Type]] =
    "(" ~> repsep(tp, ",") <~ ")"

  lazy val recFieldType: PackratParser[S.Field[S.Type]] =
    lident ~ (":=" ~> tp) ^^ S.Field[S.Type]

  lazy val enumDef: PackratParser[S.EnumDef] =
    pos((attr("enum") ~> lident) ~ typeParams ~ ("::" ~> enumCons <~ ".") ^^ {
      case name ~ params ~ cons => S.EnumDef(name, params, cons)(_)
    })

  lazy val enumDefs: PackratParser[List[S.EnumDef]] =
    enumDef *

  // --- PATTERNS ---
  lazy val pat: PackratParser[S.Pat] =
    andPat | wildPat | ("(" ~> pat <~ ")") | tuplePat |
      recordPat | listPat | consPat | enumCtrPat | boolPat | numberPat | stringPat | varPat
  lazy val valPat: PackratParser[S.Pat] =
    wildPat | ("(" ~> pat <~ ")") | tuplePat |
      recordPat | listPat | consPat | enumCtrPat | boolPat | numberPat | stringPat | varPat

  lazy val boolPat: PackratParser[S.BoolPat] =
    pos(("true" | "false") ^^ { l => S.BoolPat(l.toBoolean)(_) })
  lazy val numberPat: PackratParser[S.NumberPat] =
    pos(numericLit ^^ { l => S.NumberPat(l.toInt)(_) })
  lazy val stringPat: PackratParser[S.StringPat] =
    pos(stringLit ^^ { s => S.StringPat(s.replace("\\n", "\n"))(_) })
  lazy val consPat: PackratParser[S.ConsPat] =
    pos(("[" ~> pat <~ "|") ~ (pat <~ "]") ^^ { case h ~ t => S.ConsPat(h, t)(_) })
  lazy val tuplePat: PackratParser[S.TuplePat] =
    pos("{" ~> repsep(pat, ",") <~ "}" ^^ { elems => S.TuplePat(elems)(_) })
  lazy val recordPat: PackratParser[S.RecordPat] =
    pos(("#" ~ "#" ~ "{") ~> rowPat <~ "}" ^^ { rows => S.RecordPat(rows, true)(_) }) |
      pos(("#" ~ "{") ~> rowPat <~ "}" ^^ { rows => S.RecordPat(rows, false)(_) })
  lazy val enumCtrPat: PackratParser[S.EnumCtrPat] =
    pos(enumCtr ~ repsep(pat, ",") <~ "}" ^^ { case en ~ dc ~ pats => S.EnumCtrPat(en, dc, pats)(_) })
  lazy val listPat: PackratParser[S.ListPat] =
    pos(("[" ~> repsep(pat, ",") <~ "]") ^^ { pats => S.ListPat(pats)(_) })
  lazy val andPat: PackratParser[S.AndPat] =
    pos(pat ~ ("=" ~> pat) ^^ { case p1 ~ p2 => S.AndPat(p1, p2)(_) })
  lazy val wildPat: PackratParser[S.WildPat] =
    pos("_" ^^^ { S.WildPat()(_) })
  lazy val varPat: PackratParser[S.VarPat] =
    pos(uident ^^ { id => S.VarPat(id)(_) })

  lazy val ellipsisPat: PackratParser[String] =
    "..." ^^^ "..."

  lazy val rowPat: PackratParser[List[S.Field[S.Pat]]] =
    repsep(fieldPat, ",")

  lazy val fieldPat: Parser[S.Field[S.Pat]] =
    ((label <~ ":=") ~ pat) ^^ S.Field[S.Pat]

  // --------- EXPRESSIONS: FUNS, VALS, CLAUSES ----------
  lazy val fun: PackratParser[S.Fun] =
    pos(rep1sep(funClause, ";") <~! "." ^? {
      case ps if ps.map(_._1).toSet.size == 1 =>
        S.Fun(new S.LocalFunName(ps.head._1, ps.head._2.pats.size), ps.map(_._2))(_)
    })
  lazy val funClause: PackratParser[(String, S.Clause)] =
    lident ~ clause ^^ Tuple2[String, S.Clause]
  lazy val localFunClause: PackratParser[(String, S.Clause)] =
    uident ~ clause ^^ Tuple2[String, S.Clause]
  lazy val valDef: PackratParser[S.ValDef] =
    valPat ~ ("=" ~> exp) ^^ S.ValDef | exp ^^ { S.ValDef(S.WildPat()(Pos.NP), _) }
  lazy val body: PackratParser[S.Body] =
    rep(valDef <~ ",") ~ valDef ^^ S.Body
  lazy val clause: PackratParser[S.Clause] =
    ("(" ~> repsep(pat, ",") <~ ")") ~ guards ~ ("->" ~> body) ^^ S.Clause

  // --------- EXPRESSIONS: FUNS, VALS, CLAUSES ----------
  lazy val exp: PackratParser[S.Exp] =
    ifExp | caseExp | recordUpdateExp | binOpExp | fnExp | namedFnExp | enumConExp | appExp
  lazy val guardElem: PackratParser[S.Guard] =
    rep1sep(exp, ",") ^^ S.Guard
  lazy val guards: PackratParser[List[S.Guard]] =
    (("when" ~> rep1sep(guardElem, ";")) ?) ^^ { _.getOrElse(List()) }
  private lazy val ifExp: PackratParser[S.Exp] =
    pos(("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
      case e1 ~ e2 ~ e3 => S.IfExp(e1, e2, e3)(_)
    })
  private lazy val fnExp: PackratParser[S.Exp] =
    pos("fun" ~> rep1sep(clause, ";") <~ "end" ^^ { cs => S.FnExp(cs)(_) })
  private lazy val namedFnExp: PackratParser[S.Exp] =
    pos("fun" ~> rep1sep(localFunClause, ";") <~ "end" ^? {
      case ps if ps.map(_._1).toSet.size == 1 =>
        S.NamedFnExp(new S.LocalVarName(ps.head._1), ps.map(_._2))(_)
    })
  private lazy val caseExp: PackratParser[S.Exp] =
    pos(("case" ~> exp <~ "of") ~ rep1sep(rule, ";") <~ "end" ^^ { case e ~ rs => S.CaseExp(e, rs)(_) })

  lazy val binOpExp: PackratParser[S.BinOpExp] =
    (S.binOps1 ++ S.binOps2)
      .map {
        case (k, v) => pos((exp <~ k) ~ exp ^^ { case p ~ q => S.BinOpExp(v, p, q)(_) })
      }
      .reduce(_ | _)
  lazy val recordUpdateExp: PackratParser[S.RecordUpdateExp] =
    pos((exp ~ recordDelta) ^^ { case e ~ delta => S.RecordUpdateExp(e, delta)(_) })

  lazy val unOpExp: PackratParser[S.Exp] =
    (S.unOps1 ++ S.unOps2).map { case (k, v) => pos(k ~> exp ^^ { e => S.UOpExp(v, e)(_) }) }.reduce(_ | _)

  lazy val enumCtr: PackratParser[S.Name ~ String] =
    ((((lident <~ ".") ~ lident) ^^ S.RemoteName) ~ ("." ~> lident <~ "{")) |
      ((lident ^^ S.LocalName) ~ ("." ~> lident <~ "{"))

  lazy val appExp: PackratParser[S.Exp] =
    pos(appExp ~ args ^^ { case f ~ args => S.AppExp(f, args)(_) }) | selExp | unOpExp
  lazy val enumConExp: PackratParser[S.EnumConExp] =
    pos(enumCtr ~ repsep(exp, ",") <~ "}" ^^ { case en ~ dc ~ args => S.EnumConExp(en, dc, args)(_) })
  lazy val selExp: PackratParser[S.Exp] =
    pos(selExp ~ (("." ~ guard(not(funClause))) ~> label) ^^ { case rec ~ lbl => S.SelExp(rec, lbl)(_) }) | atomicExp
  lazy val args: PackratParser[List[S.Exp]] =
    "(" ~> repsep(exp, ",") <~ ")"

  lazy val literalExp: PackratParser[S.Exp] =
    boolExp | numberExp | charExp | stringExp | varExp | funRef

  lazy val boolExp: PackratParser[S.BoolExp] =
    pos("true" ^^^ { S.BoolExp(true)(_) } | "false" ^^^ { S.BoolExp(false)(_) })
  lazy val numberExp: PackratParser[S.NumberExp] =
    pos(numericLit ^^ { l => S.NumberExp(l.toInt)(_) })
  lazy val charExp: PackratParser[S.CharExp] =
    pos(charLit ^^ { l => S.CharExp(l)(_) })
  lazy val stringExp: PackratParser[S.StringExp] =
    pos(stringLit ^^ { s => S.StringExp(s.replace("\\n", "\n"))(_) })
  lazy val varExp: PackratParser[S.VarExp] =
    pos(varName ^^ { vn => S.VarExp(vn)(_) })

  lazy val atomicExp: PackratParser[S.Exp] =
    literalExp | tupleExp | recordExp | listExp | consExp | blockExp | ("(" ~> exp <~ ")")

  lazy val tupleExp: PackratParser[S.TupleExp] =
    pos(("{" ~> repsep(exp, ",") <~ "}") ^^ { elems => S.TupleExp(elems)(_) })
  lazy val recordExp: PackratParser[S.RecordExp] =
    pos(("#" ~ "{") ~> rowExps <~ "}" ^^ { rows => S.RecordExp(rows)(_) })
  lazy val recordDelta: PackratParser[S.RecordExp] =
    pos(("#" ~ "{") ~> updateRowExps <~ "}" ^^ { rows => S.RecordExp(rows)(_) })
  lazy val listExp: PackratParser[S.ListExp] =
    pos(("[" ~> repsep(exp, ",") <~ "]") ^^ { elems => S.ListExp(elems)(_) })
  lazy val consExp: PackratParser[S.ConsExp] =
    pos(("[" ~> exp) ~ ("|" ~> exp <~ "]") ^^ { case h ~ t => S.ConsExp(h, t)(_) })
  lazy val blockExp: PackratParser[S.BlockExpr] =
    pos(("begin" ~> body <~ "end") ^^ { e => S.BlockExpr(e)(_) })

  lazy val rowExps: PackratParser[List[S.Field[S.Exp]]] =
    repsep(fieldExp, ",")
  lazy val fieldExp: PackratParser[S.Field[S.Exp]] =
    ((label <~ "=>") ~ exp) ^^ S.Field[S.Exp]
  lazy val updateRowExps: PackratParser[List[S.Field[S.Exp]]] =
    repsep(updateFieldExp, ",")
  lazy val updateFieldExp: PackratParser[S.Field[S.Exp]] =
    ((label <~ ":=") ~ exp) ^^ S.Field[S.Exp]

  lazy val rule: PackratParser[S.Rule] =
    pat ~ guards ~ ("->" ~> body) ^^ S.Rule

  lazy val label: PackratParser[String] =
    lident

  private lazy val program: PackratParser[S.Program] =
    rawProgram ^^ (_.program)
  private lazy val rawProgram: PackratParser[S.RawProgram] =
    (progElem *) ^^ S.RawProgram
  private lazy val enumElem: PackratParser[S.EnumElem] =
    enumDef ^^ S.EnumElem
  private lazy val requireElem: PackratParser[S.RequireElem] =
    (attr("depends_on") ~ "(" ~ "[") ~> repsep(lident, ",") <~ ("]" ~ ")" ~ ".") ^^ S.RequireElem
  private lazy val compileElem: PackratParser[S.CompileElem] =
    (attr("compile") ~ "(" ~ "[") ~> repsep(lident, ",") <~ ("]" ~ ")" ~ ".") ^^ S.CompileElem
  private def attr(s: String) =
    ("-" ~ (lident ^? { case `s` => "_" })) ^^^ ()

  private lazy val specElem: PackratParser[S.SpecElem] =
    attr("spec") ~> specTp <~ "." ^^ S.SpecElem
  private lazy val langElem: PackratParser[S.LangElem] =
    (attr("lang") ~ "(" ~ "[") ~> repsep(lident, ",") <~ ("]" ~ ")" ~ ".") ^^ S.LangElem
  private lazy val moduleElem: PackratParser[S.ModuleElem] =
    (attr("module") ~ "(") ~> lident <~ (")" ~ ".") ^^ S.ModuleElem
  private lazy val exportElem: PackratParser[S.ExportElem] =
    (attr("export") ~ "(" ~ "[") ~> repsep(arId, ",") <~ ("]" ~ ")" ~ ".") ^^ S.ExportElem
  private lazy val importElem: PackratParser[S.ImportElem] =
    (attr("import") ~ "(") ~> (lident <~ ("," ~ "[")) ~ repsep(localFunName, ",") <~ ("]" ~ ")" ~ ".") ^^
      S.ImportElem
  private lazy val importTypeElem: PackratParser[S.ImportTypeElem] =
    (attr("import_type") ~ "(") ~> (lident <~ ("," ~ "[")) ~ repsep(localFunName, ",") <~ ("]" ~ ")" ~ ".") ^^
      S.ImportTypeElem
  private lazy val exportTypeElem: PackratParser[S.ExportTypeElem] =
    (attr("export_type") ~ "(" ~ "[") ~> repsep(arId, ",") <~ ("]" ~ ")" ~ ".") ^^ S.ExportTypeElem
  private lazy val funElem: PackratParser[S.FunElem] =
    fun ^^ S.FunElem
  private lazy val typeAliasElem: PackratParser[S.TypeAliasElem] =
    pos(attr("type") ~> (lident ~ typeParams ~ ("::" ~> tp <~ ".")) ^^ {
      case n ~ args ~ tp => S.TypeAlias(n, args, tp)(_)
    }) ^^ S.TypeAliasElem
  private lazy val opaqueElem: PackratParser[S.OpaqueElem] =
    pos(attr("opaque") ~> (lident ~ typeParams ~ ("::" ~> tp <~ ".")) ^^ {
      case n ~ args ~ tp => S.Opaque(n, args, tp)(_)
    }) ^^ S.OpaqueElem
  private lazy val progElem: PackratParser[S.ProgramElem] =
    requireElem | specElem | typeAliasElem | enumElem | langElem |
      moduleElem | exportElem | exportTypeElem | opaqueElem | importElem | importTypeElem | funElem | compileElem

  private lazy val arId: PackratParser[(String, Int)] =
    lident ~ "/" ~ numericLit ^^ { case lid ~ _ ~ arity => (lid, arity.toInt) }

  // --- API ---
  def parsePat(s: String): ParseResult[S.Pat] =
    phrase(pat)(new lexical.Scanner(s))
  def parseExp(s: String): ParseResult[S.Exp] =
    phrase(exp)(new lexical.Scanner(s))
  def parseType(s: String): ParseResult[S.Type] =
    phrase(tp)(new lexical.Scanner(s))
  def parseSpecType(s: String): ParseResult[S.Spec] =
    phrase(specTp)(new lexical.Scanner(s))
  def parseEnumDefs(s: String): ParseResult[List[S.EnumDef]] =
    phrase(enumDefs)(new lexical.Scanner(s))
  def parseProgram(s: String): ParseResult[S.Program] =
    phrase(program)(new lexical.Scanner(s))
  def parseRawProgram(s: String): ParseResult[S.RawProgram] =
    phrase(rawProgram)(new lexical.Scanner(s))

  type Res[A] = ParseResult[A]
  def patFromString(input: String): Res[S.Pat] =
    parsePat(input)
  def expFromString(input: String): Res[S.Exp] =
    parseExp(input)
  def enumDefsFromString(input: String): Res[List[S.EnumDef]] =
    parseEnumDefs(input)
  def programFromString(input: String): Res[S.Program] =
    parseProgram(input)
  def rawProgramFromString(input: String): Res[S.RawProgram] =
    parseRawProgram(input)
  def typeFromString(input: String): Res[S.Type] =
    parseType(input)
  def specTypeFromString(input: String): Res[S.Spec] =
    parseSpecType(input)
}
