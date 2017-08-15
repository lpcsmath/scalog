package de.csmath.scalog

import de.csmath.scalog.Types._

import scala.util.Try
import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def prolog = clause | term

  def term = struct | variable | atom | constNumber

  def clause = fact | rule

  def database = rep(clause)

  def variable = """[A-Z_]\w*""".r ^^ {
    case v => Var(v)
  }

  def atom = """([a-z]\w*)""".r ^^ {
    case a => Atom(a)
  }

//  def constString = """"(.*)"""".r ^^ {
//    case s => ConstStr(s)
//  }

  def constNumber = """(\d+)""".r ^^ {
    case num => ConstInt(num.toInt)
  }

  def struct = dotList | squareList | structure

  def structure: Parser[Struct] = atom ~ "(" ~ repsep(term,",") <~ ")" ^^ {
    case functor ~ "(" ~ list => Struct(functor, list)
  }


  def nil = "[]" ^^ {
    case emptyList => PlNil
  }

  def dotList: Parser[PlList] = nil | dotCons | variable

  def dotCons = ".(" ~> term ~ "," ~ dotList <~ ")" ^^ {
    case hd ~ "," ~ tail => PlCons(hd,tail)
  }

  def squareList: Parser[PlList] = nil | squareCons | variable

  def squareTail: Parser[PlList] = "|" ~> squareList ^^ {
    case PlNil => PlNil
    case list@PlCons(_,_) => list
    case aVar@Var(_) => aVar
  }

  def squareCons = "[" ~> repsep(term,",") ~ opt(squareTail) <~ "]" ^^ {
    case heads ~ tail => consList(heads,tail.getOrElse(PlNil))
  }

  private def consList(heads: List[Term], plTail: PlList): PlList = heads match {
    case Nil => plTail
    case hd :: tail => PlCons(hd,consList(tail,plTail))
  }

  def fact = structure ~ "." ^^ {
    case f ~ "."  => Clause(f, Nil)
  }

  def rule = structure ~ ":-" ~ repsep(structure,",") <~ "." ^^ {
    case head ~ ":-" ~ body => Clause(head,body)
  }

  def query = ":-" ~> repsep(structure,",") <~ "." ^^ {
    case q => Query(q)
  }

  def parseDb(dbString: String): Try[List[Clause]] =
    Try(parse(database, dbString).get)

  def parseQuery(queryString: String): Try[Query] =
    Try(parse(query, queryString).get)

  def parseTerm(termString: String): Try[Term] =
    Try(parse(term, termString).get)

}

object Parser {

  def apply() = new Parser

}
