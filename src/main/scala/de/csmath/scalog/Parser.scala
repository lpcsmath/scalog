package de.csmath.scalog

import de.csmath.scalog.Types._

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  def term = struct | variable | atom | constNumber

  def clause = fact | rule

  def variable = """[A-Z_][A-z\d_]*""".r ^^ {
    case v => Var(v)
  }

  def atom = """([a-z][A-z\d_]*)""".r ^^ {
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

  def dotList: Parser[PlList] = nil | dotCons

  def dotCons = ".(" ~> term ~ "," ~ dotList <~ ")" ^^ {
    case hd ~ "," ~ tail => PlCons(hd,tail)
  }

  def squareList: Parser[PlList] = nil | squareCons

  def squareTail: Parser[PlList] = "|" ~> squareList ^^ {
    case list => list
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
    case q => Clause(False,q)
  }



}
