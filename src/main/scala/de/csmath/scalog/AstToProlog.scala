package de.csmath.scalog

import de.csmath.scalog.Types._

object AstToProlog {

  def transBack(plType: PrologType): String = plType match {
    case Var(name) => name
    case Atom(name) => name
    case ConstInt(value) => value.toString
    case list: PlList =>
      "[" + elems(list) + "]"
    case Struct(Atom(functor),terms) =>
      functor + terms.map(transBack).mkString("(",",",")")
    case Clause(head,Nil) =>
      transBack(head) + "."
    case Clause(head,body) =>
      transBack(head) + " :- " + body.map(transBack).mkString(",") + "."
    case _ => " not yet implemented "
  }

  private def elems(list: PlList): String = list match {
    case PlNil => ""
    case PlCons(x,PlNil) => transBack(x)
    case PlCons(x,y) => transBack(x) + "," + elems(y)
  }

}
