package de.csmath.scalog

import de.csmath.scalog.Types._

object AstToProlog {

  def transBack(plType: PrologType): String = plType match {
    case Var(name) => name
    case Atom(name) => name
    case ConstInt(value) => value.toString
    case PlNil => "[]"
    case PlCons(head, tail) =>
      ".(" + transBack(head) + "," + transBack(tail) + ")"
    case Struct(Atom(functor),terms) =>
      functor + terms.map(transBack).mkString("(",",",")")
    case _ => " not yet implemented "
  }

}
