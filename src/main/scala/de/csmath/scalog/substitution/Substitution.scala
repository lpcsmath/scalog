package de.csmath.scalog.substitution

import de.csmath.scalog.Types.{Term, Var}
import de.csmath.scalog.AstToProlog._

trait Substitution {

  def compose(other: Substitution): Substitution

  def restrict(vars: Set[Var]): Substitution

  def mapping: Map[Var,Term]

  override def toString: String = {
    mapping.toList.map { case (k,v) => k.name + " <- " + transBack(v)}.mkString("{ ",", "," }")
  }

}

object Substitution {

  def apply() = new HashMapSubstitution

  def apply(aMap: Map[Var,Term]) = new HashMapSubstitution(aMap)

}
