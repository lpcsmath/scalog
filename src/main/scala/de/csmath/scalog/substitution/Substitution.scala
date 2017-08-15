package de.csmath.scalog.substitution

import de.csmath.scalog.Types.{Struct, Term, Var}
import de.csmath.scalog.AstToProlog._

import scala.collection.immutable.HashMap

trait Substitution {

  def apply(term: Term): Term = term match {
    case v@Var(x) if mapping.contains(v) =>
      mapping(v)
    case s@Struct(functor,terms) =>
      Struct(functor,terms.map(apply))
    case y => y
  }

  def compose(other: Substitution): Substitution

  def restrict(vars: Set[Var]): Substitution

  def mapping: Map[Var,Term]

  override def toString: String = {
    mapping.toList.map { case (k,v) => k.name + " <- " + transBack(v)}.mkString("{ ",", "," }")
  }

}

object Substitution {

  def apply() = new HashMapSubstitution

  def apply(aMap: Map[Var,Term]) = new HashMapSubstitution(HashMap.empty ++ aMap)

}
