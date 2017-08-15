package de.csmath.scalog.substitution

import de.csmath.scalog.Types._
import de.csmath.scalog.AstToProlog._

import scala.collection.immutable.HashMap

trait Substitution {

  def apply(term: Term): Term = term match {
    case v@Var(x) if mapping.contains(v) =>
      mapping(v)
    case s@Struct(functor,terms) =>
      Struct(functor,terms.map(apply))
    case PlCons(head,tail) =>
      PlCons(apply(head),subList(tail))
    case y => y
  }

  def apply(terms: List[Term]): List[Term] = terms map apply

  def subPred(predicate: Struct): Struct = apply(predicate).asInstanceOf[Struct]
  def subPred(predicates: List[Struct]): List[Struct] = predicates map subPred

  def subList(list: PlList): PlList = apply(list).asInstanceOf[PlList]

  def compose(other: Substitution): Substitution

  def restrict(vars: Set[Var]): Substitution

  def mapping: Map[Var,Term]

  override def toString: String = {
    mapping.toList.map { case (k,v) => k.name + " <- " + transBack(v)}.mkString("{ ",", "," }")
  }

}

object Substitution {

  def apply(): Substitution = new HashMapSubstitution

  def apply(aMap: Map[Var,Term]): Substitution = new HashMapSubstitution(HashMap.empty ++ aMap)

}
