package de.csmath.scalog

object Types {

  sealed trait PrologType

  trait Term extends PrologType
  case class Var(name: String) extends Term with PlList
  trait Const extends Term
  case class Atom(name: String) extends Const
  case class ConstInt(value: Int) extends Const
  case class ConstStr(value: String) extends Const
  case class Struct(functor: Atom, terms: List[Term]) extends Term


  //Prolog List
  trait PlList extends Term
  case object PlNil extends PlList
  case class PlCons(head: Term, tail: PlList) extends PlList

  //Predicates
  trait Predicate extends PrologType
  case class Clause(head: Struct, body: List[Struct]) extends Predicate
  case class Query(predicates: List[Struct]) extends Predicate



}
