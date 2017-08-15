package de.csmath.scalog

object Types {

  sealed trait PrologType

  trait Term extends PrologType
  case class Var(name: String) extends Term with PlList
  case class Atom(name: String) extends Term
  trait Const extends Term
  case class ConstInt(value: Int) extends Const
  case class ConstStr(value: String) extends Const
  case class Struct(functor: Atom, terms: List[Term]) extends Term with AtomicPred


  //Prolog List
  trait PlList extends Term
  case object PlNil extends PlList
  case class PlCons(head: Term, tail: PlList) extends PlList

  //Predicates
  trait Predicate extends PrologType
  trait AtomicPred extends Predicate
  case object True extends AtomicPred
  case object False extends AtomicPred
  case class Clause(head: AtomicPred, body: List[Struct]) extends Predicate



}
