package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

object Unifier {

  def apply(t1: Term, t2: Term): Option[Substitution] = (t1,t2) match {
    case (Var(_),_) if t1 == t2 =>
      Some(Substitution())
    case (Var(x),Var(_)) =>
      Some(Substitution(Map(Var(x) -> t2)))
    case (v@Var(x),_) if notOccurs(v,t2) =>
      Some(Substitution(Map(Var(x) -> t2)))
    case (x,v@Var(_)) if notOccurs(v,t1) =>
      Some(Substitution(Map(v -> t1)))
    case (_: Const,_) if t1 == t2 =>
      Some(Substitution())
    case (PlNil,PlNil) =>
      Some(Substitution())
    case (Struct(f1,elems1),Struct(f2,elems2)) if f1 == f2 =>
      apply(elems1,elems2)
    case (PlCons(head1,tail1),PlCons(head2,tail2)) =>
      apply(List(head1,tail1),List(head2,tail2))
    case _ =>
      None
  }

  def apply(terms1: List[Term], terms2: List[Term]): Option[Substitution] = (terms1,terms2) match {
    case (Nil,Nil) => Some(Substitution())
    case (x :: tail1, y :: tail2) =>
      val sub1 = apply(x,y)
      if (sub1.isDefined) {
        val sub2 = apply(tail1.map(sub1.get(_)), tail2.map(sub1.get(_)))
        if (sub2.isDefined)
          Some(sub2.get compose sub1.get)
        else
          None
      } else
        None
    case _ =>
      None
  }

  private def notOccurs(variable: Var, term: Term): Boolean = term match {
    case v@Var(_) => v != variable
    case Struct(_,terms) =>
      terms.forall(notOccurs(variable,_))
    case _ => true
  }

}
