package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

object Unifier {

  def apply(t1: Term, t2: Term): (Boolean, Option[Substitution]) = t1 match {
    case v@Var(x) if t1 == t2 =>
      (true,Some(Substitution()))
    case v@Var(x) if notOccurs(v,t2) =>
      (true, Some(Substitution(Map(Var(x) -> t2))))
    case x if t2.isInstanceOf[Var] && notOccurs(t2.asInstanceOf[Var],t1) =>
      (true, Some(Substitution(Map(t2.asInstanceOf[Var] -> t1))))
    case _: Const if t1 == t2 =>
      (true, Some(Substitution()))
    case Struct(f1,elems1) =>
      t2 match {
        case Struct(f2,elems2) if f1 == f2 && elems1.size == elems2.size =>
          apply(elems1,elems2)
        case _ =>
          (false, None)
      }
    case _ =>
      (false, None)
  }

  def apply(terms1: List[Term], terms2: List[Term]): (Boolean, Option[Substitution]) = (terms1,terms2) match {
    case (Nil,Nil) => (true, Some(Substitution()))
    case (x :: tail1, y :: tail2) =>
      val (isUnifiable,sub1) = apply(x,y)
      if (isUnifiable) {
        val (allUnifiable, sub2) = apply(tail1.map(sub1.get(_)), tail2.map(sub1.get(_)))
        if (allUnifiable)
          (allUnifiable, Some(sub2.get compose sub1.get))
        else
          (false, None)
      } else
        (false, None)
    case _ =>
      (false, None)
  }

  private def notOccurs(variable: Var, term: Term): Boolean = term match {
    case v@Var(_) => v != variable
    case Struct(_,terms) =>
      terms.forall(notOccurs(variable,_))
    case _ => true
  }

}
