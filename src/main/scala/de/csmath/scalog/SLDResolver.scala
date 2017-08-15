package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

class SLDResolver {

  var renNum = 10

  def resolve(query: Query, numAnswers: Int)(implicit db: List[Clause]): List[Substitution] = {
    val substitutions = resolveAux(query,db,numAnswers)(db,Substitution())
    substitutions.map(_.restrict(varsOfQuery(query)))
  }

  def resolveAux(query: Query, partDb: List[Clause], numAnswers: Int)
                (implicit completeDb: List[Clause], sub: Substitution): List[Substitution] = query match {
    case Query(Nil) => List(sub)
    case Query(_) if partDb.isEmpty => Nil
    case Query(pred :: tail) =>
      val clause = renameVars(partDb.head)
      val unifiedSub = Unifier(pred,clause.head)
      if (unifiedSub.isDefined) {
        val newSub = unifiedSub.get compose sub
        val newQuery = Query(newSub.subPred(clause.body) ++ newSub.subPred(tail))
        val subs = resolveAux(newQuery, completeDb, numAnswers)(completeDb,newSub)
        if (subs.size >= numAnswers)
          subs
        else
          subs ++ resolveAux(query, partDb.tail, subs.size - numAnswers)
      } else {
        resolveAux(query, partDb.tail, numAnswers)
      }
  }

  private def renameVars(clause: Clause): Clause = {
    val vars1 = varsOfTerm(clause.head)
    val vars2 = clause.body.flatMap(varsOfTerm)
    val allVars = vars1.toSet ++ vars2
    val mapping = allVars.map(v => (v, Var(v.name + "_" + renNum))).toMap[Var,Term]
    renNum += 1
    val sub = Substitution(mapping)
    Clause(sub.subPred(clause.head),sub.subPred(clause.body))
  }

  private def varsOfQuery(query: Query): Set[Var] =
    query.predicates.flatMap(_.terms).flatMap(varsOfTerm).toSet

  private def varsOfTerm(term: Term): List[Var] = term match {
    case v@Var(_) => List(v)
    case s@Struct(_,terms) =>
      terms.flatMap(varsOfTerm)
    case PlCons(head,tail) =>
      varsOfTerm(head) ++ varsOfTerm(tail)
    case _ => Nil
  }

}

object SLDResolver {

  def apply(): SLDResolver = new SLDResolver()

}
