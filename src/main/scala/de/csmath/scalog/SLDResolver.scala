package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

object SLDResolver {

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

  private def renameVars(clause: Clause): Clause = clause //TODO rename vars

  private def varsOfQuery(query: Query): Set[Var] =
    query.predicates.flatMap(_.terms).flatMap(varsOfStruct).toSet

  private def varsOfStruct(term: Term): List[Var] = term match {
    case v@Var(_) => List(v)
    case s@Struct(_,terms) => terms.flatMap(varsOfStruct)
    case _ => Nil
  }

}
