package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

object SLDResolver {

  def resolve(query: Query)(implicit db: List[Clause]): (Boolean,Substitution) = {
    val (answer,sub) = resolveAux(query,db)(db,Substitution())
    (answer,sub.restrict(varsOfQuery(query)))
  }

  def resolveAux(query: Query, partDb: List[Clause])(implicit completeDb: List[Clause], sub: Substitution): (Boolean,Substitution) = query match {
    case Query(Nil) => (true, sub)
    case Query(_) if partDb.isEmpty => (false, sub)
    case Query(pred :: tail) =>
      val clause = renameVars(partDb.head)
      val (unified,unifiedSub) = Unifier(pred,clause.head)
      if (unified) {
        val newSub = unifiedSub.get compose sub
        val newQuery = Query(newSub.subPred(clause.body) ++ newSub.subPred(tail))
        resolveAux(newQuery, completeDb)(completeDb,newSub)
      } else {
        resolveAux(query, partDb.tail)
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
