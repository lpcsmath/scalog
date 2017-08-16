package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.substitution.Substitution

class SLDResolver {

  type State = (Query,List[Clause],Substitution)

  var renNum = 10

  var stack = List.empty[State]

  var prog: List[Clause] = _

  var answerVars: Set[Var] = _

  def resolve(query: Query, numAnswers: Int = -1)(implicit db: List[Clause]): List[Substitution] = {
    prog = db
    val substitutions = resolveAux(query,db,numAnswers)(db,Substitution())
    answerVars = varsOfQuery(query)
    substitutions.map(_.restrict(answerVars))
  }

  private def resolveAux(query: Query, partDb: List[Clause], numAnswers: Int)
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
        if (numAnswers < 0) {
          // backtracking
          stack = (query,partDb.tail,sub) :: stack
          subs
        } else if (subs.size >= numAnswers)
          subs
        else
          subs ++ resolveAux(query, partDb.tail, numAnswers - subs.size)
      } else {
        resolveAux(query, partDb.tail, numAnswers)
      }
  }

  def backTrack(): List[Substitution] = stack match {
    case Nil => Nil
    case (query,partDb,sub) :: tail =>
      stack = tail
      val subs = resolveAux(query,partDb,-1)(prog,sub)
      if (subs.isEmpty) backTrack()
      else subs.map(_.restrict(answerVars))
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
