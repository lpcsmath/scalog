package de.csmath.scalog

import de.csmath.scalog.Types.{Atom, Clause, Query, Var}
import org.scalatest.{FlatSpec, Matchers}

class SLDResolverTest extends FlatSpec with Matchers {

  "An SLDResolver" should "resolve a true Query" in {

    val dbString =
      """
        |istVaterVon(herbert,sabine).
        |istVaterVon(hans,klaus).
        |istVaterVon(hans,klara).
        |geschwister(X,Y) :- istVaterVon(Z,X), istVaterVon(Z,Y).
      """.stripMargin
    val db = parseDb(dbString)

    val queryString =
      """
        |:- geschwister(klaus,W).
      """.stripMargin
    val query = parseQuery(queryString)

    val subs = SLDResolver.resolve(query,3)(db)

    subs should have size 2
    subs.head.mapping should have size 1
    subs.head.mapping.get(Var("W")) shouldBe Some(Atom("klaus"))
    subs.tail.head.mapping should have size 1
    subs.tail.head.mapping.get(Var("W")) shouldBe Some(Atom("klara"))



  }

  val parser = Parser()
  def parseDb(dbString: String): List[Clause] =
    parser.parse(parser.database, dbString).getOrElse(Nil)

  def parseQuery(queryString: String): Query =
    parser.parse(parser.query, queryString).getOrElse(Query(Nil))

}
