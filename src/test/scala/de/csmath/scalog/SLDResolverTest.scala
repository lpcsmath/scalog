package de.csmath.scalog

import de.csmath.scalog.Types.{Atom, Clause, Query}
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

    println(db)

    val queryString =
      """
        |:- geschwister(klaus,W).
      """.stripMargin
    val query = parseQuery(queryString)

    println(query)

    println(SLDResolver.resolve(query)(db))


  }

  val parser = Parser()
  def parseDb(dbString: String): List[Clause] =
    parser.parse(parser.database, dbString).getOrElse(Nil)

  def parseQuery(queryString: String): Query =
    parser.parse(parser.query, queryString).getOrElse(Query(Nil))

}
