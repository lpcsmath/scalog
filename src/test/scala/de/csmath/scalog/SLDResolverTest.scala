package de.csmath.scalog

import de.csmath.scalog.Types._
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

    val subs = SLDResolver().resolve(query,3)(db)

    subs should have size 2
    subs.head.mapping should have size 1
    subs.head.mapping.get(Var("W")) shouldBe Some(Atom("klaus"))
    subs.tail.head.mapping should have size 1
    subs.tail.head.mapping.get(Var("W")) shouldBe Some(Atom("klara"))

    val db2String =
      """
        |add(X,zero,X).
        |add(X,succ(Y),succ(Z)) :- add(X,Y,Z).
      """.stripMargin
    val db2 = parseDb(db2String)

    val query2String =
      """
        |:- add(succ(succ(zero)),succ(zero),Y).
      """.stripMargin
    val query2 = parseQuery(query2String)

    val subs2 = SLDResolver().resolve(query2,1)(db2)

    subs2 should have size 1
    subs2.head.mapping should have size 1
    subs2.head.mapping.get(Var("Y")) shouldBe Some(parseTerm("succ(succ(succ(zero)))"))

    val query3String =
      """
        |:- add(succ(zero),X,succ(succ(succ(zero)))).
      """.stripMargin
    val query3 = parseQuery(query3String)

    val subs3 = SLDResolver().resolve(query3,1)(db2)

    subs3 should have size 1
    subs3.head.mapping should have size 1
    subs3.head.mapping.get(Var("X")) shouldBe Some(parseTerm("succ(succ(zero))"))


  }

  val parser = Parser()
  def parseDb(dbString: String): List[Clause] =
    parser.parse(parser.database, dbString).getOrElse(Nil)

  def parseQuery(queryString: String): Query =
    parser.parse(parser.query, queryString).getOrElse(Query(Nil))

  def parseTerm(termString: String): Term =
    parser.parse(parser.term, termString).getOrElse(Atom("nothing"))

}
