package de.csmath.scalog

import de.csmath.scalog.Types._
import org.scalatest.{FlatSpec, Matchers}

class SLDResolverTest extends FlatSpec with Matchers {

  "An SLDResolver" should "resolve a simple Query" in {

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

    val subs = SLDResolver().resolve(query, 3)(db)

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

    val subs2 = SLDResolver().resolve(query2, 1)(db2)

    subs2 should have size 1
    subs2.head.mapping should have size 1
    subs2.head.mapping.get(Var("Y")) shouldBe Some(parseTerm("succ(succ(succ(zero)))"))

    val query3String =
      """
        |:- add(succ(zero),X,succ(succ(succ(zero)))).
      """.stripMargin
    val query3 = parseQuery(query3String)

    val subs3 = SLDResolver().resolve(query3, 1)(db2)

    subs3 should have size 1
    subs3.head.mapping should have size 1
    subs3.head.mapping.get(Var("X")) shouldBe Some(parseTerm("succ(succ(zero))"))

  }

  it should "work with lists" in {

    val db3String =
      """
        |append([],X,X).
        |append([W|X],Y,[W|Z]) :- append(X,Y,Z).
      """.stripMargin
    val db3 = parseDb(db3String)

    val query4String =
      """
        |:- append([1,2],[3,4],X).
      """.stripMargin
    val query4 = parseQuery(query4String)

    val subs4 = SLDResolver().resolve(query4,1)(db3)

    subs4 should have size 1
    subs4.head.mapping should have size 1
    subs4.head.mapping.get(Var("X")) shouldBe Some(parseTerm("[1,2,3,4]"))

    val query5String =
      """
        |:- append(X,Y,[1,2,3,4]).
      """.stripMargin

    val subs5 = SLDResolver().resolve(parseQuery(query5String),10)(db3)

    subs5 should have size 5

  }

  it should "generate permutations" in {

    val db3String =
      """
        |permutation([],[]).
        |permutation(L,[E|R]) :- scratch(E,L,L2), permutation(L2,R).
        |scratch(E,[E|R],R).
        |scratch(E,[F|R],[F|Rwo]) :- scratch(E,R,Rwo).
      """.stripMargin
    val db3 = parseDb(db3String)

    val query4String =
      """
        |:- permutation([1,2,3,4],X).
      """.stripMargin
    val query4 = parseQuery(query4String)

    val subs4 = SLDResolver().resolve(query4,30)(db3)

    subs4 should have size 24

  }

  val parser = Parser()
  def parseDb(dbString: String): List[Clause] =
    parser.parseDb(dbString).getOrElse(Nil)

  def parseQuery(queryString: String): Query =
    parser.parseQuery(queryString).getOrElse(Query(Nil))

  def parseTerm(termString: String): Term =
    parser.parseTerm(termString).getOrElse(Atom("nothing"))

}
