package de.csmath.scalog

import de.csmath.scalog.Types._
import org.scalatest.{FlatSpec, Matchers}

class UnifierTest extends FlatSpec with Matchers {

  "A Unifier" should "unify terms" in {

    val plTerm1 = "X"
    val term1 = parseVar(plTerm1)
    val plTerm2 = "1"
    val term2 = parseTerm(plTerm2)
    val plTerm3 = "3"
    val term3 = parseTerm(plTerm3)
    val plTerm4 = "f(X,g(a))"
    val term4 = parseTerm(plTerm4)
    val plTerm5 = "f(h(b),g(a))"
    val term5 = parseTerm(plTerm5)

    val s1 = Unifier(term1,term2)
    s1 shouldBe defined
    s1.get.mapping.get(term1) shouldBe Some(term2)
    val s2 = Unifier(term2,term1)
    s2 shouldBe defined
    s2.get.mapping.get(term1) shouldBe Some(term2)
    val s3 = Unifier(term2,term3)
    s3 shouldBe empty
    val s4 = Unifier(term3,term4)
    s4 shouldBe empty
    val s5 = Unifier(term4,term5)
    s5 shouldBe defined
    s5.get.mapping.get(term1) shouldBe Some(parseTerm("h(b)"))
    val s6 = Unifier(term1,term4)
    s6 shouldBe empty
  }

  it should "unify lists" in {
    val plTerm1 = "X"
    val term1 = parseVar(plTerm1)
    val plTerm2 = "Y"
    val term2 = parseVar(plTerm2)
    val plTerm3 = "[]"
    val term3 = parseTerm(plTerm3)
    val plTerm4 = "[1,X|Y]"
    val term4 = parseTerm(plTerm4)
    val plTerm5 = "[1,2,3,4,5]"
    val term5 = parseTerm(plTerm5)

    val s1 = Unifier(term1,term3)
    s1 shouldBe defined
    s1.get.mapping should have size 1
    s1.get.mapping.get(term1) shouldBe Some(term3)

    val s2 = Unifier(term4,term5)
    s2 shouldBe defined
    s2.get.mapping should have size 2
    s2.get.mapping.get(term1) shouldBe Some(parseTerm("2"))
    s2.get.mapping.get(term2) shouldBe Some(parseTerm("[3,4,5]"))

  }

  it should "unify predicates" in {
    val plPred1 = "abc(X,Y,Z)"
    val pred1 = parseTerm(plPred1)
    val plPred2 = "abc(a,b,c)"
    val pred2 = parseTerm(plPred2)
    val plPred3 = "abc(X,Y,X)"
    val pred3 = parseTerm(plPred3)
    val plPred4 = "abc(5,Y,X)"
    val pred4 = parseTerm(plPred4)

    val s1 = Unifier(pred1,pred2)
    s1 shouldBe defined
    s1.get.mapping should have size 3
    s1.get.mapping.get(Var("X")) shouldBe Some(Atom("a"))
    s1.get.mapping.get(Var("Y")) shouldBe Some(Atom("b"))
    s1.get.mapping.get(Var("Z")) shouldBe Some(Atom("c"))
    val s2 = Unifier(pred1,pred3)
    s2 shouldBe defined
    s2.get.mapping should have size 1
    s2.get.mapping.get(Var("Z")) shouldBe Some(Var("X"))
    val s3 = Unifier(pred2,pred3)
    s3 shouldBe empty
    val s4 = Unifier(pred3,pred4)
    s4 shouldBe defined
    s4.get.mapping should have size 1
    s4.get.mapping.get(Var("X")) shouldBe Some(ConstInt(5))

  }

  val parser = Parser()
  val nothing = Atom("Nothing")
  val notVar = Var("var")

  def parseTerm(termString: String): Term =
    parser.parse(parser.term, termString).getOrElse(nothing)
  def parseVar(varString: String): Var =
    parser.parse(parser.variable, varString).getOrElse(notVar)

}
