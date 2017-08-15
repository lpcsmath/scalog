package de.csmath.scalog

import de.csmath.scalog.Types._
import org.scalatest.{FlatSpec, Matchers}

class UnifierTest extends FlatSpec with Matchers {

  "A Unifier" should "unify terms" in {

    val plTerm1 = "X"
    val term1 = parse(plTerm1).asInstanceOf[Term]
    val plTerm2 = "1"
    val term2 = parse(plTerm2).asInstanceOf[Term]
    val plTerm3 = "3"
    val term3 = parse(plTerm3).asInstanceOf[Term]
    val plTerm4 = "f(X,g(a))"
    val term4 = parse(plTerm4).asInstanceOf[Term]
    val plTerm5 = "f(h(b),g(a))"
    val term5 = parse(plTerm5).asInstanceOf[Term]

    val s1 = Unifier(term1,term2)
    s1 shouldBe defined
    s1.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(term2)
    val s2 = Unifier(term2,term1)
    s2 shouldBe defined
    s2.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(term2)
    val s3 = Unifier(term2,term3)
    s3 shouldBe empty
    val s4 = Unifier(term3,term4)
    s4 shouldBe empty
    val s5 = Unifier(term4,term5)
    s5 shouldBe defined
    s5.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(parse("h(b)"))
    val s6 = Unifier(term1,term4)
    s6 shouldBe empty
  }

  it should "unify predicates" in {
    val plPred1 = "abc(X,Y,Z)"
    val pred1 = parse(plPred1).asInstanceOf[Term]
    val plPred2 = "abc(a,b,c)"
    val pred2 = parse(plPred2).asInstanceOf[Term]
    val plPred3 = "abc(X,Y,X)"
    val pred3 = parse(plPred3).asInstanceOf[Term]
    val plPred4 = "abc(5,Y,X)"
    val pred4 = parse(plPred4).asInstanceOf[Term]

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
  val nothing = Atom("nothing")

  def parse(plString: String) = parser.parse(parser.prolog, plString).getOrElse(nothing)

}
