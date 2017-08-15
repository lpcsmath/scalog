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

    val (res1,s1) = Unifier(term1,term2)
    res1 shouldBe true
    s1.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(term2)
    val (res2,s2) = Unifier(term2,term1)
    res2 shouldBe true
    s1.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(term2)
    val (res3,_) = Unifier(term2,term3)
    res3 shouldBe false
    val (res4,_) = Unifier(term3,term4)
    res4 shouldBe false
    val (res5,s5) = Unifier(term4,term5)
    res5 shouldBe true
    s5.get.mapping.get(term1.asInstanceOf[Var]) shouldBe Some(parse("h(b)"))
    val (res6,_) = Unifier(term1,term4)
    res6 shouldBe false
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

    val (res1,s1) = Unifier(pred1,pred2)
    res1 shouldBe true
    s1.get.mapping should have size 3
    s1.get.mapping.get(Var("X")) shouldBe Some(Atom("a"))
    s1.get.mapping.get(Var("Y")) shouldBe Some(Atom("b"))
    s1.get.mapping.get(Var("Z")) shouldBe Some(Atom("c"))
    val (res2,s2) = Unifier(pred1,pred3)
    res2 shouldBe true
    s2.get.mapping should have size 1
    s2.get.mapping.get(Var("Z")) shouldBe Some(Var("X"))
    val (res3,_) = Unifier(pred2,pred3)
    res3 shouldBe false
    val (res4,s4) = Unifier(pred3,pred4)
    res4 shouldBe true
    s4.get.mapping should have size 1
    s4.get.mapping.get(Var("X")) shouldBe Some(ConstInt(5))

  }

  val parser = Parser()
  val nothing = Atom("nothing")

  def parse(plString: String) = parser.parse(parser.prolog, plString).getOrElse(nothing)

}
