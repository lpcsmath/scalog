package de.csmath.scalog.substitution

import de.csmath.scalog.Types.{Atom, ConstInt, Struct, Var}
import org.scalatest.{FlatSpec, Matchers}

class SubstitutionTest extends FlatSpec with Matchers {

  "A Substitution" should "be composable" in {

    val x = Var("X")
    val y = Var("Y")
    val w = Var("W")
    val z = Var("Z")
    val term1 = Struct(Atom("one"),List(Atom("two")))
    val term2 = Atom("three")
    val term3 = Struct(Atom("four"),List(Atom("five")))
    val term4 = Atom("six")

    val s1 = Substitution(Map( x -> y,
                               z -> term1,
                               w -> term2 ))
    val s2 = Substitution(Map( y -> term3,
                               w -> term4 ))

    val s3 = s2 compose s1

    s3.mapping should have size 4
    s3.mapping.get(x) shouldBe Some(term3)
    s3.mapping.get(y) shouldBe Some(term3)
    s3.mapping.get(w) shouldBe Some(term2)
    s3.mapping.get(z) shouldBe Some(term1)
  }

  it should "have a proper string representation" in {
    val x = Var("X")
    val y = Var("Y")
    val w = Var("W")
    val z = Var("Z")
    val term1 = Struct(Atom("one"),List(Atom("two")))
    val term2 = Atom("three")

    val s1 = Substitution(Map( x -> y, z -> term1, w -> term2 ))

    s1.toString shouldBe "{ W <- three, X <- Y, Z <- one(two) }"
  }

  it should "be applicable on a term" in {
    val term1 = Var("X")
    val term2 = Atom("atom")
    val term3 = ConstInt(3)
    val term4 = Struct(term2,List(term1,term2))
    val resTerm4 = Struct(term2,List(term3,term2))

    val s1 = Substitution(Map(Var("Y") -> term3))
    val s2 = Substitution(Map(Var("X") -> term3))

    s1(term1) shouldBe term1
    s1(term2) shouldBe term2
    s1(term3) shouldBe term3
    s1(term4) shouldBe term4

    s2(term1) shouldBe term3
    s2(term2) shouldBe term2
    s2(term3) shouldBe term3
    s2(term4) shouldBe resTerm4
  }

}
