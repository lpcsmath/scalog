package de.csmath.scalog

import de.csmath.scalog.Types._
import de.csmath.scalog.AstToProlog._
import org.scalatest.{FlatSpec, Matchers}



class ParserTest extends FlatSpec with Matchers {

  val parser = new Parser
  val nothing = Atom("Nothing")


  "A Parser" should "parse variables" in {

    val var1 = "X"
    val var2 = "Var2"
    val var3 = "_"
    val var4 = "_Var4"
    val var5 = "hallo"
    val var6 = "Ha%$o"
    val var7 = "%$"

    parser.parse(parser.term,var1).getOrElse(nothing) shouldBe Var(var1)
    parser.parse(parser.term,var2).getOrElse(nothing) shouldBe Var(var2)
    parser.parse(parser.term,var3).getOrElse(nothing) shouldBe Var(var3)
    parser.parse(parser.term,var4).getOrElse(nothing) shouldBe Var(var4)
    parser.parse(parser.term,var5).getOrElse(nothing) shouldBe Atom(var5)
    parser.parse(parser.term,var6).getOrElse(nothing) shouldBe Var("Ha")
    parser.parse(parser.term,var7).getOrElse(nothing) shouldBe nothing

    transBack(parser.parse(parser.term,var1).getOrElse(nothing)) shouldBe var1

  }

  it should "parse atoms" in {

    val atom1 = "atom1"
    val atom2 = "at_om2"
    val atom3 = "aTom3"

    parser.parse(parser.term,atom1).getOrElse(nothing) shouldBe Atom(atom1)
    parser.parse(parser.term,atom2).getOrElse(nothing) shouldBe Atom(atom2)
    parser.parse(parser.term,atom3).getOrElse(nothing) shouldBe Atom(atom3)

  }

  it should "parse numbers" in {
    val num1 = "123"

    parser.parse(parser.term,num1).getOrElse(nothing) shouldBe ConstInt(123)
    transBack(parser.parse(parser.term,num1).getOrElse(nothing)) shouldBe num1

  }

  it should "parse structures" in {

    val struct1 = "succ(zero)"
    val struct2 = "succ(one,two)"
    val struct3 = "fun(1,X,three)"

    parser.parse(parser.term,struct1).getOrElse(nothing) shouldBe
      Struct(Atom("succ"),List(Atom("zero")))
    parser.parse(parser.term,struct2).getOrElse(nothing) shouldBe
      Struct(Atom("succ"),List(Atom("one"),Atom("two")))
    parser.parse(parser.term,struct3).getOrElse(nothing) shouldBe
      Struct(Atom("fun"),List(ConstInt(1),Var("X"),Atom("three")))

    transBack(parser.parse(parser.term,struct3).getOrElse(nothing)) shouldBe struct3

  }

  it should "parse lists" in {

    val list0 = "[]"
    val expList0 = PlNil
    val list1 = "[1]"
    val expList1 = PlCons(ConstInt(1),PlNil)
    val list2 = "[1,2,3]"
    val expList2 = PlCons(ConstInt(1),PlCons(ConstInt(2),PlCons(ConstInt(3),PlNil)))
    val list3 = "[1|[2,3]]"
    val expList3 = expList2
    val list4 = "[1,2|[3]]"
    val expList4 = expList2

    parser.parse(parser.term,list0).getOrElse(nothing) shouldBe expList0
    parser.parse(parser.term,list1).getOrElse(nothing) shouldBe expList1
    parser.parse(parser.term,list2).getOrElse(nothing) shouldBe expList2
    parser.parse(parser.term,list3).getOrElse(nothing) shouldBe expList3
    parser.parse(parser.term,list4).getOrElse(nothing) shouldBe expList4

    transBack(parser.parse(parser.term,list0).getOrElse(nothing)) shouldBe list0
    transBack(parser.parse(parser.term,list4).getOrElse(nothing)) shouldBe list2

  }

  it should "parse facts" in {
    val fact1 = "fact(one)."
    val expFact1 = Clause(Struct(Atom("fact"),List(Atom("one"))),Nil)
    val fact2 = "fact(struct(1,X,two))."
    val expFact2 = Clause(Struct(Atom("fact"),List(Struct(Atom("struct"),List(ConstInt(1),Var("X"),Atom("two"))))),Nil)

    parser.parse(parser.clause,fact1).getOrElse(nothing) shouldBe expFact1
    parser.parse(parser.clause,fact2).getOrElse(nothing) shouldBe expFact2
    

  }

}
