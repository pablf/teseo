package teseo

import teseo.*
import teseo.internal.Error
import org.scalatest.funsuite.*
import teseo.Doc.Paragraph

class TeseoSpec extends AnyFunSuite {

  trait MockImpl[C <: Conf]() extends Algm[Int, String]

  val s2: AlgmImpl.Default[Int, String, Int] =
    AlgmImpl.Default[Int, String, Int](add => (x => (x + add).toString()), 7)

  given mock: MockImpl[Conf.Default[Int]] with {
    type CC = Conf.Default[Int]
    override val alg: AlgmImpl[Int, String, CC] = s2
  }

  def toMock[C <: Conf](algm: Algm[Int, String] { type CC = C }): MockImpl[C] = new MockImpl[C] {
    override val alg = algm.alg.asInstanceOf[AlgmImpl[Int, String, CC]]
  }

  trait MockImpl2[C <: Conf]() extends Algm[Int, String]

  trait MockImpl3() extends Algm[Int, String] { type CC = Conf.Default[Double] }

  test("Summoning Conf.Wrap") {
    import teseo.given
    given Int = 3

    val confImpl = summon[Conf.Wrap[_, MockImpl]]
    assert(confImpl == summon[Conf.Wrap[Conf.Default[Int], MockImpl]])

    val a = Algm[Int, Int, Int](y => (x: Int) => x + 1)
    val b = Algm[Int, Int, Int](y => x => x + 2)

    // assert((Algm.andThen(a, b) ex 3) == 6)

  }

  test("Summoning Conf") {
    import teseo.given

    implicit val num: Int = 3

    val confBase  = summon[Conf.Default[Int]]
    val confBase2 = summon[Conf.Default[String]]

    val confAnd     = summon[Conf.And[Conf.Default[Int], Conf.Default[String]]]
    val confThen    = summon[Conf.Then[Conf.Default[Int], Conf.Default[String]]]
    val confOr1     = summon[Conf.Or[Conf.Default[Int], Conf.Default[String]]]
    val confOr2     = summon[Conf.Or[Conf.Default[String], Conf.Default[Int]]]
    val confOr3     = summon[Conf.Or[Conf.Default[Double], Conf.Default[String]]]
    val confComplex =
      summon[Conf.And[Conf.Or[Conf.Default[Int], Conf.Default[String]], Conf.Default[Int]]]

    assert(confBase == Conf.Default(Some(3)))
    assert(confBase2 == Conf.Default(None))
    assert(confAnd == Conf.And(confBase, confBase2))
    assert(confThen == Conf.Then(confBase, confBase2))
    assert(confOr1 == Conf.Or(Left(confBase)))
    assert(confOr2 == Conf.Or(Right(confBase)))
    assert(confOr3 == Conf.Or(Left(Conf.Default(None))))
    assert(confComplex == Conf.And(confOr1, confBase))
  }

  test("AlgmImpl constructors") {

    given Int   = 3
    val applied = AlgmImpl.default[Int, Int, Int](default => (x => x + default))
    val const   = AlgmImpl.const[Int, Int](x => x)

    assert(applied.ex(3) == 6)
    assert(const.ex(3) == 3)

    assertDoesNotCompile("AlgmImpl[Double, Double, Double].default(default => (x => x+default))")
  }

  test("AlgmImpl.wrap constructors") {
    /*
    val mock1 = AlgmImpl.wrap[Int, String, MockImpl]()
    val mock2 = AlgmImpl.wrap[Int, String, MockImpl2]()
    val mock3 = AlgmImpl.wrap[Int, String, Conf.Default[Double], MockImpl3]()

    assert(mock1.ex(3) == "10")
    assertDoesNotCompile("mock2.ex(3)")
    assertDoesNotCompile("mock3.ex(3)")
    {
      given Double = 3.3

      given MockImpl3 with
        val alg = AlgmImpl(double => (int => int.toString() ++ double.toString()))

      assert(mock3.ex(3) == "33.3")

    }
    assertDoesNotCompile("AlgmImpl.wrap[Int, String, Conf.Default[Int], MockImpl3]()")*/

  }

  test("Default overriden inside AlgmImpl.Wrap") {

    val mock = AlgmImpl.wrap[Int, String, MockImpl]()

    assert(mock.ex(3) == "10")

    {
      given Int = 0

      assert(mock.ex(3) == "3")
    }

  }

  test("New given overrides default in AlgmImpl.Default") {
    given Int = 3

    val applied = AlgmImpl.default[Int, Int, Int](default => (x => x + default))

    assert(applied.ex(3) == 6)

    {

      given Int = 4

      assert(applied.ex(3) == 7)

    }

  }

  test("AlgmImpl.Simple requires given") {

    val applied = AlgmImpl.Simple[Int, Int, Int](default => (x => x + default))

    assertDoesNotCompile("applied.ex(3)")

    {

      given Int = 4

      assert(applied.ex(3) == 7)

    }

  }

  test("AlgmImpl automatic execution") {
    import teseo.given

    case class Adder1(n: Int)
    case class Adder2(n: Int)

    given add: Adder1 = Adder1(3)
    // given add2: Adder2 = Adder2(2)

    val s: AlgmImpl.Default[Int, Int, Adder1]  =
      AlgmImpl.Default[Int, Int, Adder1](adder => (x => x + adder.n), Adder1(0))
    val s2: AlgmImpl.Default[Int, Int, Adder2] =
      AlgmImpl.Default[Int, Int, Adder2](adder => (x => x + adder.n), Adder2(7))
    val and                                    = s zip s2
    val or1                                    = s | s2
    val or2                                    = s2 | s
    val andThen                                = s >>= s2

    val adder1 = summon[Conf.Simple[Adder1]]
    val adder2 = summon[Conf.Default[Adder2]]

    assert(
      List(
        s.ex(1000),
        s2.ex(1000),
        and.ex(1000),
        or1.ex(1000),
        or2.ex(1000),
        andThen.ex(1000)
      ) == List(1003, 1007, (1003, 1007), 1003, 1003, 1010)
    )
  }

  test("AlgmImpl.Or") {
    import teseo.given
    val a   = AlgmImpl.Wrap[Int, String, MockImpl]()
    val b   = AlgmImpl.Wrap[Int, String, MockImpl2]()
    val or1 = Algm.mk(a | b)
    val or2 = Algm.mk(b | a)

    assert(Algm.mk(a).ex(10) == "17")
    assertDoesNotCompile("Algm(b).ex(10)")
    assert(or1.ex(10) == "17")
    assert(or2.ex(10) == "17")
  }

  test("AlgmImpl.And") {
    import teseo.given
    val a       = AlgmImpl.Wrap[Int, String, MockImpl]()
    val b       = AlgmImpl.Wrap[Int, String, MockImpl2]()
    val badAnd1 = Algm.mk(a ++ b)
    val badAnd2 = Algm.mk(b ++ a)
    val goodAnd = Algm.mk(a ++ AlgmImpl.const[Double, Double](x => x * 1.3))

    assert(goodAnd.ex((10, 10.7)) == ("17", 13.91))
    assertDoesNotCompile("badAnd1.ex(10)")
    assertDoesNotCompile("adAnd2.ex(10)")
  }

  test("AlgmImpl.Wrap only wraps Algms") {
    assertDoesNotCompile("AlgmImpl.Wrap[Int, String, Int]()")
    assertCompiles("AlgmImpl.Wrap[Int, String, MockImpl]()")
  }

  test("AlgmImpl.Wrap automatic execution") {
    import teseo.given
    given Int = 3

    trait MockImpl[C <: Conf]() extends Algm[Int, String]

    val s2: AlgmImpl.Default[Int, String, Int] =
      AlgmImpl.Default[Int, String, Int](add => (x => (x + add).toString()), 7)

    given mock: MockImpl[Conf.Default[Int]] with {
      type CC = Conf.Default[Int]
      override val alg: AlgmImpl[Int, String, CC] = s2
    }

    def toMock[C <: Conf](algm: Algm[Int, String] { type CC = C }): MockImpl[C] = new MockImpl[C] {
      override val alg = algm.alg.asInstanceOf[AlgmImpl[Int, String, CC]]
    }

    val alg = AlgmImpl.Wrap[Int, String, MockImpl]()
    assert(alg.ex(10) == "13")
  }

  test("Container preserves type") {

    trait I[A <: Conf]  extends Algm[Int, Float] { type CC = A }
    trait B[A <: Conf]  extends Algm[Float, Int] { type CC = A }
    trait IB[A <: Conf] extends Algm[Int, Int]   { type CC = A }

    // Creating companion object to link IB = I + B.
    object IB {

      // This case class is needed as a template.
      case class IBImpl[A <: Conf](alg: AlgmImpl[Int, Int, A]) extends IB[A]

      // This can be used anywhere
      val o = AlgmGen.pipeline[IB, I, B]
      export o.alg
    }

    import IB.given
    val ib = summon[IB[_]]

    // Instances of `I` and `B`.
    given I[Conf.Simple[String]] with
      override val alg: AlgmImpl[Int, Float, Conf.Simple[String]] =
        AlgmImpl.Simple(_ => (x => (x + 1.3).toFloat))

    given B[Conf.Simple[String]] with
      override val alg: AlgmImpl[Float, Int, Conf.Simple[String]] =
        AlgmImpl.Simple(_ => (x => x.toInt))

    given String = "o"

    assert(ib.ex(3) == 4)

  }

  test("Doc") {
    val doc = Doc()

    doc.addParagraph("Paragraph1", List("This is a line", "This another one"))
    assert(
      doc.toString() == "Paragraph1" ++ "\n" ++ "      " ++ "This is a line" ++ "\n" ++ "      " ++ "This another one"
    )

    doc.addParagraph("Paragraph2", List("This is a line 2", "This another one 2"))
    assert(
      doc.toString() ==
        "Paragraph1" ++ "\n" ++ "      " ++ "This is a line" ++ "\n" ++ "      " ++ "This another one" ++ "\n" ++
        "Paragraph2" ++ "\n" ++ "      " ++ "This is a line 2" ++ "\n" ++ "      " ++ "This another one 2"
    )

    doc.addLine("This the third in 2")
    assert(
      doc.toString() ==
        "Paragraph1" ++ "\n" ++ "      " ++ "This is a line" ++ "\n" ++ "      " ++ "This another one" ++ "\n" ++
        "Paragraph2" ++ "\n" ++ "      " ++ "This is a line 2" ++ "\n" ++ "      " ++ "This another one 2" ++ "\n" ++ "      " ++ "This the third in 2"
    )
    doc.addLineIn("NonParagraph", "?")
    assert(
      doc.toString() ==
        "Paragraph1" ++ "\n" ++ "      " ++ "This is a line" ++ "\n" ++ "      " ++ "This another one" ++ "\n" ++
        "Paragraph2" ++ "\n" ++ "      " ++ "This is a line 2" ++ "\n" ++ "      " ++ "This another one 2" ++ "\n" ++ "      " ++ "This the third in 2"
    )

    doc.addLineIn("Now it is added", "Paragraph1")
    assert(
      doc.toString() ==
        "Paragraph1" ++ "\n" ++ "      " ++ "This is a line" ++ "\n" ++ "      " ++ "This another one" ++ "\n" ++ "      " ++ "Now it is added" ++ "\n" ++
        "Paragraph2" ++ "\n" ++ "      " ++ "This is a line 2" ++ "\n" ++ "      " ++ "This another one 2" ++ "\n" ++ "      " ++ "This the third in 2"
    )

    val doc2 = Doc()

    doc2.addParagraph("name1", Nil)
    doc2.addParagraph("name2", Nil)
    doc2.addParagraph("name1", Nil)
    doc2.addLineIn("add to all name1", "name1", false)
    assert(
      doc2.toString() ==
        "name1" ++ "\n" ++ "      " ++ "add to all name1" ++ "\n" ++
        "name2" ++ "\n" ++
        "name1" ++ "\n" ++ "      " ++ "add to all name1"
    )

    doc2.map { case par =>
      par.content = Nil
    }
    assert(doc2.toString() == "name1" ++ "\n" ++ "name2" ++ "\n" ++ "name1")

    doc2.addLineIn("something", "name1")

    assert(doc2.search("name3") == None)
    assert(doc2.search("name1") == Some(Paragraph(Some("name1"), "something" :: Nil)))
    assert(doc2.searchAllOcurrences("name3") == Nil)
    assert(
      doc2.searchAllOcurrences("name1") == List(
        Paragraph(Some("name1"), "something" :: Nil),
        Paragraph(Some("name1"), Nil)
      )
    )

  }

  test("AlgmG") {
    /*sealed trait Number[A] {
      def op(a: Number[A]): Number[A]
    }

    case class N(a: String) extends Number[String] {
      override def op(other: Number[String]): N =
        other match {
          case N(v) => N(a ++ v)
        }
    }

    trait NumberOps[A] {
      def transform(s: String): Number[A]
    }

    given String = "format: "

    val impl: [A] => NumberOps[A] => AlgmImpl[Number[A], Number[A], Conf.Default[String]] =
      [A] => (ops: NumberOps[A]) => AlgmImpl((formatter: String) => ((a: Number[A]) => ops.transform(formatter) op a))

    val alg = AlgmG[Any, NumberOps, Number, Number, Conf.Default[String]](impl)

    assertDoesNotCompile("""alg.ex[N](N("oh"))""")

    {
      given NumberOps[String] with
        override def transform(s: String) = N(s)

      assert(alg.ex[String](N("oh")) == N("format: oh"))

    }*/

    // TODO test extensively
  }

}
