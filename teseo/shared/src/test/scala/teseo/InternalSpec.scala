package teseo

import org.scalatest.funsuite.*
import teseo.internal.*

class InternalSpec extends AnyFunSuite {

  trait MockImpl[C <: Conf]() extends Algm[Int, String]

  val s2: AlgmImpl.Default[Int, String, Int] =
    AlgmImpl.Default[Int, String, Int](add => (x => (x + add).toString()), 7)

  given mock: MockImpl[Conf.Default[Int]] with {
    type CC = Conf.Default[Int]
    override val alg: AlgmImpl[Int, String, CC] = s2
  }

  trait MockImpl2[C <: Conf]() extends Algm[Int, String]

  /**
   * Testing implicits.
   */
  test("Summoning Optional") {
    import teseo.given

    implicit val num: Int = 3

    val optionalFromInt    = summon[Optional[Int]]
    val optionalFromString = summon[Optional[String]]

    assert(optionalFromInt == Optional.Found(3))
    assert(optionalFromString == Optional.NotFound())
  }

  test("Error.or") {

    val errorOr1 = summon[Error[Conf.Or[Conf.Default[Int], Conf.Default[String]]]]
    val errorOr2 = summon[Error[Conf.Or[Conf.Default[String], Conf.Default[Int]]]]
    val errorOr3 = summon[Error[Conf.Or[Conf.Simple[Long], Conf.Default[String]]]]
    val errorOr4 = summon[Error[Conf.Or[Conf.Simple[Long], Conf.Simple[String]]]]
    val errorOr5 = summon[Error[Conf.Or[Conf.Simple[Long], Conf.Simple[Long]]]]
    val errorOr6 =
      summon[Error[Conf.Or[Conf.Or[Conf.Simple[Long], Conf.Simple[Double]], Conf.Simple[String]]]]
    val errorOr7 =
      summon[Error[Conf.Or[Conf.Or[Conf.Simple[Long], Conf.Simple[Double]], Conf.Simple[String]]]]
    assert(errorOr1 == Error.empty)
    assert(errorOr2 == Error.empty)
    assert(errorOr3 == Error.empty)
    assert(errorOr4 == Error(Nil, List(List("scala.Long"), List("java.lang.String")), Nil))
    assert(errorOr5 == Error(List("scala.Long"), Nil, Nil))
    assert(
      errorOr6 == Error(
        Nil,
        List(List("scala.Long"), List("scala.Double"), List("java.lang.String")),
        Nil
      )
    )

  }

  test("Summoning Error") {
    import teseo.given

    implicit val num: Int = 3

    val errorBase     = summon[Error[Conf.Simple[Long]]]
    val errorBase1    = summon[Error[Conf.Default[Int]]]
    val errorBase2    = summon[Error[Conf.Default[String]]]
    val errorWrapmpl1 = summon[Error[Conf.Wrap[_, MockImpl]]]
    val errorWrapmpl2 = summon[Error[Conf.Wrap[_, MockImpl2]]]
    val errorAnd      = summon[Error[Conf.And[Conf.Default[Int], Conf.Default[String]]]]
    val errorThen     = summon[Error[Conf.Then[Conf.Default[Int], Conf.Default[String]]]]

    val errorComplex1 =
      summon[Error[Conf.And[Conf.Or[Conf.Default[Int], Conf.Default[String]], Conf.Default[Int]]]]
    val errorComplex2 = summon[Error[
      Conf.And[Conf.Or[Conf.Wrap[_, MockImpl], Conf.Simple[String]], Conf.Simple[Double]]
    ]]
    val errorComplex3 = summon[Error[
      Conf.And[Conf.Or[Conf.Wrap[_, MockImpl2], Conf.Simple[String]], Conf.Simple[Double]]
    ]]

    assert(errorBase == Error(List("scala.Long"), Nil, Nil))
    assert(errorBase1 == Error.empty)
    assert(errorBase2 == Error.empty)
    assert(errorWrapmpl1 == Error.empty)
    assert(errorWrapmpl2 == Error(List("teseo.InternalSpec.MockImpl2"), Nil, Nil))
    assert(errorAnd == Error.empty)
    assert(errorThen == Error.empty)

    assert(errorComplex1 == Error.empty)
    assert(errorComplex2 == Error(List("scala.Double"), Nil, Nil))
    assert(
      errorComplex3 == Error(
        List("scala.Double"),
        List(List("teseo.InternalSpec.MockImpl2"), List("java.lang.String")),
        Nil
      )
    )
  }

  test("ErrorMacro macros") {
    assertDoesNotCompile("Error.triggerError(Error.empty)")

    inline val a = Error.compare(Error(List("a"), Nil, Nil))

    /*assertDoesNotCompile("""Error.compare(Error(List("a"), Nil, Nil))""")
        assertDoesNotCompile("""Error.compare(Error(Nil, Nil, List("a")))""")
        assertDoesNotCompile("""Error.compare(Error(Nil, List(List("a")), Nil))""")*/

    assert(Error.error[Int] == Error(List("scala.Int"), Nil, Nil))
    assert(Error.errorWrap[MockImpl] == Error(List("teseo.InternalSpec.MockImpl"), Nil, Nil))
    // TODO
    // property-test for and, or, andThen
  }

  test("Error empty") {
    assert(Error.empty == Error(Nil, Nil, Nil))
  }

  test("Explain Macro with custom Algm") {
    // TODO

  }

  test("Explain Macro with anonymous Algm") {
    // TODO
    val alg = Algm.const(x => 3)

  }

  test("Explain Macro with anonymous Algm with doc") {
    // TODO
    val alg = Algm.const[Int, String](x => x.toString())

    // assert(alg.explain() == "s")

  }

}
