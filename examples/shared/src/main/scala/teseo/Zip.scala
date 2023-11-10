package teseo

import teseo.*


/**
  * Zipping algorithms together.
  * Traits `I1`, `I2` and `I3` are zipped through the companion object of `I`.
  * Then, it's possible to summon automatically an `IB` instance that chains `I` and `B`.
  */
object Zip extends App {

    trait I1[A <: Conf] extends Algm[Int, String] {type CC = A}
    trait I2[A <: Conf] extends Algm[Int, Double] {type CC = A}
    trait I3[A <: Conf] extends Algm[Int, Double] {type CC = A}
    trait I[A <: Conf] extends Algm[(Int, Int, Int), (String, Double, Double)] {type CC = A}

    // Creating companion object to link IB = I + B.
    object I {

        // This case class is needed as a template.
        case class IImpl[A <: Conf](alg: AlgmImpl[(Int, Int, Int), (String, Double, Double), A]) extends I[A]

        // This can be used anywhere
        val o = AlgmGen.zip[I, I1, I2, I3]
        export o.alg
    }


    import I.given
    val i = summon[I[_]]

    // Instances of `I1`, `I2` and `I3`.
    given I1[Conf.Simple[String]] with
        override val alg = AlgmImpl[Int, String, String](_ =>  x => (x + 1.3).toString)

    given I2[Conf.Default[String]] with
        override val alg = AlgmImpl.default[Int, Double, String](_ =>  x => x + 10.3)(using "")

    given I3[Conf.Default[Option[Nothing]]] with
        override val alg = AlgmImpl.const[Int, Double](x => x + 100.3)

    // Doesn't compile because there is no `String` in scope
    //println(s"Result of IB: ${i ex (3, 3, 3)}")

    {
        given String = ""

        println(s"Result of I: ${i ex (3, 3, 3)}")
    }

}
