package teseo

import teseo.*


/**
  * Chaining algorithms in a pipeline.
  * Traits `I` and `B` are chained through the companion object of `IB`.
  * Then, it's possible to summon automatically an `IB` instance that chains `I` and `B`.
  */
object Pipeline extends App {

    trait I[A <: Conf] extends Algm[Int, Float] {type CC = A}
    trait B[A <: Conf] extends Algm[Float, Int] {type CC = A}
    trait IB[A <: Conf] extends Algm[Int, Int] {type CC = A}

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
        override val alg: AlgmImpl[Int, Float, Conf.Simple[String]] = AlgmImpl.Simple(_ => ( x => (x+ 1.3).toFloat))

    given B[Conf.Simple[String]] with
        override val alg: AlgmImpl[Float, Int, Conf.Simple[String]] = AlgmImpl.Simple(_ => ( x => x.toInt))

    given String = "o"

    println(s"Result of IB: ${ib ex 3}")

    // Alternative hand-made version of `ib`.
    val alternativeIB = IB.IBImpl(AlgmImpl.Then(AlgmImpl.Wrap[Int, Float, I](), AlgmImpl.Wrap[Float, Int, B]()))
    println(s"Result of alternative IB: ${alternativeIB ex 3}")

    {
        // It's also possible to construct custom IB
        given IB[Conf.Simple[String]] with 
            override val alg = AlgmImpl.Simple(_ => ( x => x+2))

        val newIB = summon[IB[_]]

        println(s"Result of new custom IB: ${newIB ex 3}")
    }

}
