package teseo

import teseo.*


/**
  * Giving an alternative between algorithms.
  * Traits `PrettyPrinter` and `UglyPrinter` are linked in `Printer` through its companion object.
  * Then, it's possible to summon automatically an  `Printer` instance if there is an instance of `PrettyPrinter` or `UglyPrinter` in scope.
  * `PrettyPrinter` will get priority over `UglyPrinter` because it appears first when using `AlgmGen.alternatives`.
  */
object Alternatives extends App {

    trait PrettyPrinter[A <: Conf] extends Algm[Int, String] {type CC = A}
    trait UglyPrinter[A <: Conf] extends Algm[Int, String] {type CC = A}
    trait Printer[A <: Conf] extends Algm[Int, String] {type CC = A}

    // Creating companion object
    object Printer {

        // This case class is needed as a template.
        case class PrinterImpl[A <: Conf](alg: AlgmImpl[Int, String, A]) extends Printer[A]

        // This can be used anywhere
        // Defines a Printer that will default to a PrettyPrinter if available or to an UglyPrinter in other case (if also available)
        val o = AlgmGen.alternatives[Printer, PrettyPrinter, UglyPrinter]
        export o.alg
    }


    import Printer.given
    val printer = summon[Printer[_]]

    // Instances of `I` and `B`.
    given UglyPrinter[Conf.Default[String]] with
        override val alg: AlgmImpl[Int, String, Conf.Default[String]] = AlgmImpl.default(formatter => value => formatter ++ value.toString())

    given String = "Ugly printer with "

    println(printer ex 3)

    {
        // When there is a PrettyPrinter, it gets priority because it appears first in `AlgmGen.alternatives`
        given PrettyPrinter[Conf.Default[Option[Nothing]]] with 
            override val alg = AlgmImpl.const(value => s"Pretty printer with: \n    -> $value")

        // It isn't needed to summon another `Printer`. It automatically checks the available `PrettyPrinter` and `UglyPrinter` in scope
        println(printer ex 3)
    }

}
