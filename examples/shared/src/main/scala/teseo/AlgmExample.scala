package teseo

object AlgmExample extends App {

    // Without `teseo.given` it does not compile.
    import teseo.given

    // Has a default `Int` value 10.
    val default = Algm.default[Int, Int, Int](e => a => e + a + 1)(using 10)

    // Doesn't have a default `Int` value.
    val simple = Algm[Int, Int, Int](e => a => e + a + 1)

    println(s"Output: ${default ex 80}. Uses the default value for the execution.")

    // Doesn't compile because there are no given `Int` instances.
    //println(simple ex -100)

    {

        inline given Int = 0
        println(s"Output: ${default ex 80}. Uses the new given value for the execution.")

        println(s"Output: ${simple ex 80}. Compiles because there is a given `Int` instance.")

    }
  
}
