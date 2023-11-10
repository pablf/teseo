# TESEO

*TESEO* is a library for the construction of workflows and pipelines through an ergonomic API.

- Easy to create and composable algorithms
- Automatic configuration though implicits
- Detailed compiler help when troubleshooting

*TESEO* provides the `Algm` data structure, representing an algorithm that can automatically construct a complex set of configuration through the usage of implicits and determine its actual execution through this. They are easy to create and highly composable. This allows safer construction and design of pipelines.

## Installation

To use *TESEO*, add the following to your `build.sbt` file:

```scala
libraryDependencies += "io.github.pablf" %% "teseo" % "0.1.0"
```

## Example

A simple example wrapping a function

```scala mdoc
import teseo.*
import teseo.given

// transform an `Int` into a `String`
val alg1 = Algm.const[Int, String](x => x.toString)

alg1 ex 7
```

An example wrapping a function and reading a `given`.

```scala mdoc
// transform an `Int` into a `String` using an implicit `Double``
val alg2 = Algm[Int, String, Double](implic => (x => x.toString ++ implic.toString))

// doesn't compile because there are no `Double` given in scope
//alg ex 7

{
    given Double = 7.7
    alg2 ex 7
}
```

An example wrapping an algorithm and searching an instance in scope.

```scala mdoc
import teseo.*

trait Transformer[A <: Conf] extends Algm[Int, String] { type CC = A }

// executes a transformer in scope
val alg3 = Algm.wrap[Int, String, Transformer]()

// doesn't compile because there are no Transformer in scope
//alg.ex(7) 

{
    given Transformer[Conf.Simple[Double]] with
        val alg = AlgmImpl[Int, String, Double](double => x => x.toString ++ double.toString)

    given Double = 7.7

    alg3 ex 7
}
```

An example creating a pipeline of three algorithms.
```scala
import teseo.*

trait Alg1[A <: Conf] extends Algm[Int, String] { type CC = A }
trait Alg2[A <: Conf] extends Algm[String, Double] { type CC = A }
trait Alg3[A <: Conf] extends Algm[Double, Boolean] { type CC = A }

trait Pipeline[A <: Conf] extends Algm[Int, Boolean] { type CC = A }

object Pipeline {
    
    val container = AlgmGen.pipeline[Alg1, Alg2, Alg3]
    export container.alg
}

// in another file
import Pipeline.given

// it will chain automatically given implementations of `Alg1`, `Alg2` and `Alg3`.
summon[Pipeline[_]] ex 3
```


## Documentation

Learn more on [Teseo](https://teseo.github.io)!

## Contributing

Contributions are welcomed. Feedback is also very important, so any comment is highly appreciated. Thanks!

## License

[License](LICENSE)
