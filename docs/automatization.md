# Automating the creation of `Algm`
To automate the generation of algorithms, it is possible to use the methods in `teseo.AlgmGen`. The three available methods are: `pipeline`, `zip` and `alternatives`. These apply automatically, respectively, methods `andThen`, `and` and `or` to a sequence of `Algm` subtraits.

## Usage
The following snippet shows the definition of a trait `Token` and different `Algm` subtraits: `Retriever`, `Parser`, `Checker` and `Pipeline`. Let's suppose that this workflow intends to retrieve a `String` from `Connection` using `Retriever`, then `Parser` will take this `String` and break it into the different tokens that compose it, while a `Checker` will check that it is a valid sequence of tokens. We can construct a `given` instance returning a `Pipeline` that can be executed when there are `Retriever`, `Parser` and `Checker` instances in scope with method `AlgmGen.pipeline`.

```scala
import teseo.*

sealed trait Token
class Configuration {
    // details
}
class Connection {
    // details
}

trait Retriever extends Algm[Configuration, String] { type CC = Conf.Default[Connection] }
trait Parser[C <: Conf] extends Algm[String, List[Token]] {type CC = C}
trait Checker[C <: Conf] extends Algm[List[Token], Boolean] {type CC = C}

// Pipeline
trait Pipeline[C <: Conf] extends Algm[String, Boolean] {type CC = C}

val container = AlgmGen.pipeline[Pipeline, Retriever, Parser, Checker]

// Code generated:
/*
val container = new Container[Pipeline] {

    type C = Conf.Then[Conf.Then[Conf.SingleWrap[Conf.Default[Connection], Retriever], Conf.Wrap[Parser]], Conf.Wrap[Checker]]

    given alg: Pipeline[Conf.Then[Conf.Then[Conf.SingleWrap[Conf.Default[Connection], Retriever], Conf.Wrap[Parser]], Conf.Wrap[Checker]]] =

}
*/
```

After this, you can freely use `container.alg` to access the `Pipeline`
```scala
val pipeline = container.alg

val configuration: Configuration = ???

// To actually execute this line, you need to make available through implicits instances of `Retriever`, `Parser` and `Checker`.
val isCorrect = pipeline.ex(configuration)
```

If you try this example, it will ask for a companion object for `Pipeline`, as `AlgmGen.pipeline` can't generate a new trait automatically for the moment (it might be possible in future versions of Scala 3!). Don't worry about this, as the macro will trigger a compiler error with the code that you need. The actual use will be the following:
```scala mdoc:silent:reset
import teseo.*

class Configuration

trait Pipeline[C <: Conf] extends Algm[Configuration, Boolean] {type CC = C}

object Pipeline {
    case class PipelineImpl[A <: Conf](alg: AlgmImpl[Configuration, Boolean, A]) extends Pipeline[A]
}
```
