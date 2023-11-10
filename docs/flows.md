# Workflows through `Algm`
An `Algm` and its generic counterpart `AlgmG` reify the notion of algorithm. It is a data structure representing an algorithm that can automatically construct a complex set of configuration through the usage of implicits and determine its actual execution through this. It is highly composable and they are easy to construct. Essentially, it represents a function `A => B` whose execution might depend on implicit parameters. The use of these implicits is not hidden to the user, so the extensive use of implicits does not turn a liability.

## Execution
The main method of the `Algm` API is `ex`. For an `Algm[A, B] { type CC = Config }` or `AlgmImpl[A, B, Config]` instance, method `ex` will construct if possible an instance of `Config` depending on implicits and then execute the algorithm contained in the instance. Instances of `Config` are searched at compile-time, so the user must make them available using `given` or a compile error will be thrown detailing the needed implicits. Take into account that the user only needs to provide the instances used at the moment of execution, so it is possible to create an `Algm` and use it at different times, facilitating a clean architecture.

## `Doc` and `explain` method
When in doubt about an `Algm`, it is possible to use its `doc` to get more information.
```scala mdoc
import teseo.*
import teseo.given

val alg: Algm[Int, Int] = Algm.const( x => x)
alg.doc.addLine("new line for the doc")

alg.doc.toString

```


### `Docs`
A `Doc` is a mutable structure that might contain a title and a list of paragraphs, each one with a subtitle and a content. 
```scala mdoc
// empty Doc
val doc = Doc()

doc.addParagraph("name of paragraph", "first line of paragraph" :: "second line of paragraph" :: Nil)

alg.doc.toString()

```

### `explain`
Each algorithm also has an method `explain` that creates a compiler warning or info with all the information contained in `doc` together with information about the type of the algorithm.

## Operations
It is possible to compose algorithms using the following operators: `andThen`, `and`, `or` and `zip`.

### `andThen`
The operator `andThen` combines two algorithms sequentially: the output of the first one will be the input of the second one.
Its alias is `>>=`.

```scala mdoc
val part1 = Algm.const[Int, Double](x => x.toDouble + 1.7)
val part2 = Algm.const[Double, String](x => x.toString)

part1 ex 3
part2 ex 3.0
(part1 >>= part2) ex 3

```

### `and`
The operator `and` combines in a tuple both input and output of two algorithms.

```scala mdoc
val part3 = Algm.const[Int, Double](x => x.toDouble + 1.7)
val part4 = Algm.const[Double, String](x => x.toString)

part3 ex 3
part4 ex 3.0
(part1 and part2) ex (3, 3.0)

```

### `or`
The operator `or` obtains the necessary dependencies of the algorithm and executes one of the algorithms depending on the dependencies. It tries to execute the first algorithm and then fall back to the second one. If no dependencies are available for both, no algorithm will be executed.
```scala mdoc
val part5 = Algm[Int, Double, Int](y => x => (x + y).toDouble + 1.7)
val part6 = Algm.const[Int, Double](x => x.toDouble)

// doesn't compile because there is no `Int` given
//part5 ex 3
part6 ex 3
(part5 or part6) ex 3 //executes `part6`
(part6 or part5) ex 3

{
    given Int = 10
    part5 ex 3 // now compiles
    (part5 or part6) ex 3 // now executes `part5`
    (part6 or part5) ex 3
}

```

## `AlgmG` or generic `Algm`
While the API of `AlgmG` is yet a work in progress, the basis of it is already prepared. It allows to represent generic functions, possibly bounded.