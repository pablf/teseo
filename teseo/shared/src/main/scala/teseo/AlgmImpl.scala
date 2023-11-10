package teseo

import teseo.internal.Optional.Found
import teseo.internal.Optional.NotFound
import scala.annotation.targetName
import scala.language.implicitConversions

/**
 * Inner implementation for Algm
 */
sealed trait AlgmImpl[A, B, Config <: Conf] { self =>

  val doc: Doc = Doc.apply()

  def accessDoc[A](f: Doc => A): A = f(doc)

  /**
   * Constructs corresponding configuration and executes the algorithm.
   */
  def ex(a: A)(using Config): B

  /**
   * Constructs an algorithm that applies `f` to the output of current algorithm.
   */
  def map[C](f: B => C): AlgmImpl[A, C, Conf.Then[Config, Conf.Default[Option[Nothing]]]] =
    self andThen AlgmImpl.const(f)

  /**
   * Constructs an algorithm that applies `f` to the input before current algorithm.
   */
  def premap[C](f: C => A): AlgmImpl[C, B, Conf.Then[Conf.Default[Option[Nothing]], Config]] =
    AlgmImpl.const(f) andThen self

  /**
   * Zips algorithms with the same input.
   */
  def zip[C, C2 <: Conf](that: AlgmImpl[A, C, C2]) =
    (self ++ that).premap[A](x => (x, x))

  /**
   * Tuples algorithms in parallel. Alias for `and`.
   */
  def ++[C, D, C2 <: Conf](
      that: AlgmImpl[C, D, C2]
  ): AlgmImpl[(A, C), (B, D), Conf.And[Config, C2]] =
    AlgmImpl.And[A, B, C, D, Config, C2](self, that)

  /**
   * Tuples algorithms in parallel.
   */
  def and[C, D, C2 <: Conf](
      that: AlgmImpl[C, D, C2]
  ): AlgmImpl[(A, C), (B, D), Conf.And[Config, C2]] =
    AlgmImpl.And[A, B, C, D, Config, C2](self, that)

  /**
   * Adds a head to a tuple.
   */
  def :*[C <: Tuple, D <: Tuple, C2 <: Conf](
      that: AlgmImpl[C, D, C2]
  ): AlgmImpl[A *: C, B *: D, Conf.And[Config, C2]] =
    AlgmImpl.And[A, B, C, D, Config, C2](self, that)

  /**
   * Chains linearly algorithms. Alias for `andThen`.
   */
  def >>=[C, C2 <: Conf](that: AlgmImpl[B, C, C2]): AlgmImpl[A, C, Conf.Then[Config, C2]] =
    AlgmImpl.Then[A, B, C, Config, C2](self, that)

  /**
   * Chains linearly algorithms.
   */
  def andThen[C, C2 <: Conf](that: AlgmImpl[B, C, C2]): AlgmImpl[A, C, Conf.Then[Config, C2]] =
    AlgmImpl.Then[A, B, C, Config, C2](self, that)

  /**
   * Construct an algorithm that fallbacks to `that` when it is not possible to construct configuration for current algorithm. Alias for `or`.
   */
  def |[C2 <: Conf](that: AlgmImpl[A, B, C2]): AlgmImpl[A, B, Conf.Or[Config, C2]] =
    AlgmImpl.Or[A, B, Config, C2](self, that)

  /**
   * Construct an algorithm that fallbacks to `that` when it is not possible to construct configuration for current algorithm.
   */
  def or[C2 <: Conf](that: AlgmImpl[A, B, C2]): AlgmImpl[A, B, Conf.Or[Config, C2]] =
    AlgmImpl.Or[A, B, Config, C2](self, that)

  /**
   * TODO Constructs an algorithm with new defaults for type `A` when there was no defaults. It does not override current default configuration.
   */
  def provide[A](a: A): AlgmImpl[A, B, AlgmImpl.Provided[A, Config]] = ???

  /**
   * TODO Constructs an algorithm with new defaults for type `A`.
   */
  def overrideDefault[A](a: A): AlgmImpl[A, B, AlgmImpl.Provided[A, Config]] = ???
}

object AlgmImpl {

  /**
   * Methods to construct Algorithms
   */

  // TODO
  type Provided[A, C <: Conf] = C

  /**
   * Constructs an algorithm with default value `E` that, after retrieving configuration, executes `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   * @param e
   *   Default parameter of algorithm.
   */
  def default[A, B, E](f: E => (A => B))(using e: E): AlgmImpl[A, B, Conf.Default[E]] =
    Default(f, e)

  /**
   * Constructs an algorithm requiring a value `E` that, after retrieving configuration, executes `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   */
  def apply[A, B, E](f: E => (A => B)): AlgmImpl[A, B, Conf.Simple[E]] =
    Simple(f)

  /**
   * Constructs an algorithm executing `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   */
  def const[A, B](f: A => B): AlgmImpl[A, B, Conf.Default[Option[Nothing]]] =
    Default(_ => f, None)

  /**
   * Constructs an algorithm that retrieves a given instance of type `M` and fallbacks to it during execution.
   */
  def wrap[A, B, M[_ <: Conf] <: Algm[A, B]](): AlgmImpl[A, B, Conf.Wrap[_, M]] =
    Wrap[A, B, M]()

  /**
   * Constructs an algorithm that retrieves a given instance of type `M` with a fixed configuration and fallbacks to it during execution.
   */
  @targetName("singleWrap")
  def wrap[A, B, C <: Conf, M <: Algm[A, B] { type CC = C }](): AlgmImpl[A, B, Conf.And[C, Conf.Simple[M]]] =
    SingleWrap[A, B, C, M]()

  /**
   * Subclasses of algorithms.
   */

  /**
   * `Simple` is an algorithm that requires a given instance of `E` when executing.
   */
  case class Simple[A, B, E](f: E => (A => B)) extends AlgmImpl[A, B, Conf.Simple[E]] {

    override def ex(a: A)(using conf: Conf.Simple[E]): B =
      val e: E = conf.asInstanceOf[Conf.Simple[E]].e
      f(e)(a)
  }

  /**
   * `Simple` is an algorithm that uses a given instance of `E` when executing if available and a default one in other case.
   */
  case class Default[A, B, E](f: E => (A => B), d: E) extends AlgmImpl[A, B, Conf.Default[E]] {

    override def ex(a: A)(using conf: Conf.Default[E]): B =
      val e: E = conf.asInstanceOf[Conf.Default[E]].find.getOrElse(d)
      f(e)(a)
  }

  /**
   * `Wrap[A, B, M]` represents an algorithm that requires a instance of a subclass of `Algm[A, B]`. Then, execution is deferred to the execution of `M`. It can be useful to create a new algorithm incorporating different standalone algorithms.
   */
  case class Wrap[A, B, M[_ <: Conf] <: Algm[A, B]]() extends AlgmImpl[A, B, Conf.Wrap[_ <: Conf, M]] {

    private def exec[C <: Conf](a: A, conf: Conf.Wrap[C, M]): B =
      conf.alg
        .asInstanceOf[Algm[A, B] { type CC = C }]
        .ex(a)(using internal.Error.empty, internal.Optional.Found(conf.c))

    override def ex(a: A)(using conf: Conf.Wrap[? <: Conf, M]): B =
      exec(a, conf)

  }

  /**
   * `SingleWrap[A, B, C, M]` represents an algorithm that requires a instance of a subclass of `Algm[A, B]` with already a predefined configuration.
   */
  case class SingleWrap[A, B, C <: Conf, M <: Algm[A, B] { type CC = C }]() extends AlgmImpl[A, B, Conf.And[C, Conf.Simple[M]]] {

    override def ex(a: A)(using conf: Conf.And[C, Conf.Simple[M]]): B =
      conf match {
        case Conf.And(c1, Conf.Simple(m)) => m.alg.ex(a)(using c1)
      }

  }

  /**
   * Zips two algorithms `A => B` and `C => D` into one `(A, C) => (B, D)`.
   */
  case class And[A, B, C, D, C1 <: Conf, C2 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[C, D, C2]
  ) extends AlgmImpl[(A, C), (B, D), Conf.And[C1, C2]] {

    override def ex(pair: (A, C))(using conf: Conf.And[C1, C2]): (B, D) =
      conf match {
        case and: Conf.And[C1, C2] => (a1.ex(pair._1)(using and.a1), a2.ex(pair._2)(using and.a2))
      }
  }

  /**
   * Concatenates two algorithms linearly.
   */
  case class Then[A, B, C, C1 <: Conf, C2 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[B, C, C2]
  ) extends AlgmImpl[A, C, Conf.Then[C1, C2]] {

    override def ex(a: A)(using conf: Conf.Then[C1, C2]): C =
      conf match {
        case and: Conf.Then[C1, C2] => a2.ex(a1.ex(a)(using and.a1))(using and.a2)
      }
  }

  /**
   * Creates an algorithm that executes `a1` if possible and fallbacks to `a2`.
   */
  case class Or[A, B, C1 <: Conf, C2 <: Conf](a1: AlgmImpl[A, B, C1], a2: AlgmImpl[A, B, C2]) extends AlgmImpl[A, B, Conf.Or[C1, C2]] {

    override def ex(a: A)(using conf: Conf.Or[C1, C2]): B =
      conf match {
        case or: Conf.Or[C1, C2] =>
          or.find match {
            case Left(conf1)  => a1.ex(a)(using conf1)
            case Right(conf2) => a2.ex(a)(using conf2)
          }
      }
  }

  private[teseo] case class Custom[A, B, C <: Conf](f: (A, C) => B) extends AlgmImpl[A, B, C] {

    override def ex(a: A)(using c: C): B = f(a, c)

  }

  /**
   * Combination methods.
   */

  def or[A, B, C1 <: Conf, C2 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2]
  ): AlgmImpl[A, B, Conf.Or[C1, C2]] = AlgmImpl.Or(a1, a2)

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, C3]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, a3))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, C4]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, a4)))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, C5]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, a5))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, C6]]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, a6)))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, C7]]]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, a7))))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, C8]]]]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, a8)))))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, C9]]]]]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, a9))))))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, C10]]]]]]]]]] = AlgmImpl.Or(a1, AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, a10)))))))))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, C11]]]]]]]]]]] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(a2, AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, a11)))))))))
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12]
  ): AlgmImpl[A, B, Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, C12]]]]]]]]]]]] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(a3, AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, a12)))))))))
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, C13]]]]]]]]]]]]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(a4, AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, a13)))))))))
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, C14]]]]]]]]]]]]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(a5, AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, a14)))))))))
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, C15]]]]]]]]]]]]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(a6, AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, a15)))))))))
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, C16]]]]]]]]]]]]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(a7, AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, a16)))))))))
            )
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16],
      a17: AlgmImpl[A, B, C17]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[
            C4,
            Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, Conf.Or[C16, C17]]]]]]]]]]]]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(
                a7,
                AlgmImpl.Or(a8, AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, AlgmImpl.Or(a16, a17)))))))))
              )
            )
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16],
      a17: AlgmImpl[A, B, C17],
      a18: AlgmImpl[A, B, C18]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[
            C4,
            Conf.Or[
              C5,
              Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, Conf.Or[C16, Conf.Or[C17, C18]]]]]]]]]]]]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(
                a7,
                AlgmImpl.Or(
                  a8,
                  AlgmImpl.Or(a9, AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, AlgmImpl.Or(a16, AlgmImpl.Or(a17, a18)))))))))
                )
              )
            )
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16],
      a17: AlgmImpl[A, B, C17],
      a18: AlgmImpl[A, B, C18],
      a19: AlgmImpl[A, B, C19]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[
            C4,
            Conf.Or[
              C5,
              Conf.Or[
                C6,
                Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, Conf.Or[C16, Conf.Or[C17, Conf.Or[C18, C19]]]]]]]]]]]]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(
                a7,
                AlgmImpl.Or(
                  a8,
                  AlgmImpl.Or(
                    a9,
                    AlgmImpl.Or(a10, AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, AlgmImpl.Or(a16, AlgmImpl.Or(a17, AlgmImpl.Or(a18, a19)))))))))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16],
      a17: AlgmImpl[A, B, C17],
      a18: AlgmImpl[A, B, C18],
      a19: AlgmImpl[A, B, C19],
      a20: AlgmImpl[A, B, C20]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[
            C4,
            Conf.Or[
              C5,
              Conf.Or[
                C6,
                Conf.Or[
                  C7,
                  Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, Conf.Or[C16, Conf.Or[C17, Conf.Or[C18, Conf.Or[C19, C20]]]]]]]]]]]]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(
                a7,
                AlgmImpl.Or(
                  a8,
                  AlgmImpl.Or(
                    a9,
                    AlgmImpl.Or(
                      a10,
                      AlgmImpl.Or(a11, AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, AlgmImpl.Or(a16, AlgmImpl.Or(a17, AlgmImpl.Or(a18, AlgmImpl.Or(a19, a20)))))))))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: AlgmImpl[A, B, C1],
      a2: AlgmImpl[A, B, C2],
      a3: AlgmImpl[A, B, C3],
      a4: AlgmImpl[A, B, C4],
      a5: AlgmImpl[A, B, C5],
      a6: AlgmImpl[A, B, C6],
      a7: AlgmImpl[A, B, C7],
      a8: AlgmImpl[A, B, C8],
      a9: AlgmImpl[A, B, C9],
      a10: AlgmImpl[A, B, C10],
      a11: AlgmImpl[A, B, C11],
      a12: AlgmImpl[A, B, C12],
      a13: AlgmImpl[A, B, C13],
      a14: AlgmImpl[A, B, C14],
      a15: AlgmImpl[A, B, C15],
      a16: AlgmImpl[A, B, C16],
      a17: AlgmImpl[A, B, C17],
      a18: AlgmImpl[A, B, C18],
      a19: AlgmImpl[A, B, C19],
      a20: AlgmImpl[A, B, C20],
      a21: AlgmImpl[A, B, C21]
  ): AlgmImpl[
    A,
    B,
    Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[
            C4,
            Conf.Or[
              C5,
              Conf.Or[
                C6,
                Conf.Or[
                  C7,
                  Conf.Or[
                    C8,
                    Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, Conf.Or[C16, Conf.Or[C17, Conf.Or[C18, Conf.Or[C19, Conf.Or[C20, C21]]]]]]]]]]]]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Or(
    a1,
    AlgmImpl.Or(
      a2,
      AlgmImpl.Or(
        a3,
        AlgmImpl.Or(
          a4,
          AlgmImpl.Or(
            a5,
            AlgmImpl.Or(
              a6,
              AlgmImpl.Or(
                a7,
                AlgmImpl.Or(
                  a8,
                  AlgmImpl.Or(
                    a9,
                    AlgmImpl.Or(
                      a10,
                      AlgmImpl.Or(
                        a11,
                        AlgmImpl.Or(a12, AlgmImpl.Or(a13, AlgmImpl.Or(a14, AlgmImpl.Or(a15, AlgmImpl.Or(a16, AlgmImpl.Or(a17, AlgmImpl.Or(a18, AlgmImpl.Or(a19, AlgmImpl.Or(a20, a21)))))))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, C1 <: Conf, C2 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2]
  ): AlgmImpl[A1, A3, Conf.Then[C1, C2]] = AlgmImpl.Then(a1, a2)

  def andThen[A1, A2, A3, A4, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3]
  ): AlgmImpl[A1, A4, Conf.Then[C1, Conf.Then[C2, C3]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, a3))

  def andThen[A1, A2, A3, A4, A5, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4]
  ): AlgmImpl[A1, A5, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, C4]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, a4)))

  def andThen[A1, A2, A3, A4, A5, A6, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5]
  ): AlgmImpl[A1, A6, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, C5]]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, a5))))

  def andThen[A1, A2, A3, A4, A5, A6, A7, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6]
  ): AlgmImpl[A1, A7, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, C6]]]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, a6)))))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7]
  ): AlgmImpl[A1, A8, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, C7]]]]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, a7))))))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8]
  ): AlgmImpl[A1, A9, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, C8]]]]]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, a8)))))))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9]
  ): AlgmImpl[A1, A10, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, C9]]]]]]]]] = AlgmImpl.Then(a1, AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, a9))))))))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10]
  ): AlgmImpl[A1, A11, Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, C10]]]]]]]]]] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(a2, AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, a10))))))))
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11]
  ): AlgmImpl[
    A1,
    A12,
    Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, C11]]]]]]]]]]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(a3, AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, a11))))))))
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12]
  ): AlgmImpl[
    A1,
    A13,
    Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, C12]]]]]]]]]]]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(a4, AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, a12))))))))
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13]
  ): AlgmImpl[
    A1,
    A14,
    Conf.Then[
      C1,
      Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, C13]]]]]]]]]]]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(a5, AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, a13))))))))
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14]
  ): AlgmImpl[
    A1,
    A15,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, C14]]]]]]]]]]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(a6, AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, a14))))))))
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15]
  ): AlgmImpl[
    A1,
    A16,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, C15]]]]]]]]]]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(a7, AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, a15))))))))
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16]
  ): AlgmImpl[
    A1,
    A17,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, C16]]]]]]]]]]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(a8, AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, a16))))))))
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16],
      a17: AlgmImpl[A17, A18, C17]
  ): AlgmImpl[
    A1,
    A18,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[
                C6,
                Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, Conf.Then[C16, C17]]]]]]]]]]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(
                  a8,
                  AlgmImpl.Then(a9, AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, AlgmImpl.Then(a16, a17))))))))
                )
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16],
      a17: AlgmImpl[A17, A18, C17],
      a18: AlgmImpl[A18, A19, C18]
  ): AlgmImpl[
    A1,
    A19,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[
                C6,
                Conf.Then[
                  C7,
                  Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, Conf.Then[C16, Conf.Then[C17, C18]]]]]]]]]]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(
                  a8,
                  AlgmImpl.Then(
                    a9,
                    AlgmImpl.Then(a10, AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, AlgmImpl.Then(a16, AlgmImpl.Then(a17, a18))))))))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16],
      a17: AlgmImpl[A17, A18, C17],
      a18: AlgmImpl[A18, A19, C18],
      a19: AlgmImpl[A19, A20, C19]
  ): AlgmImpl[
    A1,
    A20,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[
                C6,
                Conf.Then[
                  C7,
                  Conf.Then[
                    C8,
                    Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, Conf.Then[C16, Conf.Then[C17, Conf.Then[C18, C19]]]]]]]]]]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(
                  a8,
                  AlgmImpl.Then(
                    a9,
                    AlgmImpl.Then(
                      a10,
                      AlgmImpl.Then(a11, AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, AlgmImpl.Then(a16, AlgmImpl.Then(a17, AlgmImpl.Then(a18, a19))))))))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16],
      a17: AlgmImpl[A17, A18, C17],
      a18: AlgmImpl[A18, A19, C18],
      a19: AlgmImpl[A19, A20, C19],
      a20: AlgmImpl[A20, A21, C20]
  ): AlgmImpl[
    A1,
    A21,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[
                C6,
                Conf.Then[
                  C7,
                  Conf.Then[
                    C8,
                    Conf.Then[
                      C9,
                      Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, Conf.Then[C16, Conf.Then[C17, Conf.Then[C18, Conf.Then[C19, C20]]]]]]]]]]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(
                  a8,
                  AlgmImpl.Then(
                    a9,
                    AlgmImpl.Then(
                      a10,
                      AlgmImpl.Then(
                        a11,
                        AlgmImpl.Then(a12, AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, AlgmImpl.Then(a16, AlgmImpl.Then(a17, AlgmImpl.Then(a18, AlgmImpl.Then(a19, a20))))))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: AlgmImpl[A1, A2, C1],
      a2: AlgmImpl[A2, A3, C2],
      a3: AlgmImpl[A3, A4, C3],
      a4: AlgmImpl[A4, A5, C4],
      a5: AlgmImpl[A5, A6, C5],
      a6: AlgmImpl[A6, A7, C6],
      a7: AlgmImpl[A7, A8, C7],
      a8: AlgmImpl[A8, A9, C8],
      a9: AlgmImpl[A9, A10, C9],
      a10: AlgmImpl[A10, A11, C10],
      a11: AlgmImpl[A11, A12, C11],
      a12: AlgmImpl[A12, A13, C12],
      a13: AlgmImpl[A13, A14, C13],
      a14: AlgmImpl[A14, A15, C14],
      a15: AlgmImpl[A15, A16, C15],
      a16: AlgmImpl[A16, A17, C16],
      a17: AlgmImpl[A17, A18, C17],
      a18: AlgmImpl[A18, A19, C18],
      a19: AlgmImpl[A19, A20, C19],
      a20: AlgmImpl[A20, A21, C20],
      a21: AlgmImpl[A21, A22, C21]
  ): AlgmImpl[
    A1,
    A22,
    Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[
            C4,
            Conf.Then[
              C5,
              Conf.Then[
                C6,
                Conf.Then[
                  C7,
                  Conf.Then[
                    C8,
                    Conf.Then[
                      C9,
                      Conf.Then[
                        C10,
                        Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, Conf.Then[C14, Conf.Then[C15, Conf.Then[C16, Conf.Then[C17, Conf.Then[C18, Conf.Then[C19, Conf.Then[C20, C21]]]]]]]]]]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = AlgmImpl.Then(
    a1,
    AlgmImpl.Then(
      a2,
      AlgmImpl.Then(
        a3,
        AlgmImpl.Then(
          a4,
          AlgmImpl.Then(
            a5,
            AlgmImpl.Then(
              a6,
              AlgmImpl.Then(
                a7,
                AlgmImpl.Then(
                  a8,
                  AlgmImpl.Then(
                    a9,
                    AlgmImpl.Then(
                      a10,
                      AlgmImpl.Then(
                        a11,
                        AlgmImpl.Then(
                          a12,
                          AlgmImpl.Then(a13, AlgmImpl.Then(a14, AlgmImpl.Then(a15, AlgmImpl.Then(a16, AlgmImpl.Then(a17, AlgmImpl.Then(a18, AlgmImpl.Then(a19, AlgmImpl.Then(a20, a21))))))))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  def and[A1, B1, A2, B2, C1 <: Conf, C2 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2]
  ): AlgmImpl[(A1, A2), (B1, B2), Conf.And[C1, C2]] = a1 and a2

  def and[A1, B1, A2, B2, A3, B3, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3]
  ): AlgmImpl[(A1, A2, A3), (B1, B2, B3), Conf.And[C1, Conf.And[C2, C3]]] = a1 :* (a2 and a3)

  def and[A1, B1, A2, B2, A3, B3, A4, B4, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4]
  ): AlgmImpl[(A1, A2, A3, A4), (B1, B2, B3, B4), Conf.And[C1, Conf.And[C2, Conf.And[C3, C4]]]] = a1 :* (a2 :* (a3 and a4))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5]
  ): AlgmImpl[(A1, A2, A3, A4, A5), (B1, B2, B3, B4, B5), Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, C5]]]]] = a1 :* (a2 :* (a3 :* (a4 and a5)))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6]
  ): AlgmImpl[(A1, A2, A3, A4, A5, A6), (B1, B2, B3, B4, B5, B6), Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, C6]]]]]] = a1 :* (a2 :* (a3 :* (a4 :* (a5 and a6))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7]
  ): AlgmImpl[(A1, A2, A3, A4, A5, A6, A7), (B1, B2, B3, B4, B5, B6, B7), Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, C7]]]]]]] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 and a7)))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8]
  ): AlgmImpl[(A1, A2, A3, A4, A5, A6, A7, A8), (B1, B2, B3, B4, B5, B6, B7, B8), Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, C8]]]]]]]] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 and a8))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9),
    Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, C9]]]]]]]]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 and a9)))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10),
    Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, C10]]]]]]]]]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 and a10))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11),
    Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, C11]]]]]]]]]]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 and a11)))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12),
    Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, C12]]]]]]]]]]]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 and a12))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13),
    Conf.And[
      C1,
      Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, C13]]]]]]]]]]]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 and a13)))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, C14]]]]]]]]]]]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 and a14))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, C15]]]]]]]]]]]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 and a15)))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, C16]]]]]]]]]]]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 and a16))))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16],
      a17: AlgmImpl[A17, B17, C17]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[
              C5,
              Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, Conf.And[C16, C17]]]]]]]]]]]
            ]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 :* (a16 and a17)))))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16],
      a17: AlgmImpl[A17, B17, C17],
      a18: AlgmImpl[A18, B18, C18]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[
              C5,
              Conf.And[
                C6,
                Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, Conf.And[C16, Conf.And[C17, C18]]]]]]]]]]]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 :* (a16 :* (a17 and a18))))))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16],
      a17: AlgmImpl[A17, B17, C17],
      a18: AlgmImpl[A18, B18, C18],
      a19: AlgmImpl[A19, B19, C19]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[
              C5,
              Conf.And[
                C6,
                Conf.And[
                  C7,
                  Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, Conf.And[C16, Conf.And[C17, Conf.And[C18, C19]]]]]]]]]]]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 :* (a16 :* (a17 :* (a18 and a19)))))))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, A20, B20, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16],
      a17: AlgmImpl[A17, B17, C17],
      a18: AlgmImpl[A18, B18, C18],
      a19: AlgmImpl[A19, B19, C19],
      a20: AlgmImpl[A20, B20, C20]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[
              C5,
              Conf.And[
                C6,
                Conf.And[
                  C7,
                  Conf.And[
                    C8,
                    Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, Conf.And[C16, Conf.And[C17, Conf.And[C18, Conf.And[C19, C20]]]]]]]]]]]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 :* (a16 :* (a17 :* (a18 :* (a19 and a20))))))))))))))))))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, A20, B20, A21, B21, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: AlgmImpl[A1, B1, C1],
      a2: AlgmImpl[A2, B2, C2],
      a3: AlgmImpl[A3, B3, C3],
      a4: AlgmImpl[A4, B4, C4],
      a5: AlgmImpl[A5, B5, C5],
      a6: AlgmImpl[A6, B6, C6],
      a7: AlgmImpl[A7, B7, C7],
      a8: AlgmImpl[A8, B8, C8],
      a9: AlgmImpl[A9, B9, C9],
      a10: AlgmImpl[A10, B10, C10],
      a11: AlgmImpl[A11, B11, C11],
      a12: AlgmImpl[A12, B12, C12],
      a13: AlgmImpl[A13, B13, C13],
      a14: AlgmImpl[A14, B14, C14],
      a15: AlgmImpl[A15, B15, C15],
      a16: AlgmImpl[A16, B16, C16],
      a17: AlgmImpl[A17, B17, C17],
      a18: AlgmImpl[A18, B18, C18],
      a19: AlgmImpl[A19, B19, C19],
      a20: AlgmImpl[A20, B20, C20],
      a21: AlgmImpl[A21, B21, C21]
  ): AlgmImpl[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21),
    Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[
            C4,
            Conf.And[
              C5,
              Conf.And[
                C6,
                Conf.And[
                  C7,
                  Conf.And[
                    C8,
                    Conf.And[
                      C9,
                      Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, Conf.And[C15, Conf.And[C16, Conf.And[C17, Conf.And[C18, Conf.And[C19, Conf.And[C20, C21]]]]]]]]]]]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ] = a1 :* (a2 :* (a3 :* (a4 :* (a5 :* (a6 :* (a7 :* (a8 :* (a9 :* (a10 :* (a11 :* (a12 :* (a13 :* (a14 :* (a15 :* (a16 :* (a17 :* (a18 :* (a19 :* (a20 and a21)))))))))))))))))))

}

/*
Generation for or

def header(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"C$i <: Conf"
  "def or[A, B, " ++ a.mkString(", ") ++ "]("
}

def params(i: Int): String = {
  val a = for {
    i <- Range(1, i)
  } yield s"a$i: AlgmImpl[A, B, C$i],"
  a.mkString("\n  ")
}

def tpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.Or[C$acc, ${tpe(i, acc + 1)}]"
def value(i: Int, acc: Int): String = if (acc == i - 1) s"a$acc" else s"AlgmImpl.Or(a$acc, ${value(i, acc + 1)})"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i)
  } yield s"a$i.alg"

  s"): AlgmImpl[A, B, ${tpe(i, 1)}] = ${value(i, 1)}"
}

def total(i: Int) =
s"""${header(i + 1 )}
  ${params(i + 1)}
${body(i + 1)}

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)

 */

/*
Generation for andThen

def header(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"C$i <: Conf"

  val b = for {
    i <- Range(1, i + 1)
  } yield s"A$i"

  "def andThen[" ++ b.mkString(", ") ++ ", " ++ a.mkString(", ") ++ "]("
}

def params(i: Int): String = {
  val a = for {
    i <- Range(1, i)
  } yield s"a$i: AlgmImpl[A$i, A${i + 1}, C$i],"
  a.mkString("\n  ")
}

def tpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.Then[C$acc, ${tpe(i, acc + 1)}]"
def value(i: Int, acc: Int): String = if (acc == i - 1) s"a$acc" else s"AlgmImpl.Then(a$acc, ${value(i, acc + 1)})"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i)
  } yield s"a$i.alg"

  s"): AlgmImpl[A1, A$i, ${tpe(i, 1)}] = ${value(i, 1)}"
}

def total(i: Int) =
s"""${header(i + 1 )}
  ${params(i + 1)}
${body(i + 1)}

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)

 */

/*
Generation for and

def header(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"C$i <: Conf"

  val b = for {
    i <- Range(1, i)
  } yield s"A$i, B$i"

  "def and[" ++ b.mkString(", ") ++ ", " ++ a.mkString(", ") ++ "]("
}

def params(i: Int): String = {
  val a = for {
    i <- Range(1, i)
  } yield s"a$i: AlgmImpl[A$i, B$i, C$i],"
  a.mkString("\n  ")
}

def tpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.And[C$acc, ${tpe(i, acc + 1)}]"
def fnTpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.And[C$acc, ${tpe(i, acc + 1)}]"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i - 1)
  } yield s"a$i"

  val as = for {
    i <- Range(1, i )
  } yield s"A$i"

  val bs = for {
    i <- Range(1, i)
  } yield s"B$i"

  s"""): AlgmImpl[(${as.mkString(", ")}), (${bs.mkString(", ")}), ${tpe(i, 1)}] = ${a.mkString(" :* (")} and a${i-1}${")"*{ i - 3}}"""
}

def total(i: Int) =
s"""${header(i + 1 )}
  ${params(i + 1)}
${body(i + 1)}

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)

 */
