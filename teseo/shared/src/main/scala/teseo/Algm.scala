package teseo

import teseo.internal.*
import scala.annotation.experimental
import scala.annotation.targetName

/**
 * `Algm[A, B]` is the main object in Teseo and represents an algorithm receiving a value of type `A` and giving a value of type `B`. Type `CC` describes the given objects that will be used when executing the algorithm. They are only required when using method `ex`. This allows to delay the assemblying of the algorithm until execution. In most cases, Teseo will construct the correct type `CC`, so it should go unrefined.
 */
trait Algm[A, B] { self =>

  type CC <: Conf

  type Self = Algm[A, B]

  val alg: AlgmImpl[A, B, CC]

  def doc = alg.doc

  def accessDoc[A](f: Doc => A): A = f(doc)

  def mapAlgm[C, D, C2 <: Conf](
      f: AlgmImpl[A, B, CC] => AlgmImpl[C, D, C2]
  ): Algm[C, D] { type CC = C2 } = new Algm[C, D] {

    type CC = C2

    override val alg: AlgmImpl[C, D, C2] = f(self.alg)
  }

  /**
   * Executes the underlying algorithm.
   */
  inline def ex(a: A)(using inline error: Error[CC], config: Optional[CC]): B = {
    val conf: CC =
      inline if Error.compare(error) then config.summon.get else Error.triggerError(error)
    alg.ex(a)(using conf)
  }

  def isEqual[C <: Conf](algm2: Algm[A, B] { type CC = C }): Boolean = algm2.alg == alg

  /**
   * Creates a compiler message with information about the algorithm.
   *
   * @param warn
   *   `true` creates a warning, `false` creates info.
   */
  inline def explain(warn: Boolean = false): String = ${
    ExplainMacro.explain[A, B, CC, Self]('warn, '{ doc.toString() })
  }

  /**
   * Constructs an algorithm that applies `f` to the output of current algorithm.
   */
  def map[C](f: B => C) =
    self andThen Algm.const(f)

  /**
   * Constructs an algorithm that applies `f` to the input before current algorithm.
   */
  def premap[C](f: C => A) =
    Algm.const(f) andThen self

  /**
   * Zips algorithms with the same input.
   */
  def zip[C, C2 <: Conf](that: Algm[A, C]) =
    (self ++ that).premap[A](x => (x, x))

  /**
   * Tuples algorithms in parallel. Alias for `and`.
   */
  def ++[C, D, C2 <: Conf](that: Algm[C, D]) =
    Algm.and(self, that)

  /**
   * Tuples algorithms in parallel.
   */
  def and[C, D, C2 <: Conf](that: Algm[C, D]) = Algm.and(self, that)

  /**
   * Adds a head to a tuple.
   */
  def :*[C <: Tuple, D <: Tuple, C2 <: Conf](that: Algm[C, D]) = Algm.mk(alg :* that.alg)

  /**
   * Chains linearly algorithms. Alias for `andThen`.
   */
  def >>=[C, C2 <: Conf](that: Algm[B, C]) =
    Algm.andThen(self, that)

  /**
   * Chains linearly algorithms.
   */
  def andThen[C, C2 <: Conf](that: Algm[B, C]) =
    Algm.andThen(self, that)

  /**
   * Construct an algorithm that fallbacks to `that` when it is not possible to construct configuration for current algorithm. Alias for `or`.
   */
  def |[C2 <: Conf](that: Algm[A, B]) =
    Algm.or(self, that)

  /**
   * Construct an algorithm that fallbacks to `that` when it is not possible to construct configuration for current algorithm.
   */
  def or[C2 <: Conf](that: Algm[A, B]) =
    Algm.or(self, that)

}

object Algm {

  inline def mk[A, B, C <: Conf](inline algImpl: AlgmImpl[A, B, C]): Algm[A, B] { type CC = C } =
    new Algm[A, B] {

      type CC = C

      override val alg: AlgmImpl[A, B, C] = algImpl

    }

  /**
   * Methods to construct Algorithms
   */

  /**
   * Constructs an algorithm with default value `E` that, after retrieving configuration, executes `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   * @param e
   *   Default parameter of algorithm.
   */
  def default[A, B, E](f: E => (A => B))(using e: E): Algm[A, B] { type CC = Conf.Default[E] } =
    Algm.mk(AlgmImpl.default(f)(using e))

  /**
   * Constructs an algorithm requiring a value `E` that, after retrieving configuration, executes `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   */
  def apply[A, B, E](f: E => (A => B)): Algm[A, B] { type CC = Conf.Simple[E] } =
    Algm.mk(AlgmImpl[A, B, E](f))

  /**
   * Constructs an algorithm executing `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   */
  def const[A, B](f: A => B): Algm[A, B] { type CC = Conf.Default[Option[Nothing]] } =
    Algm.mk(AlgmImpl.const(f))

  /**
   * Constructs an algorithm that retrieves a given instance of type `M` and fallbacks to it during execution.
   */
  def wrap[A, B, M[_ <: Conf] <: Algm[A, B]](): Algm[A, B] { type CC = Conf.Wrap[_, M] } =
    Algm.mk(AlgmImpl.wrap[A, B, M]())

  /**
   * Constructs an algorithm that retrieves a given instance of type `M` with a fixed configuration and fallbacks to it during execution.
   */
  @targetName("singleWrap")
  def wrap[A, B, C <: Conf, M <: Algm[A, B] { type CC = C }](): Algm[A, B] { type CC = Conf.And[C, Conf.Simple[M]] } =
    Algm.mk(AlgmImpl.wrap[A, B, C, M]())

  /**
   * Combination methods.
   */

  def or[A, B, C1 <: Conf, C2 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 }
  ): Algm[A, B] { type CC = Conf.Or[C1, C2] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, C3]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, C4]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, C5]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, C6]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, C7]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, C8]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, C9]]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, C10]]]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, C11]]]]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, C12]]]]]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 }
  ): Algm[A, B] { type CC = Conf.Or[C1, Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, C13]]]]]]]]]]]] } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 }
  ): Algm[A, B] {
    type CC = Conf.Or[
      C1,
      Conf.Or[C2, Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, C14]]]]]]]]]]]]
    ]
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 }
  ): Algm[A, B] {
    type CC = Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[C3, Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, C15]]]]]]]]]]]]
      ]
    ]
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 }
  ): Algm[A, B] {
    type CC = Conf.Or[
      C1,
      Conf.Or[
        C2,
        Conf.Or[
          C3,
          Conf.Or[C4, Conf.Or[C5, Conf.Or[C6, Conf.Or[C7, Conf.Or[C8, Conf.Or[C9, Conf.Or[C10, Conf.Or[C11, Conf.Or[C12, Conf.Or[C13, Conf.Or[C14, Conf.Or[C15, C16]]]]]]]]]]]]
        ]
      ]
    ]
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 },
      a17: Algm[A, B] { type CC = C17 }
  ): Algm[A, B] {
    type CC = Conf.Or[
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
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 },
      a17: Algm[A, B] { type CC = C17 },
      a18: Algm[A, B] { type CC = C18 }
  ): Algm[A, B] {
    type CC = Conf.Or[
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
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 },
      a17: Algm[A, B] { type CC = C17 },
      a18: Algm[A, B] { type CC = C18 },
      a19: Algm[A, B] { type CC = C19 }
  ): Algm[A, B] {
    type CC = Conf.Or[
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
  } = Algm.mk(AlgmImpl.or(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg, a19.alg))

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 },
      a17: Algm[A, B] { type CC = C17 },
      a18: Algm[A, B] { type CC = C18 },
      a19: Algm[A, B] { type CC = C19 },
      a20: Algm[A, B] { type CC = C20 }
  ): Algm[A, B] {
    type CC = Conf.Or[
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
  } = Algm.mk(
    AlgmImpl.or(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg
    )
  )

  def or[A, B, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: Algm[A, B] { type CC = C1 },
      a2: Algm[A, B] { type CC = C2 },
      a3: Algm[A, B] { type CC = C3 },
      a4: Algm[A, B] { type CC = C4 },
      a5: Algm[A, B] { type CC = C5 },
      a6: Algm[A, B] { type CC = C6 },
      a7: Algm[A, B] { type CC = C7 },
      a8: Algm[A, B] { type CC = C8 },
      a9: Algm[A, B] { type CC = C9 },
      a10: Algm[A, B] { type CC = C10 },
      a11: Algm[A, B] { type CC = C11 },
      a12: Algm[A, B] { type CC = C12 },
      a13: Algm[A, B] { type CC = C13 },
      a14: Algm[A, B] { type CC = C14 },
      a15: Algm[A, B] { type CC = C15 },
      a16: Algm[A, B] { type CC = C16 },
      a17: Algm[A, B] { type CC = C17 },
      a18: Algm[A, B] { type CC = C18 },
      a19: Algm[A, B] { type CC = C19 },
      a20: Algm[A, B] { type CC = C20 },
      a21: Algm[A, B] { type CC = C21 }
  ): Algm[A, B] {
    type CC = Conf.Or[
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
  } = Algm.mk(
    AlgmImpl.or(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg,
      a21.alg
    )
  )

  def andThen[A1, A2, A3, C1 <: Conf, C2 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 }
  ): Algm[A1, A3] { type CC = Conf.Then[C1, C2] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg))

  def andThen[A1, A2, A3, A4, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 }
  ): Algm[A1, A4] { type CC = Conf.Then[C1, Conf.Then[C2, C3]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg))

  def andThen[A1, A2, A3, A4, A5, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 }
  ): Algm[A1, A5] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, C4]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg))

  def andThen[A1, A2, A3, A4, A5, A6, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 }
  ): Algm[A1, A6] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, C5]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 }
  ): Algm[A1, A7] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, C6]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 }
  ): Algm[A1, A8] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, C7]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 }
  ): Algm[A1, A9] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, C8]]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 }
  ): Algm[A1, A10] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, C9]]]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 }
  ): Algm[A1, A11] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, C10]]]]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 }
  ): Algm[A1, A12] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, C11]]]]]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 }
  ): Algm[A1, A13] { type CC = Conf.Then[C1, Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, C12]]]]]]]]]]] } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 }
  ): Algm[A1, A14] {
    type CC = Conf.Then[
      C1,
      Conf.Then[C2, Conf.Then[C3, Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, C13]]]]]]]]]]]
    ]
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 }
  ): Algm[A1, A15] {
    type CC = Conf.Then[
      C1,
      Conf.Then[
        C2,
        Conf.Then[
          C3,
          Conf.Then[C4, Conf.Then[C5, Conf.Then[C6, Conf.Then[C7, Conf.Then[C8, Conf.Then[C9, Conf.Then[C10, Conf.Then[C11, Conf.Then[C12, Conf.Then[C13, C14]]]]]]]]]]
        ]
      ]
    ]
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 }
  ): Algm[A1, A16] {
    type CC = Conf.Then[
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
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 }
  ): Algm[A1, A17] {
    type CC = Conf.Then[
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
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 },
      a17: Algm[A17, A18] { type CC = C17 }
  ): Algm[A1, A18] {
    type CC = Conf.Then[
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
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 },
      a17: Algm[A17, A18] { type CC = C17 },
      a18: Algm[A18, A19] { type CC = C18 }
  ): Algm[A1, A19] {
    type CC = Conf.Then[
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
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 },
      a17: Algm[A17, A18] { type CC = C17 },
      a18: Algm[A18, A19] { type CC = C18 },
      a19: Algm[A19, A20] { type CC = C19 }
  ): Algm[A1, A20] {
    type CC = Conf.Then[
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
  } = Algm.mk(AlgmImpl.andThen(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg, a19.alg))

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 },
      a17: Algm[A17, A18] { type CC = C17 },
      a18: Algm[A18, A19] { type CC = C18 },
      a19: Algm[A19, A20] { type CC = C19 },
      a20: Algm[A20, A21] { type CC = C20 }
  ): Algm[A1, A21] {
    type CC = Conf.Then[
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
  } = Algm.mk(
    AlgmImpl.andThen(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg
    )
  )

  def andThen[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: Algm[A1, A2] { type CC = C1 },
      a2: Algm[A2, A3] { type CC = C2 },
      a3: Algm[A3, A4] { type CC = C3 },
      a4: Algm[A4, A5] { type CC = C4 },
      a5: Algm[A5, A6] { type CC = C5 },
      a6: Algm[A6, A7] { type CC = C6 },
      a7: Algm[A7, A8] { type CC = C7 },
      a8: Algm[A8, A9] { type CC = C8 },
      a9: Algm[A9, A10] { type CC = C9 },
      a10: Algm[A10, A11] { type CC = C10 },
      a11: Algm[A11, A12] { type CC = C11 },
      a12: Algm[A12, A13] { type CC = C12 },
      a13: Algm[A13, A14] { type CC = C13 },
      a14: Algm[A14, A15] { type CC = C14 },
      a15: Algm[A15, A16] { type CC = C15 },
      a16: Algm[A16, A17] { type CC = C16 },
      a17: Algm[A17, A18] { type CC = C17 },
      a18: Algm[A18, A19] { type CC = C18 },
      a19: Algm[A19, A20] { type CC = C19 },
      a20: Algm[A20, A21] { type CC = C20 },
      a21: Algm[A21, A22] { type CC = C21 }
  ): Algm[A1, A22] {
    type CC = Conf.Then[
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
  } = Algm.mk(
    AlgmImpl.andThen(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg,
      a21.alg
    )
  )

  def and[A1, B1, A2, B2, A3, B3, C1 <: Conf, C2 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 }
  ): Algm[(A1, A2), (B1, B2)] { type CC = Conf.And[C1, C2] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, C1 <: Conf, C2 <: Conf, C3 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 }
  ): Algm[(A1, A2, A3), (B1, B2, B3)] { type CC = Conf.And[C1, Conf.And[C2, C3]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 }
  ): Algm[(A1, A2, A3, A4), (B1, B2, B3, B4)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, C4]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 }
  ): Algm[(A1, A2, A3, A4, A5), (B1, B2, B3, B4, B5)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, C5]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 }
  ): Algm[(A1, A2, A3, A4, A5, A6), (B1, B2, B3, B4, B5, B6)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, C6]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7), (B1, B2, B3, B4, B5, B6, B7)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, C7]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8), (B1, B2, B3, B4, B5, B6, B7, B8)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, C8]]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9), (B1, B2, B3, B4, B5, B6, B7, B8, B9)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, C9]]]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, C10]]]]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, C11]]]]]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12)] { type CC = Conf.And[C1, Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, C12]]]]]]]]]]] } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13)] {
    type CC = Conf.And[
      C1,
      Conf.And[C2, Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, C13]]]]]]]]]]]
    ]
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14)] {
    type CC = Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[C3, Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, C14]]]]]]]]]]]
      ]
    ]
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15)] {
    type CC = Conf.And[
      C1,
      Conf.And[
        C2,
        Conf.And[
          C3,
          Conf.And[C4, Conf.And[C5, Conf.And[C6, Conf.And[C7, Conf.And[C8, Conf.And[C9, Conf.And[C10, Conf.And[C11, Conf.And[C12, Conf.And[C13, Conf.And[C14, C15]]]]]]]]]]]
        ]
      ]
    ]
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16)] {
    type CC = Conf.And[
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
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 },
      a17: Algm[A17, B17] { type CC = C17 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17)] {
    type CC = Conf.And[
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
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 },
      a17: Algm[A17, B17] { type CC = C17 },
      a18: Algm[A18, B18] { type CC = C18 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18)] {
    type CC = Conf.And[
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
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, A20, B20, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 },
      a17: Algm[A17, B17] { type CC = C17 },
      a18: Algm[A18, B18] { type CC = C18 },
      a19: Algm[A19, B19] { type CC = C19 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19)] {
    type CC = Conf.And[
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
  } = Algm.mk(AlgmImpl.and(a1.alg, a2.alg, a3.alg, a4.alg, a5.alg, a6.alg, a7.alg, a8.alg, a9.alg, a10.alg, a11.alg, a12.alg, a13.alg, a14.alg, a15.alg, a16.alg, a17.alg, a18.alg, a19.alg))

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, A20, B20, A21, B21, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 },
      a17: Algm[A17, B17] { type CC = C17 },
      a18: Algm[A18, B18] { type CC = C18 },
      a19: Algm[A19, B19] { type CC = C19 },
      a20: Algm[A20, B20] { type CC = C20 }
  ): Algm[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20)] {
    type CC = Conf.And[
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
  } = Algm.mk(
    AlgmImpl.and(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg
    )
  )

  def and[A1, B1, A2, B2, A3, B3, A4, B4, A5, B5, A6, B6, A7, B7, A8, B8, A9, B9, A10, B10, A11, B11, A12, B12, A13, B13, A14, B14, A15, B15, A16, B16, A17, B17, A18, B18, A19, B19, A20, B20, A21, B21, A22, B22, C1 <: Conf, C2 <: Conf, C3 <: Conf, C4 <: Conf, C5 <: Conf, C6 <: Conf, C7 <: Conf, C8 <: Conf, C9 <: Conf, C10 <: Conf, C11 <: Conf, C12 <: Conf, C13 <: Conf, C14 <: Conf, C15 <: Conf, C16 <: Conf, C17 <: Conf, C18 <: Conf, C19 <: Conf, C20 <: Conf, C21 <: Conf](
      a1: Algm[A1, B1] { type CC = C1 },
      a2: Algm[A2, B2] { type CC = C2 },
      a3: Algm[A3, B3] { type CC = C3 },
      a4: Algm[A4, B4] { type CC = C4 },
      a5: Algm[A5, B5] { type CC = C5 },
      a6: Algm[A6, B6] { type CC = C6 },
      a7: Algm[A7, B7] { type CC = C7 },
      a8: Algm[A8, B8] { type CC = C8 },
      a9: Algm[A9, B9] { type CC = C9 },
      a10: Algm[A10, B10] { type CC = C10 },
      a11: Algm[A11, B11] { type CC = C11 },
      a12: Algm[A12, B12] { type CC = C12 },
      a13: Algm[A13, B13] { type CC = C13 },
      a14: Algm[A14, B14] { type CC = C14 },
      a15: Algm[A15, B15] { type CC = C15 },
      a16: Algm[A16, B16] { type CC = C16 },
      a17: Algm[A17, B17] { type CC = C17 },
      a18: Algm[A18, B18] { type CC = C18 },
      a19: Algm[A19, B19] { type CC = C19 },
      a20: Algm[A20, B20] { type CC = C20 },
      a21: Algm[A21, B21] { type CC = C21 }
  ): Algm[
    (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21),
    (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21)
  ] {
    type CC = Conf.And[
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
  } = Algm.mk(
    AlgmImpl.and(
      a1.alg,
      a2.alg,
      a3.alg,
      a4.alg,
      a5.alg,
      a6.alg,
      a7.alg,
      a8.alg,
      a9.alg,
      a10.alg,
      a11.alg,
      a12.alg,
      a13.alg,
      a14.alg,
      a15.alg,
      a16.alg,
      a17.alg,
      a18.alg,
      a19.alg,
      a20.alg,
      a21.alg
    )
  )

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
  } yield s"a$i: Algm[A, B] { type CC = C$i },"
  a.mkString("\n  ")
}

def tpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.Or[C$acc, ${tpe(i, acc + 1)}]"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i)
  } yield s"a$i.alg"

  s"): Algm[A, B] { type CC = ${tpe(i, 1)} } = Algm.mk(AlgmImpl.or(${a.mkString(", ")}))"
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
  } yield s"a$i: Algm[A$i, A${i + 1}] { type CC = C$i },"
  a.mkString("\n  ")
}

def tpe(i: Int, acc: Int): String = if (acc == i - 1) s"C$acc" else s"Conf.Then[C$acc, ${tpe(i, acc + 1)}]"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i)
  } yield s"a$i.alg"

  s"): Algm[A1, A$i] { type CC = ${tpe(i, 1)} } = Algm.mk(AlgmImpl.andThen(${a.mkString(", ")}))"
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
  "def or[A, B, " ++ a.mkString(", ") ++ "]("
}

def params(i: Int): String = {
  val a = for {
    i <- Range(1, i)
  } yield s"a$i: Algm[A, B] { type CC = C$i },"
  a.mkString("\n    ")
}

def tpe(i: Int, acc: Int): String = if (acc == i) s"C$acc" else s"Conf.Or[C$acc, ${tpe(i, acc + 1)}]"

def body(i: Int) = {

  val a = for {
    i <- Range(1, i)
  } yield s"a$i.alg"

  s"): Algm[A, B] { type CC = ${tpe(i, 1)} } = Algm.mk(AlgmImpl.or(${a.mkString(", ")}))"
}

def total(i: Int) =
s"""${header(i)}
    ${params(i)}
${body(i)}

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)

 */
