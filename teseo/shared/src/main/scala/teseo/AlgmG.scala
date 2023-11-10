package teseo

import teseo.internal.*
import scala.annotation.targetName

/**
 * `AlgmG` is a generic version of `Algm`. WIP
 */
trait AlgmG[Bound, TC[_ <: Bound], A[_ <: Bound], B[_ <: Bound]] {

  type CC <: Conf

  type Self = AlgmG[Bound, TC, A, B]

  val alg: [D <: Bound] => TC[D] => AlgmImpl[A[D], B[D], CC]

  def doc: Doc = Doc()

  def accessDoc[A](f: Doc => A): A = f(doc)

  /**
   * Executes the underlying algorithm.
   */
  inline def ex[D <: Bound: TC](
      a: A[D]
  )(using inline error: Error[CC], config: Optional[CC]): B[D] = {
    val conf: CC =
      inline if Error.compare(error) then config.summon.get else Error.triggerError(error)
    alg[D](summon[TC[D]]).ex(a)(using conf)
  }

  /**
   * Creates a compiler message with information about the algorithm.
   *
   * @param warn
   *   `true` creates a warning, `false` creates info.
   */
  inline def explain(warn: Boolean = false): String = ${
    ExplainMacro.explain[A, B, CC, Self]('warn, '{ doc.toString() })
  }

}

object AlgmG {

  inline def apply[Bound, TC[_ <: Bound], A[_ <: Bound], B[_ <: Bound], C <: Conf](
      inline algImpl: [D <: Bound] => TC[D] => AlgmImpl[A[D], B[D], C]
  ): AlgmG[Bound, TC, A, B] { type CC = C } = new AlgmG[Bound, TC, A, B] {

    type CC = C

    override val alg = algImpl

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
  def mk[Bound, TC[_ <: Bound], A[_ <: Bound], B[_ <: Bound], E](
      f: [D <: Bound] => (E, TC[D]) => (A[D] => B[D])
  )(using e: E): AlgmG[Bound, TC, A, B] { type CC = Conf.Default[E] } = {

    type CC = Conf.Default[E]

    val alg: [D <: Bound] => TC[D] => AlgmImpl[A[D], B[D], CC] = [D <: Bound] => (tc: TC[D]) => AlgmImpl.default(e => f[D](e, tc))(using e)

    AlgmG(alg)

  }

  /**
   * Constructs an algorithm executing `f`.
   *
   * @param f
   *   Function applied by the algorithm.
   */
  def const[Bound, TC[_ <: Bound], A[_ <: Bound], B[_ <: Bound]](
      f: [D <: Bound] => TC[D] => (A[D] => B[D])
  ): AlgmG[Bound, TC, A, B] { type CC = Conf.Default[Option[Nothing]] } = {

    type CC = Conf.Default[Option[Nothing]]

    val alg: [D <: Bound] => TC[D] => AlgmImpl[A[D], B[D], CC] = [D <: Bound] => (tc: TC[D]) => AlgmImpl.const(f[D](tc))

    AlgmG(alg)

  }

}
