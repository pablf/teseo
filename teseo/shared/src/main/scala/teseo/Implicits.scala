package teseo

import scala.util.NotGiven
import teseo.internal.Optional.Found
import teseo.internal.Optional.NotFound
import teseo.internal.Optional
import teseo.internal.Error
import scala.Conversion
import teseo.Conf.Then

/**
 * Given instances for Conf class.
 */
inline given [E](using ev: E): Conf.Simple[E] = Conf.Simple[E](ev)

inline given [E](using ev: Optional[E]): Conf.Default[E] = Conf.Default[E](ev.summon)

inline given [C <: Conf, M[_ <: Conf]](using alg: M[C])(using c: C): Conf.Wrap[C, M] =
  Conf.Wrap(c, alg)

inline given [C1 <: Conf, C2 <: Conf](using conf1: C1, conf2: C2): Conf.And[C1, C2] =
  Conf.And[C1, C2](conf1, conf2)

inline given [C1 <: Conf, C2 <: Conf](using conf1: C1, conf2: C2): Conf.Then[C1, C2] =
  Conf.Then[C1, C2](conf1, conf2)

inline given [C1 <: Conf, C2 <: Conf](using conf1: C1, conf2: NotGiven[C2]): Conf.Or[C1, C2] =
  Conf.Or[C1, C2](Left(conf1))

inline given [C1 <: Conf, C2 <: Conf](using conf1: NotGiven[C1], conf2: C2): Conf.Or[C1, C2] =
  Conf.Or[C1, C2](Right(conf2))

inline given [C1 <: Conf, C2 <: Conf](using conf1: C1, conf2: C2): Conf.Or[C1, C2] =
  (conf1.hasNone, conf2.hasNone) match {
    case (true, false) => Conf.Or[C1, C2](Right(conf2))
    case _             => Conf.Or[C1, C2](Left(conf1))
  }

/**
 * Given instances for Optional class.
 */
inline given [E](using inline e: E): Optional.Found[E] = internal.Optional.Found[E](e)

inline given [E](using NotGiven[E]): Optional.NotFound[E] = internal.Optional.NotFound[E]()

/**
 * Given instances for Error class.
 */
inline given [E](using inline ev: NotGiven[Conf.Simple[E]]): Error[Conf.Simple[E]] = Error.error[E]

inline given [E](using inline ev: Conf.Simple[E]): Error[Conf.Simple[E]] = internal.Error.empty

inline given [E](using inline ev: Conf.Default[E]): Error[Conf.Default[E]] = internal.Error.empty

inline given [M[_ <: Conf]](using inline ev: NotGiven[Conf.Wrap[_, M]]): Error[Conf.Wrap[_, M]] =
  internal.Error.errorWrap[M]

inline given [M[_ <: Conf]](using inline ev: Conf.Wrap[_, M]): Error[Conf.Wrap[_, M]] =
  internal.Error.empty

inline given [C1 <: Conf, C2 <: Conf](using
    inline tree1: Error[C1],
    inline tree2: Error[C2]
): Error[Conf.And[C1, C2]] =
  internal.Error.and(tree1, tree2)

inline given [C1 <: Conf, C2 <: Conf](using
    inline tree1: Error[C1],
    inline tree2: Error[C2]
): Error[Conf.Then[C1, C2]] =
  internal.Error.andThen(tree1, tree2)

inline given [C1 <: Conf, C2 <: Conf](using
    inline tree1: Error[C1],
    inline tree2: Error[C2]
): Error[Conf.Or[C1, C2]] =
  internal.Error.or(tree1, tree2)

import scala.language.implicitConversions

/**
 * Homogenize input and output of `Algm`.
 */
given converter[A1, B1, A2 <: Tuple, B2 <: Tuple, C1 <: Conf, C2 <: Conf]: Conversion[
  AlgmImpl[(A1, A2), (B1, B2), Conf.And[C1, C2]],
  AlgmImpl.Custom[A1 *: A2, B1 *: B2, Conf.And[C1, C2]]
] with
  def apply(x: AlgmImpl[(A1, A2), (B1, B2), Conf.And[C1, C2]]): AlgmImpl.Custom[A1 *: A2, B1 *: B2, Conf.And[C1, C2]] =
    x match {

      case AlgmImpl.And(alg1, a @ AlgmImpl.And(alg2, alg3)) =>
        AlgmImpl.Custom[A1 *: A2, B1 *: B2, Conf.And[C1, C2]] { case (t, Conf.And(c1, c2)) =>
          alg1.ex(t.head)(using c1) *: EmptyTuple ++ a.ex(t.tail)(using c2)
        }

      case AlgmImpl.And(alg1, AlgmImpl.Custom(f)) =>
        AlgmImpl.Custom[A1 *: A2, B1 *: B2, Conf.And[C1, C2]] { case (t, Conf.And(c1, c2)) =>
          alg1.ex(t.head)(using c1) *: EmptyTuple ++ f(t.tail, c2)
        }

      case alg => alg
    }
