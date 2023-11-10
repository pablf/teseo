package teseo

import scala.reflect.ClassTag

/**
 * `Container` is a wrapper for an algorithm whose type is decided in a macro.
 */
trait Container[Final[_ <: Conf]] {
  type C <: Conf

  given alg: Final[C]
}

object Container {
  def apply[Final[_ <: Conf], CC <: Conf](f: Final[CC]) = new Container[Final] {
    type C = CC

    given alg: Final[C] = f
  }
}
