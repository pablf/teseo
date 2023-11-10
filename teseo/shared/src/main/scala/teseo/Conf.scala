package teseo

/**
 * Trait `Conf` represents the requisites needed for execution of an algorithm. They are generated at compile-time.
 */
sealed trait Conf {
  def hasNone: Boolean
}

object Conf {

  case class Simple[+E](e: E) extends Conf { self =>

    override def hasNone: Boolean = false

  }

  case class Default[+E](e: Option[E]) extends Conf { self =>

    def find: Option[E] = e

    override def hasNone: Boolean = e.map(_ => false).getOrElse(true)

  }

  case class Wrap[C <: Conf, M[_ <: Conf]](c: C, alg: M[C]) extends Conf { self =>

    override def hasNone: Boolean = true

  }

  case class And[+C1 <: Conf, +C2 <: Conf](a1: C1, a2: C2) extends Conf {

    override def hasNone: Boolean = a1.hasNone | a2.hasNone

  }

  case class Then[+C1 <: Conf, +C2 <: Conf](a1: C1, a2: C2) extends Conf {

    override def hasNone: Boolean = a1.hasNone | a2.hasNone

  }

  case class Or[+C1 <: Conf, +C2 <: Conf](e: Either[C1, C2]) extends Conf {

    def find: Either[C1, C2] = e

    override def hasNone: Boolean = e match {
      case Left(value)  => value.hasNone
      case Right(value) => value.hasNone
    }

  }
}
