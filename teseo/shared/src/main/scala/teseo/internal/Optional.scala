package teseo.internal

import scala.util.NotGiven

/**
 * `Optional` allows using `if else` construction with the existence of `given` instances.
 */

sealed private[teseo] trait Optional[E]:
  def summon: Option[E]

object Optional {

  case class Found[E](e: E) extends Optional[E]:
    override def summon: Option[E] = Some(e)

  case class NotFound[E]() extends Optional[E]:
    override def summon: Option[E] = None

  inline def summon[E](inline opt: Optional[E]): Option[E] =
    inline opt match {
      case NotFound() => None
      case Found(e)   => Some(e)
    }

}
