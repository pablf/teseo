package teseo.internal

import scala.quoted.*
import teseo.*

/**
 * Error
 */

private[teseo] case class Error[C <: Conf](needed: List[String], oneOf: List[List[String]], optional: List[String]) { self =>

  def addNeeded(obj: String): Error[C] = self.copy(needed = obj :: needed)

  def addOneOf(obj: List[String]): Error[C] = self.copy(oneOf = obj :: oneOf)

  def addOptional(obj: String): Error[C] = self.copy(optional = obj :: optional)

  def shouldThrow: Boolean = needed.nonEmpty | oneOf.nonEmpty | optional.nonEmpty

  def string =
    if (shouldThrow) List(getHeader, getNeeded, getOneOf, getOptional).filter(_ != "").mkString("\n")
    else "A fatal error ocurred."

  def getHeader = "Please, provide the following given/implicit Mechanisms to execute Algorithm:"

  def getNeeded = needed match {
    case Nil => ""
    case _   => needed.mkString("- ", ", ", ".")
  }

  def getOneOf = oneOf.filter(_.nonEmpty) match {
    case Nil      => ""
    case filtered => filtered.map(_.mkString("      - ", ", ", ".")).mkString("- An instance of one of the following:\n", "\n", "\n")
  }

  def getOptional = optional match {
    case Nil => ""
    case _   => optional.mkString("You can also provide, if desired, instances of classes:\n      - ", ", ", ".")
  }

  /*inline def and[C2 <: Conf](inline error2: Error[C2]): Error[Conf.And[C, C2]] = Error(
        self.needed ++ error2.needed,
        self.oneOf ++ error2.oneOf,
        self.optional ++ error2.optional
    )

    def andThen[C2 <: Conf](error2: Error[C2]): Error[Conf.Then[C, C2]] = Error(
        self.needed ++ error2.needed,
        self.oneOf ++ error2.oneOf,
        self.optional ++ error2.optional
    )

    def or[C2 <: Conf](error2: Error[C2]): Error[Conf.Or[C, C2]] = Error(
        self.needed ++ error2.needed,
        self.oneOf ++ error2.oneOf,
        self.optional ++ error2.optional
    )*/
}

object Error {

  transparent inline def triggerError[A, C <: Conf](inline err: Error[C]): A = ${ ErrorMacro.triggerErrorMacro[A, C]('err) }
  transparent inline def compare[C <: Conf](inline err: Error[C]): Boolean   = ${ ErrorMacro.compareMacro[C]('err) }
  transparent inline def error[E]: Error[Conf.Simple[E]]                     = ${ ErrorMacro.errorMacro[E]() }
  transparent inline def errorWrap[M[_ <: Conf]]: Error[Conf.Wrap[_, M]]     = ${ ErrorMacro.errorWrapMacro[M]() }

  transparent inline def and[C1 <: Conf, C2 <: Conf](inline error1: Error[C1], inline error2: Error[C2]): Error[Conf.And[C1, C2]]      = ${ ErrorMacro.and('error1, 'error2) }
  transparent inline def andThen[C1 <: Conf, C2 <: Conf](inline error1: Error[C1], inline error2: Error[C2]): Error[Conf.Then[C1, C2]] = ${ ErrorMacro.andThen('error1, 'error2) }
  transparent inline def or[C1 <: Conf, C2 <: Conf](inline error1: Error[C1], inline error2: Error[C2]): Error[Conf.Or[C1, C2]]        = ${ ErrorMacro.or('error1, 'error2) }

  transparent inline def empty[C <: Conf] = Error[C](Nil, Nil, Nil)

}
