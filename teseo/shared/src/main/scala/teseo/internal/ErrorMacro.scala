package teseo.internal

import scala.quoted.*
import scala.compiletime.error
import teseo.*

private[internal] object ErrorMacro {

  import scala.quoted.FromExpr.{ListFromExpr, StringFromExpr}

  given [C <: Conf: Type]: FromExpr[Error[C]] with
    def unapply(x: Expr[Error[C]])(using Quotes): Option[Error[C]] =
      x match {
        case '{ internal.Error.apply(${ Expr(needed) }, ${ Expr(oneOf) }, ${ Expr(optional) }) } => Some(Error(needed, oneOf, optional))
        case '{ internal.Error.apply(${ needed }, ${ oneOf }, ${ optional }) }                   =>
          Some(Error(ListFromExpr[String].unapply(needed).get, ListFromExpr[List[String]].unapply(oneOf).get, ListFromExpr[String].unapply(optional).get))
        case '{ Error(${ Expr(needed) }, ${ Expr(oneOf) }, ${ Expr(optional) }) }                => Some(Error(needed, oneOf, optional))
        case '{ new Error(${ Expr(needed) }, ${ Expr(oneOf) }, ${ Expr(optional) }) }            => Some(Error(needed, oneOf, optional))
        case _                                                                                   => None
      }

  given [C <: Conf: Type]: ToExpr[Error[C]] with
    def apply(x: Error[C])(using Quotes): Expr[Error[C]] =
      x match {
        case Error(needed, oneOf, optional) => '{ Error[C](${ Expr(needed) }, ${ Expr(oneOf) }, ${ Expr(optional) }) }
      }

  def and[C1 <: Conf: Type, C2 <: Conf: Type](error1: Expr[Error[C1]], error2: Expr[Error[C2]])(using Quotes): Expr[Error[Conf.And[C1, C2]]] =
    (error1.valueOrAbort, error2.valueOrAbort) match {
      case (Error(a1, b1, c1), Error(a2, b2, c2)) =>
        val needed   = a1 ++ a2
        val oneOf    = b1 ++ b2
        val optional = c1 ++ c2

        '{ Error(${ Expr(needed.distinct) }, ${ Expr(oneOf.distinct) }, ${ Expr(optional.distinct) }) }
    }

  def andThen[C1 <: Conf: Type, C2 <: Conf: Type](error1: Expr[Error[C1]], error2: Expr[Error[C2]])(using Quotes): Expr[Error[Conf.Then[C1, C2]]] =
    (error1.valueOrAbort, error2.valueOrAbort) match {
      case (Error(a1, b1, c1), Error(a2, b2, c2)) =>
        val needed   = a1 ++ a2
        val oneOf    = b1 ++ b2
        val optional = c1 ++ c2

        '{ Error(${ Expr(needed.distinct) }, ${ Expr(oneOf.distinct) }, ${ Expr(optional.distinct) }) }
    }

  def or[C1 <: Conf: Type, C2 <: Conf: Type](error1: Expr[Error[C1]], error2: Expr[Error[C2]])(using Quotes): Expr[Error[Conf.Or[C1, C2]]] =
    (error1.valueOrAbort, error2.valueOrAbort) match {
      case (Error(a1, b1, c1), Error(a2, b2, c2)) =>
        val oneOf1   = (b1.isEmpty, a1.isEmpty) match {
          case (true, true)   => List()
          case (true, false)  => List(a1)
          case (false, true)  => b1
          case (false, false) => b1.map(x => a1 ++ x)
        }
        val oneOf2   = (b2.isEmpty, a2.isEmpty) match {
          case (true, true)   => List()
          case (true, false)  => List(a2)
          case (false, true)  => b2
          case (false, false) => b2.map(x => a2 ++ x)
        }
        val oneOf    = if (oneOf1.isEmpty || oneOf2.isEmpty) Nil else oneOf1 ++ oneOf2
        val optional = c1 ++ c2

        val needed = oneOf.flatten.distinct.filterNot { x =>
          oneOf.map(_.contains(x)).contains(false)
        }

        val oneOfFiltered = oneOf.filterNot(_.map(needed.contains).contains(true))

        '{ Error(${ Expr(needed.distinct) }, ${ Expr(oneOfFiltered.distinct) }, ${ Expr(optional.distinct) }) }
    }

  def compareMacro[C <: Conf: Type](err: Expr[Error[C]])(using Quotes): Expr[Boolean] = {

    import quotes.reflect.*

    err.value match {
      case Some(value) => Expr(value == internal.Error.empty)
      case None        => report.errorAndAbort(err.show)
    }

  }

  def triggerErrorMacro[A, C <: Conf: Type](err: Expr[Error[C]])(using Quotes): Expr[A] = {

    import quotes.reflect.*

    val er = err.value
    report.errorAndAbort(er.map(_.string).getOrElse("Impossible to read error"))

  }

  def errorMacro[E: Type]()(using Quotes): Expr[Error[Conf.Simple[E]]] = {
    import quotes.reflect.*
    val tpe  = TypeRepr.of[E]
    val name = tpe.show
    val err  = Error[Conf.Simple[E]](List(name), Nil, Nil)
    Expr(err)
  }

  def errorWrapMacro[M[_ <: Conf]: Type]()(using Quotes): Expr[Error[Conf.Wrap[_, M]]] = {
    import quotes.reflect.*
    val tpe  = TypeRepr.of[M].classSymbol.map(_.fullName)
    val name = tpe.getOrElse("Unknown type")
    val err  = Error[Conf.Wrap[_, M]](List(name), Nil, Nil)
    Expr(err)
  }

}
