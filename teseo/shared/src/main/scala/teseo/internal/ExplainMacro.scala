package teseo.internal

import scala.quoted.*
import scala.compiletime.error
import teseo.*

private[teseo] object ExplainMacro {

  // TODO create annotation with similar functionality
  def explain[A <: AnyKind: Type, B <: AnyKind: Type, CC: Type, Self: Type](warn: Expr[Boolean], doc: Expr[String])(using Quotes): Expr[String] = {

    import quotes.reflect.*

    val tpeInfo = TypeRepr.of[A].classSymbol.map(_.name).getOrElse("UnknownType") match {
      case "Algm" => s"It's a generic algorithm."
      case n      => s"It represents a instance of $n."
    }

    val aInfo = TypeRepr.of[A].classSymbol.map(_.name).getOrElse("UnknownType")
    val bInfo = TypeRepr.of[B].classSymbol.map(_.name).getOrElse("UnknownType")

    val msg =
      s"""This algorithm instance transforms a value of type $aInfo into a value of type $bInfo.
      $tpeInfo
      It needs to a use ${TypeRepr.of[CC].toString()}.
      """

    val finalMsg = doc.value.map(_.++("\n")).getOrElse("") ++ msg

    warn.value match {
      case Some(false) => report.info(finalMsg)
      case _           => report.warning(finalMsg)
    }

    Expr(finalMsg)

  }

}
