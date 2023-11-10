package teseo.internal

import scala.quoted.*
import scala.annotation.*
import teseo.*

private[teseo] object AlgmGenMacro {

  /**
   * `Mode` describes how are algorithms linked:
   *   - `Pipeline` for `AlgmImpl.Then`
   *   - `Zip` for `AlgmImpl.And`
   *   - `Alternatives` for `AlgmImpl.Or`
   */
  enum Mode:
    case Pipeline, Zip, Alternatives

  given FromExpr[Mode] with
    def unapply(x: Expr[Mode])(using Quotes): Option[Mode] =
      x.show match {
        case "teseo.internal.AlgmGenMacro.Mode.Pipeline"     => Some(Mode.Pipeline)
        case "teseo.internal.AlgmGenMacro.Mode.Zip"          => Some(Mode.Zip)
        case "teseo.internal.AlgmGenMacro.Mode.Alternatives" => Some(Mode.Alternatives)
        case _                                               => None
      }

  /**
   * Obtains the appropriate information from type parameters
   *   - type `T` extending `Algm[A, B] { type CC = C}` returns `List(TypeRepr.of[T], TypeRepr.of[A], TypeRepr.of[B])`
   *   - type `T[C <: Conf]` extending `Algm[A, B] { type CC = C}` returns `List(TypeRepr.of[T], TypeRepr.of[C], TypeRepr.of[A], TypeRepr.of[B])`
   */
  private def process(using Quotes)(t: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {

    import quotes.reflect.*

    t match {
      // `Algm` of type `A[_ <: Conf]`
      case tpe @ TypeLambda(params, bounds, body) =>
        if (tpe.derivesFrom(TypeRepr.of[Algm[?, ?]].typeSymbol)) {
          val baseClass = tpe.baseType(TypeRepr.of[Algm[?, ?]].typeSymbol)

          baseClass match {
            case AppliedType(algm, in :: out :: Nil) =>
              List(tpe, in, out)
            case _                                   =>
              report.errorAndAbort(s"This should not happen...2")
          }

        } else report.errorAndAbort(s"Type ${tpe.show} doesn't extend trait Algm")

      // `Algm` of type `A` with fixed `Conf`
      case tpe: TypeRef                           =>
        if (tpe.derivesFrom(TypeRepr.of[Algm[?, ?]].typeSymbol)) {
          val baseClass = tpe.baseType(TypeRepr.of[Algm[?, ?]].typeSymbol)

          var conf: Option[TypeRepr] = None
          try
            conf = Some(baseClass.memberType(Symbol.classSymbol("CC")))
          catch {
            case _ => report.errorAndAbort(s"Type ${tpe.name} doesn't have a member type CC")
          }

          baseClass match {
            case AppliedType(algm, in :: out :: Nil) =>
              List(tpe, conf.get, in, out)
            case _                                   =>
              report.errorAndAbort(s"This should not happen...2")
          }

        } else report.errorAndAbort(s"Type ${tpe.name} doesn't extend trait Algm")

      // Invalid cases
      case _                                      => quotes.reflect.report.errorAndAbort("Non-supported type")
    }

  }

  /**
   * Assembles a `Container` from a list of `List[TypeRepr]` gathering the algevant information about a type.
   */
  private def assemble[TFinal[_ <: Conf] <: Algm[?, ?]: Type](using Quotes)(tpesList: List[List[quotes.reflect.TypeRepr]], mode: Expr[Mode]): Expr[Container[TFinal]] = {

    import quotes.reflect.*

    sealed trait AlgPattern {
      val isConfigured: Boolean
      val a: TypeRepr
      val b: TypeRepr
    }

    case class ConfiguredPattern(tpe: TypeRepr, conf: TypeRepr, a: TypeRepr, b: TypeRepr) extends AlgPattern {
      override val isConfigured: Boolean = true
    }

    case class OpenPattern(tpe: TypeRepr, a: TypeRepr, b: TypeRepr) extends AlgPattern {
      override val isConfigured: Boolean = false
    }

    def transform(tpe: List[TypeRepr]): AlgPattern =
      tpe match {
        case t1 :: t2 :: t3 :: Nil       => OpenPattern(t1, t2, t3)
        case t1 :: t2 :: t3 :: t4 :: Nil => ConfiguredPattern(t1, t2, t3, t4)
        case _                           => report.errorAndAbort("This should not happen")
      }

    def extractor(tpe: TypeRepr): TypeRepr =
      tpe match {
        case AppliedType(p, _) => p
        case _                 => report.errorAndAbort("This should not happen")
      }

    val tpes = tpesList.map(transform)

    val mde = mode.value.getOrElse(report.errorAndAbort(mode.show))

    // Makes a list of the types appearing in the
    val confTpe: TypeRepr = tpes
      .map {
        case ConfiguredPattern(_, cTpe, _, _) => cTpe
        case OpenPattern(oTpe, _, _)          =>
          AppliedType(
            extractor(TypeRepr.of[Conf.Wrap[_ <: Conf, _]]),
            List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Conf]), oTpe)
          )
      }
      .foldRight[Option[TypeRepr]](None) {
        case (t, Some(o)) =>
          mde match
            case Mode.Pipeline     =>
              Some(
                AppliedType(
                  extractor(TypeRepr.of[Conf.Then[_, _]]),
                  List(t, o)
                )
              )
            case Mode.Zip          =>
              Some(
                AppliedType(
                  extractor(TypeRepr.of[Conf.And[_, _]]),
                  List(t, o)
                )
              )
            case Mode.Alternatives =>
              Some(
                AppliedType(
                  extractor(TypeRepr.of[Conf.Or[_, _]]),
                  List(t, o)
                )
              )

        case (t, None) => Some(t)
      }
      .get

    val algmImpl1 = tpes
      .map[(Expr[AlgmImpl[_, _, _ <: Conf]], Type[? <: AnyKind], Type[? <: AnyKind], Type[? <: AnyKind])] {

        case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) =>
          (tpe.asType, cTpe.asType, aTpe.asType, bTpe.asType) match {
            case ('[t], '[c], '[a], '[b]) =>
              (
                '{
                  type c <: Conf
                  type t <: Algm[a, b] { type CC = c }
                  AlgmImpl.SingleWrap[a, b, c, t]()
                },
                Type.of[c],
                aTpe.asType,
                bTpe.asType
              )
            case _                        => report.errorAndAbort("eh")
          }

        case OpenPattern(oTpe, aTpe, bTpe) =>
          (aTpe.asType, aTpe.asType, bTpe.asType) match {
            case ('[t], '[a], '[b]) =>
              (
                '{
                  type t <: [_ <: Conf] =>> Algm[a, b]
                  AlgmImpl.Wrap[a, b, t]()
                },
                Type.of[t],
                aTpe.asType,
                bTpe.asType
              )
            case _                  => report.errorAndAbort("eh")
          }

      }

    val algmImpl = algmImpl1 match
      case standalone :: Nil => standalone._1
      case _                 =>
        mde match
          case Mode.Zip =>
            val dir      = TypeRepr.of[AlgmImpl].typeSymbol.companionModule
            val zipTpes1 = tpes.flatMap(pattern => List(pattern.a, pattern.b))
            val zipTpes2 = tpes.map {
              case ConfiguredPattern(tpe, c, a, b) =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.And[_, _]]),
                  List(
                    c,
                    AppliedType(
                      extractor(TypeRepr.of[Conf.Simple[_]]),
                      tpe :: Nil
                    )
                  )
                )
              case OpenPattern(tpe, a, b)          =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.Wrap[_ <: Conf, _]]),
                  List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Conf]), tpe)
                )
            }
            val zipTpes  = zipTpes1 ++ zipTpes2
            val args     = algmImpl1.zip(zipTpes2).map { case ((alg, _, a, b), c) =>
              val t = AppliedType(
                extractor(TypeRepr.of[AlgmImpl[_, _, _]]),
                TypeRepr.of(using a) :: TypeRepr.of(using b) :: c :: Nil
              )
              Typed(
                alg.asTerm,
                TypeTree.of(using t.asType)
              )
            }
            Select.overloaded(Ref(dir), "and", zipTpes, args).asExprOf[AlgmImpl[?, ?, ? <: Conf]]

          case Mode.Pipeline =>
            val dir      = TypeRepr.of[AlgmImpl].typeSymbol.companionModule
            val zipTpes1 = tpes.map(pattern => pattern.a) ++ List(tpes.last.b)
            val zipTpes2 = tpes.map {
              case ConfiguredPattern(tpe, c, a, b) =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.And[_, _]]),
                  List(
                    c,
                    AppliedType(
                      extractor(TypeRepr.of[Conf.Simple[_]]),
                      tpe :: Nil
                    )
                  )
                )
              case OpenPattern(tpe, a, b)          =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.Wrap[_ <: Conf, _]]),
                  List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Conf]), tpe)
                )
            }
            val zipTpes  = zipTpes1 ++ zipTpes2
            val args     = algmImpl1.zip(zipTpes2).map { case ((alg, _, a, b), c) =>
              val t = AppliedType(
                extractor(TypeRepr.of[AlgmImpl[_, _, _]]),
                TypeRepr.of(using a) :: TypeRepr.of(using b) :: c :: Nil
              )
              Typed(
                alg.asTerm,
                TypeTree.of(using t.asType)
              )
            }
            Select.overloaded(Ref(dir), "andThen", zipTpes, args).asExprOf[AlgmImpl[?, ?, ? <: Conf]]

          case Mode.Alternatives =>
            val dir      = TypeRepr.of[AlgmImpl].typeSymbol.companionModule
            val zipTpes1 = tpes.head.a :: tpes.head.b :: Nil
            val zipTpes2 = tpes.map {
              case ConfiguredPattern(tpe, c, a, b) =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.And[_, _]]),
                  List(
                    c,
                    AppliedType(
                      extractor(TypeRepr.of[Conf.Simple[_]]),
                      tpe :: Nil
                    )
                  )
                )
              case OpenPattern(tpe, a, b)          =>
                AppliedType(
                  extractor(TypeRepr.of[Conf.Wrap[_ <: Conf, _]]),
                  List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Conf]), tpe)
                )
            }
            val zipTpes  = zipTpes1 ++ zipTpes2
            val args     = algmImpl1.zip(zipTpes2).map { case ((alg, _, a, b), c) =>
              val t = AppliedType(
                extractor(TypeRepr.of[AlgmImpl[_, _, _]]),
                TypeRepr.of(using a) :: TypeRepr.of(using b) :: c :: Nil
              )
              Typed(
                alg.asTerm,
                TypeTree.of(using t.asType)
              )
            }
            Select.overloaded(Ref(dir), "or", zipTpes, args).asExprOf[AlgmImpl[?, ?, ? <: Conf]]

    val aTpe = mde match
      case Mode.Pipeline     =>
        tpes.head match {
          case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => aTpe
          case OpenPattern(oTpe, aTpe, bTpe)            => aTpe
        }
      case Mode.Zip          =>
        tpes match
          case standalone :: Nil => standalone.a
          case _                 =>
            tpes
              .map {
                case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => aTpe
                case OpenPattern(oTpe, aTpe, bTpe)            => aTpe
              }
              .foldRight(TypeRepr.of[EmptyTuple]) { case (t, tuple) =>
                AppliedType(
                  extractor(TypeRepr.of[*:[_, _]]),
                  List(t, tuple)
                )
              }
      case Mode.Alternatives =>
        tpes.head match {
          case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => aTpe
          case OpenPattern(oTpe, aTpe, bTpe)            => aTpe
        }

    val bTpe = mde match
      case Mode.Pipeline     =>
        tpes.last match {
          case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => bTpe
          case OpenPattern(oTpe, aTpe, bTpe)            => bTpe
        }
      case Mode.Zip          =>
        tpes match
          case standalone :: Nil => standalone.a
          case _                 =>
            tpes
              .map {
                case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => bTpe
                case OpenPattern(oTpe, aTpe, bTpe)            => bTpe
              }
              .foldRight(TypeRepr.of[EmptyTuple]) { case (t, tuple) =>
                AppliedType(
                  extractor(TypeRepr.of[*:[_, _]]),
                  List(t, tuple)
                )
              }
      case Mode.Alternatives =>
        tpes.last match {
          case ConfiguredPattern(tpe, cTpe, aTpe, bTpe) => bTpe
          case OpenPattern(oTpe, aTpe, bTpe)            => bTpe
        }

    val tpeRepr            = AppliedType(extractor(TypeRepr.of[TFinal[Conf.Simple[_]]]), List(confTpe))
    val tpeTree            = TypeTree.of(using tpeRepr.asType)
    val name: String       = tpeRepr.typeSymbol.name
    val configName: String = if name == "C" then "A" else "C"
    val companion          = TypeRepr.of[TFinal].typeSymbol.companionModule

    val companionSample =
      s"""


object $name {
    case class ${name}Impl[$configName <: Conf](alg: AlgmImpl[${aTpe.typeSymbol.name}, ${bTpe.typeSymbol.name}, $configName]) extends $name[$configName]
}
            """

    if (!companion.exists) report.errorAndAbort(s"A companion object for class ${name} is needed. Consider using the following one: $companionSample")

    val constructor = companion.declaredType(name ++ "Impl") match {
      case h :: Nil => h
      case _        => report.errorAndAbort(s"A case class extending the desired algorithm is needed within the companion object. Consider using the following one:$companionSample")
    }

    val instance =
      Typed(
        Apply(
          TypeApply(
            Select(
              New(TypeIdent(constructor)),
              constructor.primaryConstructor
            ),
            List(TypeTree.of(using confTpe.asType))
          ),
          Typed(
            algmImpl.asTerm,
            TypeTree.of(using AppliedType(extractor(TypeRepr.of[AlgmImpl[Int, Int, Conf.Simple[Int]]]), List(aTpe, bTpe, confTpe)).asType)
          ) :: Nil
        ),
        tpeTree
      )

    val container = TypeRepr.of[Container].typeSymbol.companionModule

    val fun = Select.unique(Ref(container), "apply")

    Apply(
      TypeApply(
        fun,
        List(TypeTree.of[TFinal], TypeTree.of(using confTpe.asType))
      ),
      List(instance)
    ).asExprOf[Container[TFinal]]

  }

  object Route {

    def routeMacro1[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro2[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*
      val list = List(TypeRepr.of[T1], TypeRepr.of[T2])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro3[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro4[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro5[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4], TypeRepr.of[T5])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro6[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4], TypeRepr.of[T5], TypeRepr.of[T6])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro7[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4], TypeRepr.of[T5], TypeRepr.of[T6], TypeRepr.of[T7])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro8[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4], TypeRepr.of[T5], TypeRepr.of[T6], TypeRepr.of[T7], TypeRepr.of[T8])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro9[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(TypeRepr.of[T1], TypeRepr.of[T2], TypeRepr.of[T3], TypeRepr.of[T4], TypeRepr.of[T5], TypeRepr.of[T6], TypeRepr.of[T7], TypeRepr.of[T8], TypeRepr.of[T9])
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro10[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro11[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro12[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro13[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro14[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro15[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro16[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type, T16 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15],
        TypeRepr.of[T16]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro17[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type, T16 <: AnyKind: Type, T17 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15],
        TypeRepr.of[T16],
        TypeRepr.of[T17]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro18[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type, T16 <: AnyKind: Type, T17 <: AnyKind: Type, T18 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15],
        TypeRepr.of[T16],
        TypeRepr.of[T17],
        TypeRepr.of[T18]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro19[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type, T16 <: AnyKind: Type, T17 <: AnyKind: Type, T18 <: AnyKind: Type, T19 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15],
        TypeRepr.of[T16],
        TypeRepr.of[T17],
        TypeRepr.of[T18],
        TypeRepr.of[T19]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

    def routeMacro20[TFinal[_ <: Conf] <: Algm[?, ?]: Type, T1 <: AnyKind: Type, T2 <: AnyKind: Type, T3 <: AnyKind: Type, T4 <: AnyKind: Type, T5 <: AnyKind: Type, T6 <: AnyKind: Type, T7 <: AnyKind: Type, T8 <: AnyKind: Type, T9 <: AnyKind: Type, T10 <: AnyKind: Type, T11 <: AnyKind: Type, T12 <: AnyKind: Type, T13 <: AnyKind: Type, T14 <: AnyKind: Type, T15 <: AnyKind: Type, T16 <: AnyKind: Type, T17 <: AnyKind: Type, T18 <: AnyKind: Type, T19 <: AnyKind: Type, T20 <: AnyKind: Type](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {

      import quotes.reflect.*

      val list = List(
        TypeRepr.of[T1],
        TypeRepr.of[T2],
        TypeRepr.of[T3],
        TypeRepr.of[T4],
        TypeRepr.of[T5],
        TypeRepr.of[T6],
        TypeRepr.of[T7],
        TypeRepr.of[T8],
        TypeRepr.of[T9],
        TypeRepr.of[T10],
        TypeRepr.of[T11],
        TypeRepr.of[T12],
        TypeRepr.of[T13],
        TypeRepr.of[T14],
        TypeRepr.of[T15],
        TypeRepr.of[T16],
        TypeRepr.of[T17],
        TypeRepr.of[T18],
        TypeRepr.of[T19],
        TypeRepr.of[T20]
      )
      val tpes = list.map(process)
      assemble[TFinal](tpes, mode)
    }

  }

}

//CODE GENERATION
/*

def header(i: Int) = s"transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], "

def fstBody(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"T$i <: AnyKind"
  a.mkString(", ")
}

def sndBody(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"T$i"
  a.mkString(", ")
}

val u = "$"

def total(i: Int) =
s"""@targetName("pipeline${i-1}")
${header(i) ++ fstBody(i)}]: Container[TFinal] = $u{ Route.routeMacro2[TFinal, ${sndBody(i)}]('{Mode.Pipeline}) }

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)
 */

/*
def header(i: Int) = s"def routeMacro${i-1}[TFinal[_ <: Conf] <: Algm[?, ?] : Type, "

def fstBody(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"T$i <: AnyKind : Type"
  a.mkString(", ")
}

def sndBody(i: Int) = {
  val a = for {
    i <- Range(1, i)
  } yield s"TypeRepr.of[T$i]"
  a.mkString(", ")
}



def total(i: Int) =
s"""${header(i) ++ fstBody(i) ++ "](mode: Expr[Mode])(using Quotes): Expr[Container[TFinal]] = {"}

  	import quotes.reflect.*

  	val list = List(${sndBody(i)})
  	val tpes = list.map(process)
  	assemble[TFinal](tpes, mode)
}

"""

var acc = ""

for {
  i <- 2 to 21

} yield acc = acc ++ total(i)

println(acc)

 */
