package teseo

import scala.annotation.targetName

/**
 * Contains generator of `Container`. There are three types: `pipeline`, `alternatives` and `zip`. The number of type arguments is variable.
 *
 * `AlgmGen.pipeline[T, T1, ..., Tn]` creates a `Container[T]` whose `alg` is a linear chaining of `T1`, `T2`, ..., `Tn`. `AlgmGen.alternatives[T, T1, ..., Tn]` creates a `Container[T]` whose `alg` executes `Ti` if available as given and fallbacks to `T{i+1}` in other case. `AlgmGen.zip[T, T1, ..., Tn]` creates a `Container[T]` whose `alg` zips the input and output of the `Ti`.
 *
 * The types of `T` and `Ti` must be adequate for the method choosen.
 */
object AlgmGen {

  import teseo.internal.AlgmGenMacro.*

  @targetName("pipeline1")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro1[TFinal, T1]('{ Mode.Pipeline }) }

  @targetName("pipeline2")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro2[TFinal, T1, T2]('{ Mode.Pipeline }) }

  @targetName("pipeline3")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro3[TFinal, T1, T2, T3]('{ Mode.Pipeline }) }

  @targetName("pipeline4")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro4[TFinal, T1, T2, T3, T4]('{ Mode.Pipeline }) }

  @targetName("pipeline5")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro5[TFinal, T1, T2, T3, T4, T5]('{ Mode.Pipeline }) }

  @targetName("pipeline6")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro6[TFinal, T1, T2, T3, T4, T5, T6]('{ Mode.Pipeline }) }

  @targetName("pipeline7")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro7[TFinal, T1, T2, T3, T4, T5, T6, T7]('{ Mode.Pipeline }) }

  @targetName("pipeline8")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro8[TFinal, T1, T2, T3, T4, T5, T6, T7, T8]('{ Mode.Pipeline }) }

  @targetName("pipeline9")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro9[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9]('{ Mode.Pipeline }) }

  @targetName("pipeline10")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro10[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]('{ Mode.Pipeline }) }

  @targetName("pipeline11")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro11[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]('{ Mode.Pipeline }) }

  @targetName("pipeline12")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro12[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]('{ Mode.Pipeline }) }

  @targetName("pipeline13")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro13[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]('{ Mode.Pipeline }) }

  @targetName("pipeline14")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro14[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]('{ Mode.Pipeline }) }

  @targetName("pipeline15")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro15[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]('{ Mode.Pipeline }) }

  @targetName("pipeline16")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro16[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]('{ Mode.Pipeline }) }

  @targetName("pipeline17")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro17[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]('{ Mode.Pipeline }) }

  @targetName("pipeline18")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro18[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]('{ Mode.Pipeline }) }

  @targetName("pipeline19")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro19[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]('{ Mode.Pipeline }) }

  @targetName("pipeline20")
  transparent inline def pipeline[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind, T20 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro20[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]('{ Mode.Pipeline }) }

  @targetName("zip1")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro1[TFinal, T1]('{ Mode.Zip }) }

  @targetName("zip2")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro2[TFinal, T1, T2]('{ Mode.Zip }) }

  @targetName("zip3")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro3[TFinal, T1, T2, T3]('{ Mode.Zip }) }

  @targetName("zip4")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro4[TFinal, T1, T2, T3, T4]('{ Mode.Zip }) }

  @targetName("zip5")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro5[TFinal, T1, T2, T3, T4, T5]('{ Mode.Zip }) }

  @targetName("zip6")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro6[TFinal, T1, T2, T3, T4, T5, T6]('{ Mode.Zip }) }

  @targetName("zip7")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro7[TFinal, T1, T2, T3, T4, T5, T6, T7]('{ Mode.Zip }) }

  @targetName("zip8")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro8[TFinal, T1, T2, T3, T4, T5, T6, T7, T8]('{ Mode.Zip }) }

  @targetName("zip9")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro9[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9]('{ Mode.Zip }) }

  @targetName("zip10")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro10[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]('{ Mode.Zip }) }

  @targetName("zip11")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro11[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]('{ Mode.Zip }) }

  @targetName("zip12")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro12[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]('{ Mode.Zip }) }

  @targetName("zip13")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro13[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]('{ Mode.Zip }) }

  @targetName("zip14")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro14[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]('{ Mode.Zip }) }

  @targetName("zip15")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro15[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]('{ Mode.Zip }) }

  @targetName("zip16")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro16[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]('{ Mode.Zip }) }

  @targetName("zip17")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro17[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]('{ Mode.Zip }) }

  @targetName("zip18")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro18[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]('{ Mode.Zip }) }

  @targetName("zip19")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro19[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]('{ Mode.Zip }) }

  @targetName("zip20")
  transparent inline def zip[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind, T20 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro20[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]('{ Mode.Zip }) }

  @targetName("alternatives1")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro1[TFinal, T1]('{ Mode.Alternatives }) }

  @targetName("alternatives2")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro2[TFinal, T1, T2]('{ Mode.Alternatives }) }

  @targetName("alternatives3")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro3[TFinal, T1, T2, T3]('{ Mode.Alternatives }) }

  @targetName("alternatives4")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro4[TFinal, T1, T2, T3, T4]('{ Mode.Alternatives }) }

  @targetName("alternatives5")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro5[TFinal, T1, T2, T3, T4, T5]('{ Mode.Alternatives }) }

  @targetName("alternatives6")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro6[TFinal, T1, T2, T3, T4, T5, T6]('{ Mode.Alternatives }) }

  @targetName("alternatives7")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro7[TFinal, T1, T2, T3, T4, T5, T6, T7]('{ Mode.Alternatives }) }

  @targetName("alternatives8")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro8[TFinal, T1, T2, T3, T4, T5, T6, T7, T8]('{ Mode.Alternatives }) }

  @targetName("alternatives9")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro9[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9]('{ Mode.Alternatives }) }

  @targetName("alternatives10")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro10[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]('{ Mode.Alternatives }) }

  @targetName("alternatives11")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro11[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]('{ Mode.Alternatives }) }

  @targetName("alternatives12")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro12[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]('{ Mode.Alternatives }) }

  @targetName("alternatives13")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro13[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]('{ Mode.Alternatives }) }

  @targetName("alternatives14")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro14[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]('{ Mode.Alternatives }) }

  @targetName("alternatives15")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro15[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]('{ Mode.Alternatives }) }

  @targetName("alternatives16")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro16[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]('{ Mode.Alternatives }) }

  @targetName("alternatives17")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro17[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]('{ Mode.Alternatives }) }

  @targetName("alternatives18")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro18[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]('{ Mode.Alternatives }) }

  @targetName("alternatives19")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro19[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]('{ Mode.Alternatives }) }

  @targetName("alternatives20")
  transparent inline def alternatives[TFinal[_ <: Conf] <: Algm[?, ?], T1 <: AnyKind, T2 <: AnyKind, T3 <: AnyKind, T4 <: AnyKind, T5 <: AnyKind, T6 <: AnyKind, T7 <: AnyKind, T8 <: AnyKind, T9 <: AnyKind, T10 <: AnyKind, T11 <: AnyKind, T12 <: AnyKind, T13 <: AnyKind, T14 <: AnyKind, T15 <: AnyKind, T16 <: AnyKind, T17 <: AnyKind, T18 <: AnyKind, T19 <: AnyKind, T20 <: AnyKind]: Container[TFinal] = ${ Route.routeMacro20[TFinal, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]('{ Mode.Alternatives }) }

}
