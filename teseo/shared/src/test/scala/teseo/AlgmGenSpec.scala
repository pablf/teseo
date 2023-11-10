package teseo

import org.scalatest.funsuite.*

class AlgmGenSpec extends AnyFunSuite {

  test("AlgmGen.pipeline") {
    trait Sum[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait A[C <: Conf]   extends Algm[Int, Int] { type CC = C }

    object A {
      case class AImpl[C <: Conf](alg: AlgmImpl[Int, Int, C]) extends A[C]
    }

    given Sum[Conf.Default[Option[Nothing]]] with
      override val alg = AlgmImpl.const[Int, Int](x => x + 1)

    assertCompiles("AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]")
    assertDoesNotCompile("AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]")

    /*
    generation with

def sums(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield s"Sum"
  a.mkString(", ")
}

def givens(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield "{\n  " ++ s"val given$i = AlgmGen.pipeline[A, ${sums(i)}]" ++ s"\n  import given$i.given\n  assert((summon[A[_]] ex 0) == $i)\n}"
  a.mkString("\n\n")
}

println(givens(21))
     */

    {
      val given1 = AlgmGen.pipeline[A, Sum]
      import given1.given
      assert((summon[A[Conf.Wrap[_, Sum]]] ex 0) == 1)
    }

    {
      val given2 = AlgmGen.pipeline[A, Sum, Sum]
      import given2.given
      assert((summon[A[_]] ex 0) == 2)
    }

    {
      val given3 = AlgmGen.pipeline[A, Sum, Sum, Sum]
      import given3.given
      assert((summon[A[_]] ex 0) == 3)
    }

    {
      val given4 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum]
      import given4.given
      assert((summon[A[_]] ex 0) == 4)
    }

    {
      val given5 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum]
      import given5.given
      assert((summon[A[_]] ex 0) == 5)
    }

    {
      val given6 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum]
      import given6.given
      assert((summon[A[_]] ex 0) == 6)
    }

    {
      val given7 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given7.given
      assert((summon[A[_]] ex 0) == 7)
    }

    {
      val given8 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given8.given
      assert((summon[A[_]] ex 0) == 8)
    }

    {
      val given9 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given9.given
      assert((summon[A[_]] ex 0) == 9)
    }

    {
      val given10 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given10.given
      assert((summon[A[_]] ex 0) == 10)
    }

    {
      val given11 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given11.given
      assert((summon[A[_]] ex 0) == 11)
    }

    {
      val given12 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given12.given
      assert((summon[A[_]] ex 0) == 12)
    }

    {
      val given13 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given13.given
      assert((summon[A[_]] ex 0) == 13)
    }

    {
      val given14 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given14.given
      assert((summon[A[_]] ex 0) == 14)
    }

    {
      val given15 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given15.given
      assert((summon[A[_]] ex 0) == 15)
    }

    {
      val given16 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given16.given
      assert((summon[A[_]] ex 0) == 16)
    }

    {
      val given17 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given17.given
      assert((summon[A[_]] ex 0) == 17)
    }

    {
      val given18 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given18.given
      assert((summon[A[_]] ex 0) == 18)
    }

    {
      val given19 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given19.given
      assert((summon[A[_]] ex 0) == 19)
    }

    {
      val given20 = AlgmGen.pipeline[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given20.given
      assert((summon[A[_]] ex 0) == 20)
    }

  }

  test("AlgmGen.alternatives") {

    trait A[C <: Conf] extends Algm[Int, Int] { type CC = C }

    object A {
      case class AImpl[C <: Conf](alg: AlgmImpl[Int, Int, C]) extends A[C]
    }

    /*
generation with


def classes(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield s"trait Sum$i[C <: Conf] extends Algm[Int, Int] { type CC = C }"
  a.mkString("\n") ++ "\n"
}

def instance(i: Int): String = s"        given Sum$i[Conf.Default[Option[Nothing]]] with\n            override val alg = AlgmImpl.const[Int, Int](x => x + $i)"

def asserts(i: Int): String = {
  val a1 = "\n    {\n" ++ instance(1) ++ s"\n\n        assert((alg ex 0) == 1)\n    }"
  val ai = "\n    {\n" ++ instance(i) ++ s"\n\n        assert((alg ex 0) == $i)\n    }"
  a1 ++ "\n\n" ++ ai
}

def sums(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield s"Sum$i"
  a.mkString(", ")
}

def givens(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield "{\n    " ++ s"val given$i = AlgmGen.alternatives[A, ${sums(i)}]\n    import given$i.given\n    val alg = summon[A[_]]\n" ++ asserts(i) ++ s"\n\n}"
  a.mkString("\n\n")
}

println(classes(20) ++ "\n" ++ givens(20))
     */

    trait Sum1[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum2[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum3[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum4[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum5[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum6[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum7[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum8[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum9[C <: Conf]  extends Algm[Int, Int] { type CC = C }
    trait Sum10[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum11[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum12[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum13[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum14[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum15[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum16[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum17[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum18[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum19[C <: Conf] extends Algm[Int, Int] { type CC = C }
    trait Sum20[C <: Conf] extends Algm[Int, Int] { type CC = C }

    {
      val given1 = AlgmGen.alternatives[A, Sum1]
      import given1.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

    }

    {
      val given2 = AlgmGen.alternatives[A, Sum1, Sum2]
      import given2.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum2[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 2)

        assert((alg ex 0) == 2)
      }

    }

    {
      val given3 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3]
      import given3.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum3[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 3)

        assert((alg ex 0) == 3)
      }

    }

    {
      val given4 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4]
      import given4.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum4[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 4)

        assert((alg ex 0) == 4)
      }

    }

    {
      val given5 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5]
      import given5.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum5[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 5)

        assert((alg ex 0) == 5)
      }

    }

    {
      val given6 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6]
      import given6.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum6[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 6)

        assert((alg ex 0) == 6)
      }

    }

    {
      val given7 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7]
      import given7.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum7[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 7)

        assert((alg ex 0) == 7)
      }

    }

    {
      val given8 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8]
      import given8.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum8[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 8)

        assert((alg ex 0) == 8)
      }

    }

    {
      val given9 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9]
      import given9.given
      val alg    = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum9[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 9)

        assert((alg ex 0) == 9)
      }

    }

    {
      val given10 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10]
      import given10.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum10[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 10)

        assert((alg ex 0) == 10)
      }

    }

    {
      val given11 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11]
      import given11.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum11[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 11)

        assert((alg ex 0) == 11)
      }

    }

    {
      val given12 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12]
      import given12.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum12[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 12)

        assert((alg ex 0) == 12)
      }

    }

    {
      val given13 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13]
      import given13.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum13[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 13)

        assert((alg ex 0) == 13)
      }

    }

    {
      val given14 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14]
      import given14.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum14[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 14)

        assert((alg ex 0) == 14)
      }

    }

    {
      val given15 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15]
      import given15.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum15[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 15)

        assert((alg ex 0) == 15)
      }

    }

    {
      val given16 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15, Sum16]
      import given16.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum16[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 16)

        assert((alg ex 0) == 16)
      }

    }

    {
      val given17 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15, Sum16, Sum17]
      import given17.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum17[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 17)

        assert((alg ex 0) == 17)
      }

    }

    {
      val given18 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15, Sum16, Sum17, Sum18]
      import given18.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum18[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 18)

        assert((alg ex 0) == 18)
      }

    }

    {
      val given19 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15, Sum16, Sum17, Sum18, Sum19]
      import given19.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum19[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 19)

        assert((alg ex 0) == 19)
      }

    }

    {
      val given20 = AlgmGen.alternatives[A, Sum1, Sum2, Sum3, Sum4, Sum5, Sum6, Sum7, Sum8, Sum9, Sum10, Sum11, Sum12, Sum13, Sum14, Sum15, Sum16, Sum17, Sum18, Sum19, Sum20]
      import given20.given
      val alg     = summon[A[_]]

      {
        given Sum1[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 1)

        assert((alg ex 0) == 1)
      }

      {
        given Sum20[Conf.Default[Option[Nothing]]] with
          override val alg = AlgmImpl.const[Int, Int](x => x + 20)

        assert((alg ex 0) == 20)
      }

    }

  }

  test("AlgmGen.zip") {

    trait Sum[C <: Conf] extends Algm[Int, Int] { type CC = C }

    given Sum[Conf.Default[Option[Nothing]]] with
      override val alg = AlgmImpl.const[Int, Int](x => x + 1)

    {
      trait A[C <: Conf]
          extends Algm[
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
          ]    { type CC = C }
      object A {
        case class AImpl[C <: Conf](
            alg: AlgmImpl[
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              C
            ]
        ) extends A[C]
      }

      assertDoesNotCompile("AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]")
    }

    /*
    generation with

def sums(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield s"Sum"
  a.mkString(", ")
}

def res(i: Int, name: String): String = {
  val a = for {
    i <- 1 to i
  } yield name
  a.mkString("(", ", ", ")")
}

def aTrait(i: Int): String = {
  val a1 = s"""  trait A[C <: Conf] extends Algm[${res(i, "Int")}, ${res(i, "Int")}] { type CC = C }\n"""
  val a2 = "  object A {\n"
  val a3 = s"""    case class AImpl[C <: Conf](alg: AlgmImpl[${res(i, "Int")}, ${res(i, "Int")}, C]) extends A[C]\n"""
  val a4 = "  }\n"
  a1 ++ a2 ++ a3 ++ a4
}

def givens(i: Int): String = {
  val a = for {
    i <- 1 to i
  } yield "{\n" ++ aTrait(i) ++ "\n  " ++s"val given$i = AlgmGen.zip[A, ${sums(i)}]" ++ s"\n  import given$i.given\n  assert((summon[A[_]] ex ${res(i, 0.toString())}) == ${res(i, 1.toString())})\n}"
  a.mkString("\n\n")
}

println(givens(20))
     */

    {
      trait A[C <: Conf] extends Algm[(Int), (Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int), (Int), C]) extends A[C]
      }

      val given1 = AlgmGen.zip[A, Sum]
      import given1.given
      assert((summon[A[_]] ex (0)) == (1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int), (Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int), (Int, Int), C]) extends A[C]
      }

      val given2 = AlgmGen.zip[A, Sum, Sum]
      import given2.given
      assert((summon[A[_]] ex (0, 0)) == (1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int), (Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int), (Int, Int, Int), C]) extends A[C]
      }

      val given3 = AlgmGen.zip[A, Sum, Sum, Sum]
      import given3.given
      assert((summon[A[_]] ex (0, 0, 0)) == (1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int), (Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int), (Int, Int, Int, Int), C]) extends A[C]
      }

      val given4 = AlgmGen.zip[A, Sum, Sum, Sum, Sum]
      import given4.given
      assert((summon[A[_]] ex (0, 0, 0, 0)) == (1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given5 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum]
      import given5.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given6 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum]
      import given6.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given7 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given7.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given8 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given8.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given9 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given9.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given10 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given10.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given11 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given11.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given12 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given12.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given13 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given13.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given14 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given14.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given15 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given15.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given16 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given16.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given17 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given17.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf] extends Algm[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] { type CC = C }
      object A {
        case class AImpl[C <: Conf](alg: AlgmImpl[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), C]) extends A[C]
      }

      val given18 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given18.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf]
          extends Algm[
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
          ]    { type CC = C }
      object A {
        case class AImpl[C <: Conf](
            alg: AlgmImpl[
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              C
            ]
        ) extends A[C]
      }

      val given19 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given19.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    {
      trait A[C <: Conf]
          extends Algm[
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
            (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
          ]    { type CC = C }
      object A {
        case class AImpl[C <: Conf](
            alg: AlgmImpl[
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
              C
            ]
        ) extends A[C]
      }

      val given20 = AlgmGen.zip[A, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum, Sum]
      import given20.given
      assert((summon[A[_]] ex (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

  }

}
