package haskell

import cats.implicits._
import cats.kernel.Semigroup

object SemigroupExercises extends App {

  case class IntegerWrapper(a: Int) extends AnyVal

  object IntegerWrapper {
    implicit def integerWrapperInstance(implicit intInstance: Semigroup[Int]): Semigroup[IntegerWrapper] =
      (a: IntegerWrapper, b: IntegerWrapper) => IntegerWrapper(intInstance.combine(a.a, b.a))
  }

  IntegerWrapper(1) |+| IntegerWrapper(2)

  case class Four[A, B, C, D](a: A, b: B, c: C, d: D)

  object Four {

    implicit def fourSemigroupInstance[A, B, C, D](implicit A: Semigroup[A],
                                                   B: Semigroup[B],
                                                   C: Semigroup[C],
                                                   D: Semigroup[D]): Semigroup[Four[A, B, C, D]] =
      (x: Four[A, B, C, D], y: Four[A, B, C, D]) =>
        Four(
          A.combine(x.a, y.a),
          B.combine(x.b, y.b),
          C.combine(x.c, y.c),
          D.combine(x.d, y.d)
      )
  }

  Four(1, 2, 3, 4) |+| Four(2, 3, 4, 5)
  Four("1", "2", "3", "4") |+| Four("2", "3", "4", "5")
}