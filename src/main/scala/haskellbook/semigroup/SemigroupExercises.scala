package haskellbook.semigroup

import cats.Show
import cats.implicits._
import cats.kernel.Semigroup

object SemigroupExercises extends App {

  case class IntegerWrapper(a: Int) extends AnyVal

  object IntegerWrapper {
    implicit def integerWrapperInstance: Semigroup[IntegerWrapper] =
      (a: IntegerWrapper, b: IntegerWrapper) => IntegerWrapper(implicitly[Semigroup[Int]].combine(a.a, b.a))
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

  case class Identity[A](a: A)

  object Identity {
    implicit def identitySemigroupInstance[A](implicit semiA: Semigroup[A]): Semigroup[Identity[A]] =
      (x: Identity[A], y: Identity[A]) =>
        Identity(
          semiA.combine(x.a, y.a)
      )
  }

  case class BoolConj(a: Boolean)

  object BoolConj {
    implicit def semigroupInstanceForBoolConj: Semigroup[BoolConj] =
      (a: BoolConj, b: BoolConj) => BoolConj(a.a && b.a)
  }

  class Trivial
  object Trivial {
    implicit def trivialSemigroupInstance: Semigroup[Trivial] = (x: Trivial, y: Trivial) => x

    implicit def showTrivial: Show[Trivial] =
      (a: Trivial) => s"Tostring for Trivial using cats.Show ${Trivial.getClass.getSimpleName}"
  }

  sealed trait Or[+A, +B]
  object Or {
    case class Fst[A, B](a: A) extends Or[A, B]
    case class Snd[A, B](b: B) extends Or[A, B]

    implicit def orSemigroupInstance[A, B]: Semigroup[Or[A, B]] = (x: Or[A, B], y: Or[A, B]) =>
        (x, y) match {
          case (Fst(a), snd@Fst(b)) => snd
          case (Fst(a), snd@Snd(b)) => snd
          case (Snd(a), snd@Snd(b)) => snd
          case (first@Snd(a), _) => first
      }
  }

  sealed trait Validation[+A, +B]
  object Validation {
    case class MyFailure[A, B](errors: Seq[A]) extends Validation[A, B]
    case class MySuccess[A, B](b: B) extends Validation[A, B]

    implicit def semiGroupInstanceValidation[A, B](implicit combineB: Semigroup[B]): Semigroup[Validation[A, B]] = (a: Validation[A, B], b: Validation[A, B]) => 
      (a, b) match {
        case (MyFailure(l), MyFailure(r)) => MyFailure(l ++ r)
        case (f@MyFailure(_), _) => f
        case (_, f@MyFailure(_)) => f
        case (MySuccess(a), MySuccess(b)) => MySuccess(combineB.combine(a, b))
      }
  }


}
