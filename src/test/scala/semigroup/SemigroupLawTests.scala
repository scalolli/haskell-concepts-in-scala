package semigroup

import cats.Eq
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import haskellbook.semigroup.SemigroupExercises.Four
import org.scalacheck.{Arbitrary, Gen}

class SemigroupLawTests extends CatsSuite {

  implicit def eqFour[A: Eq, B: Eq, C: Eq, D: Eq]: Eq[Four[A, B, C, D]] = Eq.fromUniversalEquals

  implicit def arbFour[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[Four[A, B, C, D]] =
    Arbitrary(
      for{
        a <- Arbitrary.arbitrary[A]
        b <- Arbitrary.arbitrary[B]
        c <- Arbitrary.arbitrary[C]
        d <- Arbitrary.arbitrary[D]
      } yield Four(a, b, c, d)
    )

  checkAll("semigroup laws for Four", SemigroupTests[Four[Int, Int, Int, Int]].semigroup)
}
