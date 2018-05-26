package semigroup

import cats.Eq
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import haskellbook.semigroup.SemigroupExercises.{Four, Identity}
import org.scalacheck.Arbitrary

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

  checkAll("semigroup laws for data Four a b c d", SemigroupTests[Four[Int, Int, Int, Int]].semigroup)


  implicit def eqIdentity[A: Eq]: Eq[Identity[A]] = Eq.fromUniversalEquals
  implicit def arbitraryIdentity[A: Arbitrary]: Arbitrary[Identity[A]] =
    Arbitrary(
      for {
        a <- Arbitrary.arbitrary[A]
      } yield Identity(a)
    )

  checkAll("semigroup laws for Identity a", SemigroupTests[Identity[Int]].semigroup)
}
