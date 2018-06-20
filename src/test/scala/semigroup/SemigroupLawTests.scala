package semigroup

import cats.Eq
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import haskellbook.semigroup.SemigroupExercises.Or._
import haskellbook.semigroup.SemigroupExercises._
import org.scalacheck.{Arbitrary, Gen}

class SemigroupLawTests extends CatsSuite {

//  TODO: how do we get this to compile ?? Ask in Cats channel?
//  implicit def functorGen: Functor[Gen] = new Functor[Gen] {
//    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)
//  }
//
//  implicit def semigroupGen[A](implicit sA: Semigroup[A]): Semigroup[Gen[A]] = new Semigroup[A] {
//    override def combine(x: Gen[A], y: Gen[A]): A = sA.combine()
//  }
//
//  implicit def arbFour[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[Four[A, B, C, D]] =
//    Arbitrary((Arbitrary.arbitrary[A], Arbitrary.arbitrary[B], Arbitrary.arbitrary[C], Arbitrary.arbitrary[D]).mapN(Four.apply))


  implicit def eqFour[A: Eq, B: Eq, C: Eq, D: Eq]: Eq[Four[A, B, C, D]] = Eq.fromUniversalEquals
  implicit def arbFour[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[Four[A, B, C, D]] =
    Arbitrary(
      for {
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

  implicit def eqBoolConj: Eq[BoolConj]               = Eq.fromUniversalEquals
  implicit def boolConjArbitrary: Arbitrary[BoolConj] = Arbitrary(Gen.oneOf(Seq(true, false)).map(BoolConj(_)))
  checkAll("semigroup laws for BoolConj", SemigroupTests[BoolConj].semigroup)

  implicit def eqTrivial: Eq[Trivial]               = Eq.fromUniversalEquals
  implicit def arbitraryTrivial: Arbitrary[Trivial] = Arbitrary(new Trivial())
  checkAll("semigroup laws for Trivial", SemigroupTests[Trivial].semigroup)

  implicit def eqOr[A: Eq, B: Eq]: Eq[Or[A, B]] = Eq.fromUniversalEquals
  implicit def arbitraryOr[A: Arbitrary, B: Arbitrary]: Arbitrary[Or[A, B]] =
    Arbitrary(Gen.oneOf(Arbitrary.arbitrary[A].map(Fst(_)), Arbitrary.arbitrary[B].map(Snd(_))))

  checkAll("SemiGroup laws for Or", SemigroupTests[Or[Int, String]].semigroup(arbitraryOr, eqOr))

}
