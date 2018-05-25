package haskell

import cats.kernel.Semigroup
import cats.implicits._

object SemigroupExercises extends App {

  case class Four(a: Int)

  object Four {
    implicit object CustomSemiGroup extends Semigroup[Four] {
      override def combine(x: Four, y: Four): Four = Four(x.a + y.a)
    }
  }

  Four(1) |+| Four(2)
}
