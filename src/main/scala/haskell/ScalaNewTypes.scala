package haskell

import haskell.Status.{Failed, Success}

object ScalaNewTypes extends App {

  type FName = String

  val Fname = "abc"
  Fname.equals()

  // The reason to have a Tag is to do assertions where we have access to the type for e.g below
  // we cannot pass to foo(Integer(1)) but foo(Long(1)) works
  type Tagged[U] = {
    type Tag = U
  }

  println("Hello")

  val x = 1.asInstanceOf[Tagged[Long]]
  foo(x)

  //  this wont work
  //  val y: Tagged[Long] = foo(1)

  def foo[T](a: Tagged[T])(implicit ev: a.Tag =:= Long): Tagged[Long] = a.asInstanceOf[Tagged[Long]]

  type @@[A, T] = A with Tagged[T]

  object Tag {
    def apply[A, T](a: A) = a.asInstanceOf[@@[A, T]]
  }

  sealed trait Multiplication

  private val oneMultiplicationNewTypeForInt: @@[Int, Multiplication] = Tag[Int, Multiplication](2)
  private val twoMultiplicationNewTypeForInt: @@[Int, Multiplication] = Tag[Int, Multiplication](2)

  val result = |+|(oneMultiplicationNewTypeForInt, twoMultiplicationNewTypeForInt)

  def |+|(a: @@[Int, Multiplication], b: @@[Int, Multiplication]): @@[Int, Multiplication] = Tag(a * b)

  print(s"Its still just a int: $result")

  val status = Status.Failed

  patternMatch(status)

  def patternMatch(status: Status): Unit = {
    status match {
      case s@Success => println(s)
      case s@Failed => println(s)
    }
  }

}
