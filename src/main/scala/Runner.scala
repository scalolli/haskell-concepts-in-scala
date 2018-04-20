

object Runner extends App {

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

//  val y: Tagged[Long] = foo(1)

  def foo[T](a: Tagged[T])(implicit ev: a.Tag =:= Long) : Tagged[Long] = a.asInstanceOf[Tagged[Long]]

  type @@[A, T] = A with Tagged[T]

  object Tag {
    def Tag[A, T](a: A) = a.asInstanceOf[@@[A, T]]
  }

  sealed trait Multiplication


}
