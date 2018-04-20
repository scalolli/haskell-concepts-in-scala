

object Runner extends App {

  type FName = String

  val Fname = "abc"
  Fname.equals()

  type Tagged[U] = {
      type Tag = U
  }

  println("Hello")

  val x: Tagged[Long] = foo(Long(1))

  def foo[T](a: Tagged[T])(implicit ev: a.Tag = Long) : Tagged[Long] = a.asInstanceOf[Tagged[Long]]

}
