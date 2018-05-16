import cats.Show
import cats.data.Validated._
import cats.data.ValidatedNel
import cats.implicits._

object ValidationApplicative extends App {

  type Name = String
  type Address = String

  sealed trait ValidationErrors
  object NameTooLong extends ValidationErrors
  object AddressTooLong extends ValidationErrors

  type ValidationResult[A] = ValidatedNel[ValidationErrors, A]

  case class Person(firstName: Name, address: Address)

  def getName(len: Int, rawName: String) : ValidationResult[String] =
    if(rawName.length > len) NameTooLong.invalidNel else rawName.validNel

  def getAddress(len: Int, rawAddress: String): ValidationResult[String] =
    if(rawAddress.length > len) AddressTooLong.invalidNel else rawAddress.validNel

  def mkPerson(n: String, a: String) : ValidationResult[Person] = {
    val person: (Name, Address) => Person = Person.apply
    (getName(10, n), getAddress(100, a)).mapN(person)
  }

  implicit val validationResultShow: Show[ValidationErrors] = Show.show(error => error.getClass.getSimpleName)

  mkPerson("basuuuuuuuuuuuuuu", "London").toEither match {
    case Right(x) => println(x)
    case Left(errors) =>
      println(s"There were errors:")
      println(errors.show)
  }

}
