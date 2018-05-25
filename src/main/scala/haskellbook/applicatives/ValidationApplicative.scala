package haskellbook.applicatives

import cats.Show
import cats.data.Validated._
import cats.data.ValidatedNel
import cats.implicits._

object ValidationApplicative extends App {

  type Name = String
  type Address = String
  type ValidationResult[A] = ValidatedNel[ValidationErrors, A]

  sealed trait ValidationErrors
  object ValidationErrors {
    case object NameTooLong extends ValidationErrors
    case object AddressTooLong extends ValidationErrors

    implicit val validationResultShow: Show[ValidationErrors] = Show.show((errors: ValidationErrors) => s"Custom show: $errors")
  }

  case class Person(firstName: Name, address: Address)

  object Person {
    implicit val showPerson: Show[Person] = Show.show((person: Person) => s"Custom show: ${person.firstName}")
  }

  import Person._
  import ValidationErrors._

  def getName(len: Int, rawName: String): ValidationResult[String] =
    if (rawName.length > len) NameTooLong.invalidNel else rawName.validNel

  def getAddress(len: Int, rawAddress: String): ValidationResult[String] =
    if (rawAddress.length > len) AddressTooLong.invalidNel else rawAddress.validNel

  def mkPerson(n: String, a: String): ValidationResult[Person] = {
    (getName(10, n), getAddress(100, a)).mapN(Person.apply)
  }

  val invalidPerson = mkPerson("basuuuuuuuuuuuuuu", "London")
  val validPerson = mkPerson("Basu", "blaaah")

  processValidated(invalidPerson)
  processValidated(validPerson)

  private def processValidated(person: ValidationResult[Person]): Unit =
    person match {
      case Valid(p) =>
        println(s"Display the person using cats show ${p.show}")
      case Invalid(errors) =>
        println(s"There were errors: ${errors.show}")
    }

}
