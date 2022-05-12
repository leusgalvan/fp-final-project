package fpfinal.model

import cats._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import fpfinal.model.Person.showPerson

/**
 * A class for representing a person.
 *
 * @param name the name of the person
 */
class Person private (val name: String) {
  /**
   * @return a string representation of this person which just shows the name
   */
  override def toString: String = showPerson.show(this)
}

object Person {
  /**
   * Creates an instance of Money without performing any validations.
   * Should only be used in tests.
   *
   * @param name the amount expressed in cents
   */
  def unsafeCreate(name: String): Person = new Person(name)

  /**
    * TODO #3d: Create a validated instance of Person. There are three validations:
    * - The name should not be empty
    * - The name should only contain letters
    * - The name should be at most 32 chars long
    */
  def create(name: String): IsValid[Person] =
    (allLetters(name), nonEmptyString(name), maxLength(name, 32)).mapN(
      (_, _, _) => new Person(name)
    )

  implicit val showPerson: Show[Person] = Show.show(_.name)

  implicit def eqPerson(implicit eqString: Eq[String]): Eq[Person] =
    Eq.instance((p1, p2) => p1.name === p2.name)

  implicit def ordPerson(implicit ordString: Order[String]): Order[Person] =
    Order.by(_.name)
}
