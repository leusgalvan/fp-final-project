package fpfinal.model

import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import cats._
import cats.implicits._
import fpfinal.model.Person.showPerson

class Person private (val name: String) {
  override def toString: String = showPerson.show(this)
}

object Person {
  def unsafeCreate(name: String): Person = new Person(name)

  def create(name: String): IsValid[Person] =
    (allLetters(name), nonEmptyString(name)).mapN((_, _) => new Person(name))

  implicit val showPerson: Show[Person] = Show.show(_.name)

  implicit def eqPerson(implicit eqString: Eq[String]): Eq[Person] =
    Eq.instance((p1, p2) => p1.name === p2.name)
}
