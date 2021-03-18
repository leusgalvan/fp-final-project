package fpfinal.model

import fpfinal.app.Configuration.IsValid
import fpfinal.app.Validations._
import cats._
import cats.implicits._

case class Person private (name: String)

object Person {
  def createPerson(name: String): IsValid[Person] =
    (allLetters(name), nonEmptyString(name)).mapN((_, _) => Person(name))
}
