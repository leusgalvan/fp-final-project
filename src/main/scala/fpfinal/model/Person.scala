package fpfinal.model

import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import cats._
import cats.implicits._

case class Person private (name: String)

object Person {
  def create(name: String): IsValid[Person] =
    (allLetters(name), nonEmptyString(name)).mapN((_, _) => Person(name))
}
