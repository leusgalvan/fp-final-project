package fpfinal.common

import cats.data.{NonEmptyChain, NonEmptyList, Validated}
import fpfinal.app.Configuration.IsValid
import cats.implicits._

object Validations {
  def double(s: String): IsValid[Double] =
    Validated.fromOption(s.toDoubleOption, NonEmptyChain("Invalid double"))

  def nonNegative(x: Double): IsValid[Double] =
    Validated.condNec(x >= 0, x, s"$x should be nonnegative")

  def nonEmptyList[A](list: List[A]): IsValid[NonEmptyList[A]] =
    Validated.fromOption(list.toNel, NonEmptyChain("List should be non-empty"))

  def nonEmptyString(s: String): IsValid[String] =
    Validated.condNec(s.nonEmpty, s, "String should be non-empty")

  def allLetters(s: String): IsValid[String] =
    Validated.condNec(s.forall(_.isLetter), s, "String should be all letters")
}
