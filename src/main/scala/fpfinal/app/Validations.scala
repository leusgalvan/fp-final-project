package fpfinal.app

import cats.data._
import cats.implicits._
import fpfinal.app.Configuration.IsValid

object Validations {
  def double(s: String): IsValid[Double] =
    Validated.fromOption(s.toDoubleOption, NonEmptyChain("Invalid double"))

  def positive(x: Double): IsValid[Double] =
    Validated.condNec(x > 0, x, s"$x is not positive")

  def nonEmptyList[A](list: List[A]): IsValid[NonEmptyList[A]] =
    Validated.fromOption(list.toNel, NonEmptyChain("List is empty"))

  def nonEmptyString(s: String): IsValid[String] =
    Validated.condNec(s.nonEmpty, s, "String is empty")
}
