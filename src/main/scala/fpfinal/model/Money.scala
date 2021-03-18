package fpfinal.model

import cats._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

class Money private (_cents: Int) {
  def cents: Int = _cents
  def dollars: Double = _cents / 100
  def plus(other: Money): Money = new Money(_cents + other.cents)
  def divideBy(n: Int) = new Money(_cents / n)
}

object Money {
  val zero: Money = new Money(0)

  def dollars(amount: Double): IsValid[Money] =
    positive(amount).map { dls =>
      val cents = (dls * 100).toInt
      new Money(cents)
    }

  implicit val monoidMoney: Monoid[Money] = Monoid.instance(zero, _ plus _)

  implicit val showMoney: Show[Money] = Show.show(m => f"$$${m.dollars}%.2f")
}
