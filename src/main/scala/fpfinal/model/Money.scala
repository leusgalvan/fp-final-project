package fpfinal.model

import cats._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import fpfinal.model.Money.showMoney

import scala.util.Try

class Money private (_cents: Int) {
  def cents: Int = _cents
  def dollars: Double = _cents / 100.0
  def plus(other: Money): Money = new Money(_cents + other.cents)
  def minus(other: Money): Money = new Money(_cents - other.cents)
  def times(n: Int): Money = new Money(_cents * n)

  /**
    * TODO: Implement division by an integer. When n is 0, the result should be None.
    * Otherwise, it should be a Some.
    */
  def divideBy(n: Int): Option[Money] = ???

  override def toString: String = showMoney.show(this)
}

object Money {
  def unsafeCreate(cents: Int): Money = new Money(cents)

  val zero: Money = new Money(0)

  /**
    * TODO: Create a validated Money object that represents this dollars amount.
    * The only validation to perform is:
    * - Amount should be non-negative
    */
  def dollars(amount: Double): IsValid[Money] = ???

  implicit val monoidMoney: Monoid[Money] = Monoid.instance(zero, _ plus _)

  implicit val showMoney: Show[Money] = Show.show(m => f"$$${m.dollars}%.2f")

  implicit def eqMoney(implicit eqInt: Eq[Int]): Eq[Money] =
    Eq.instance((m1, m2) => m1.cents === m2.cents)

  /**
    * TODO: Implement and instance of Order for Money that compares its cents.
    * We should we able to use any Order[Int] we provide as an implicit parameter.
    */
  implicit def orderMoney = ???
}
