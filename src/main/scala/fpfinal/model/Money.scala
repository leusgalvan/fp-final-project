package fpfinal.model

import cats._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._
import fpfinal.model.Money.showMoney

import scala.util.Try

/**
 * Simple class for representing money amounts.
 *
 * It operates on cents, in order to keep a better precision.
 *
 * @param _cents the amount in cents
 */
class Money private (_cents: Int) {
  /**
   * @return the amount expressed in cents
   */
  def cents: Int = _cents

  /**
   * @return the amount expressed in dollars
   */
  def dollars: Double = _cents / 100.0

  /**
   * Adds this money amount to another money amount,
   * keeping precision up to the cents.
   */
  def plus(other: Money): Money = new Money(_cents + other.cents)

  /**
   * Substracts a money amount from this money amount,
   * keeping precision up to the cents.
   */
  def minus(other: Money): Money = new Money(_cents - other.cents)

  /**
   * Multiplies this money amount by a constant factor,
   * keeping precision up to the cents.
   */
  def times(n: Int): Money = new Money(_cents * n)

  /**
    * TODO #6: Implement division by an integer. When n is 0, the result should be None.
    * Otherwise, it should be a Some.
    *
    * For simplicity we don't care about losing cents. For example, dividing 1 dollar
    * by 3 should yield 33 cents.
    */
  def divideBy(n: Int): Option[Money] = Try(new Money(_cents / n)).toOption

  /**
   * @return a string representation of this money amount in dollars, with two decimal places
   *         and a dollar sign (e.g. $45.33)
   */
  override def toString: String = showMoney.show(this)
}

object Money {
  /**
   * Creates an instance of Money without performing any validations.
   * Should only be used in tests.
   *
   * @param cents the amount expressed in cents
   */
  def unsafeCreate(cents: Int): Money = new Money(cents)

  val zero: Money = new Money(0)

  /**
    * TODO #7: Create a validated Money object that represents this dollars amount.
    * The only validation to perform is:
    * - Amount should be non-negative
    */
  def dollars(amount: Double): IsValid[Money] =
    nonNegative(amount).map { dls =>
      val cents = (dls * 100).toInt
      new Money(cents)
    }

  implicit val monoidMoney: Monoid[Money] = Monoid.instance(zero, _ plus _)

  implicit val showMoney: Show[Money] = Show.show(m => f"$$${m.dollars}%.2f")

  implicit def eqMoney(implicit eqInt: Eq[Int]): Eq[Money] =
    Eq.instance((m1, m2) => m1.cents === m2.cents)

  /**
    * TODO #4: Implement and instance of Order for Money that compares its cents.
    * Use the given Order instance for comparing any Int values.
    */
  implicit def orderMoney(implicit orderInt: Order[Int]): Order[Money] = Order.by(_.cents)
}
