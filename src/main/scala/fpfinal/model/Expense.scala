package fpfinal.model

import cats._
import cats.data._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

import scala.collection.immutable.SortedSet

class Expense private (
    val payer: Person,
    val amount: Money,
    val participants: NonEmptySet[Person]
) {

  /**
    * TODO: Divide this amount accross the payer and the participants so each
    * has the same debt for this expense
    */
  def amountByParticipant: Money = ???
}

object Expense {
  def unsafeCreate(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): Expense =
    new Expense(
      payer,
      amount,
      NonEmptySet.fromSetUnsafe(SortedSet.from(participants))
    )

  /**
    * TODO: Create a validated expense. The validations to perform are:
    * - The participants list should not be empty
    * - The payer should not be included in the participants
    *
    * Note: List's contains method will use == equality. Figure out a way
    * to use the equality instance received in the implicit argument eqPerson
    */
  def create(
      payer: Person,
      amount: Money,
      participants: List[Person]
  )(implicit eqPerson: Eq[Person]): IsValid[Expense] = ???

  /**
    * TODO: Implement an Eq instance by comparing every field.
    * Parameterize it so that we can pass definitions of Eq for
    * each of the types we need to compare (i.e.: Person, Money, NonEmptySet[Person]).
    */
  implicit def eqExpense = ???
}
