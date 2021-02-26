package fpfinal.model

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import fpfinal.app.Configuration.IsValid

class Expense private (
    payer: Person,
    amount: Money,
    participants: NonEmptySet[Person],
    computed: Boolean
) {
  def computed: Expense = new Expense(payer, amount, participants, true)
}

object Expense {
  def create(
      payer: Person,
      amount: Money,
      participants: NonEmptySet[Person]
  ): IsValid[Expense] = {
    if (participants.contains(payer)) {
      Invalid(
        NonEmptyChain(
          "Invalid expense: payer must not be included in participants"
        )
      )
    } else {
      Valid(new Expense(payer, amount, participants, false))
    }
  }
}
