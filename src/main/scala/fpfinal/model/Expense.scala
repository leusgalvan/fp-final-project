package fpfinal.model

import cats._
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import fpfinal.app.Configuration.IsValid
import fpfinal.app.Validations._

class Expense private (
    val payer: Person,
    val amount: Money,
    val participants: NonEmptyList[Person],
    val isComputed: Boolean
) {
  def computed: Expense = new Expense(payer, amount, participants, true)
}

object Expense {
  def create(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): IsValid[Expense] = {
    (
      nonEmptyList(participants),
      Validated.condNec(
        !participants.contains(payer),
        payer,
        "payer cannot be included in participants"
      )
    ).mapN { (ps, p) =>
      new Expense(p, amount, ps, false)
    }
  }
}
