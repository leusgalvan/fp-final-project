package fpfinal.model

import cats._
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

class Expense private (
    val payer: Person,
    val amount: Money,
    val participants: NonEmptyList[Person]
) {
  def amountByParticipant: Money = amount.divideBy(participants.length + 1)
}

object Expense {
  def unsafeCreate(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): Expense = new Expense(payer, amount, participants.toNel.get)

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
      new Expense(p, amount, ps)
    }
  }

  implicit def eqExpense(implicit
      eqPerson: Eq[Person],
      eqMoney: Eq[Money],
      eqParticipants: Eq[List[Person]]
  ): Eq[Expense] =
    Eq.instance((e1, e2) =>
      e1.payer === e2.payer && e1.amount === e2.amount && e1.participants === e2.participants
    )
}
