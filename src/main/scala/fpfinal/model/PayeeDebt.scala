package fpfinal.model

import cats._
import cats.implicits._

case class PayeeDebt private (debtByPayee: Map[Person, Money]) {
  def debtForPayee(person: Person): Option[Money] = debtByPayee.get(person)
}

object PayeeDebt {
  def fromExpense(expense: Expense): PayeeDebt =
    PayeeDebt {
      expense.participants
        .map(p => Map(p -> expense.amountByParticipant))
        .combineAll
    }

  implicit val monoidPayeeDebt: Monoid[PayeeDebt] =
    Monoid[Map[Person, Money]].imap(PayeeDebt.apply)(_.debtByPayee)
}
