package fpfinal.model

import cats._
import cats.implicits._

case class PayerDebt private (debtByPerson: Map[Person, PayeeDebt]) {
  def debtForPayer(person: Person): Option[PayeeDebt] = debtByPerson.get(person)
}

object PayerDebt {
  def fromExpense(expense: Expense): PayerDebt =
    PayerDebt(Map(expense.payer -> PayeeDebt.fromExpense(expense)))

  implicit val monoidPayerDebt: Monoid[PayerDebt] =
    Monoid[Map[Person, PayeeDebt]].imap(PayerDebt.apply)(_.debtByPerson)
}
