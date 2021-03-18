package fpfinal.model

import cats._
import cats.implicits._

case class PayerDebt private (debtByPerson: Map[Person, PayeeDebt]) {
  def debtForPayer(person: Person): Option[PayeeDebt] = debtByPerson.get(person)
  def allPayers(): List[Person] = debtByPerson.keySet.toList
}

object PayerDebt {
  def fromExpense(expense: Expense): PayerDebt =
    PayerDebt(Map(expense.payer -> PayeeDebt.fromExpense(expense)))

  implicit val monoidPayerDebt: Monoid[PayerDebt] =
    Monoid[Map[Person, PayeeDebt]].imap(PayerDebt.apply)(_.debtByPerson)

  implicit val showPayerDebt: Show[PayerDebt] = Show.show { pd =>
    s"""
       |*****************
       |* Debt by payer *
       |*****************
       |
       |${pd
      .allPayers()
      .foldMap(payer => s"${payer.show}:\n" + pd.debtForPayer(payer).show)}
       |""".stripMargin
  }
}
