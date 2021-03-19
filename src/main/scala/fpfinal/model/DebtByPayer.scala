package fpfinal.model

import cats._
import cats.implicits._

case class DebtByPayer private (debtByPerson: Map[Person, DebtByPayee]) {
  def debtForPayer(person: Person): Option[DebtByPayee] =
    debtByPerson.get(person)
  def allPayers(): List[Person] = debtByPerson.keySet.toList
}

object DebtByPayer {
  def fromExpense(expense: Expense): DebtByPayer =
    DebtByPayer(Map(expense.payer -> DebtByPayee.fromExpense(expense)))

  implicit val monoidDebtByPayer: Monoid[DebtByPayer] =
    Monoid[Map[Person, DebtByPayee]].imap(DebtByPayer.apply)(_.debtByPerson)

  implicit val showDebtByPayer: Show[DebtByPayer] = Show.show { pd =>
    s"""
       |*****************
       |* Debt by payer *
       |*****************
       |
       |${pd
      .allPayers()
      .foldMap(payer =>
        s"${payer.show}:\n" + pd.debtForPayer(payer).foldMap(_.show)
      )}
       |""".stripMargin
  }
}
