package fpfinal.model

import cats._
import cats.implicits._

class DebtByPayer private (val debtByPerson: Map[Person, DebtByPayee]) {
  def debtForPayer(person: Person): Option[DebtByPayee] =
    debtByPerson.get(person)
  def allPayers(): List[Person] = debtByPerson.keySet.toList
}

object DebtByPayer {
  def unsafeCreate(debtByPerson: Map[Person, DebtByPayee]): DebtByPayer =
    new DebtByPayer(debtByPerson)

  def fromExpense(expense: Expense): DebtByPayer =
    new DebtByPayer(Map(expense.payer -> DebtByPayee.fromExpense(expense)))

  implicit def eqDebtByPayer(implicit
      eqMap: Eq[Map[Person, DebtByPayee]]
  ): Eq[DebtByPayer] =
    Eq.instance((d1, d2) => d1.debtByPerson === d2.debtByPerson)

  implicit def monoidDebtByPayer(implicit
      monoidMap: Monoid[Map[Person, DebtByPayee]]
  ): Monoid[DebtByPayer] =
    monoidMap.imap(DebtByPayer.unsafeCreate)(_.debtByPerson)

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
