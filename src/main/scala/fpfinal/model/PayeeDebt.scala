package fpfinal.model

import cats._
import cats.implicits._

case class PayeeDebt private (debtByPayee: Map[Person, Money]) {
  def debtForPayee(person: Person): Option[Money] = debtByPayee.get(person)
  def allPayees(): List[Person] = debtByPayee.keySet.toList
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

  implicit val showPayeeDebt: Show[PayeeDebt] = Show.show { pd =>
    pd.allPayees()
      .foldMap(payee =>
        s"- ${payee.show}: ${pd.debtForPayee(payee).getOrElse(Money.zero).show}\n"
      )
  }
}
