package fpfinal.model

import cats._
import cats.implicits._

class DebtByPayee private (val debtByPayee: Map[Person, Money]) {
  def debtForPayee(person: Person): Option[Money] = debtByPayee.get(person)
  def allPayees(): List[Person] = debtByPayee.keySet.toList
}

object DebtByPayee {
  def unsafeCreate(debtByPayee: Map[Person, Money]): DebtByPayee =
    new DebtByPayee(debtByPayee)

  def fromExpense(expense: Expense): DebtByPayee =
    new DebtByPayee({
      expense.participants
        .map(p => Map(p -> expense.amountByParticipant))
        .combineAll
    })

  implicit def eqDebtByPayee(implicit
      eqM: Eq[Map[Person, Money]]
  ): Eq[DebtByPayee] =
    Eq.instance { (d1, d2) =>
      d1.debtByPayee eqv d2.debtByPayee
    }

  implicit val monoidDebtByPayee: Monoid[DebtByPayee] =
    Monoid[Map[Person, Money]].imap(x => new DebtByPayee(x))(_.debtByPayee)

  implicit val showDebtByPayee: Show[DebtByPayee] = Show.show { d =>
    d.allPayees()
      .foldMap(payee =>
        s"- ${payee.show}: ${d.debtForPayee(payee).getOrElse(Money.zero).show}\n"
      )
  }
}
