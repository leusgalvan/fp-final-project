package fpfinal.model

import cats._
import cats.implicits._

class DebtByPayee private (val debtByPayee: Map[Person, Money]) {

  /**
    * TODO: Return the debt for this payee
    */
  def debtForPayee(person: Person): Option[Money] = ???

  /**
    * TODO: Return all the payees as a list
    */
  def allPayees(): List[Person] = ???
}

object DebtByPayee {
  def unsafeCreate(debtByPayee: Map[Person, Money]): DebtByPayee =
    new DebtByPayee(debtByPayee)

  /**
    * TODO: Create a DebtByPayee instance using the information from this Expense.
    * Each participant should get the same debt to the payer.
    */
  def fromExpense(expense: Expense): DebtByPayee = ???

  /**
    * TODO: Implement an eq instance and their corresponding tests.
    * Two values are equal iff their debtByPayee maps are equal.
    */
  implicit def eqDebtByPayee(implicit
      eqMap: Eq[Map[Person, Money]]
  ): Eq[DebtByPayee] = ???

  /**
    * TODO: Implement a monoid instance.
    *
    * Hint: Use the monoidMap instance and a suitable method to convert it
    * to the instance you need.
    */
  implicit def monoidDebtByPayee(implicit
      monoidMap: Monoid[Map[Person, Money]]
  ): Monoid[DebtByPayee] = ???

  implicit def showDebtByPayee(implicit
      personShow: Show[Person],
      moneyShow: Show[Money]
  ): Show[DebtByPayee] =
    Show.show { d =>
      d.allPayees()
        .foldMap(payee =>
          s"- ${payee.show}: ${d.debtForPayee(payee).getOrElse(Money.zero).show}\n"
        )
    }
}
