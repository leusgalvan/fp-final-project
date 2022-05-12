package fpfinal.model

import cats._
import cats.implicits._

/**
 * This class holds information about how much money is owed by each Person.
 * It does not contain information about the person the money is owed to, only
 * about the people who owes (i.e. the 'payees').
 *
 * For example, if Alan owes 10 dollars and Betty owes 50:
 *
 * allPayees() == List(Alan, Betty)
 *
 * debtForPayee(Alan) == Some($10.00)
 * debtForPayee(Betty) == Some($50.00)
 * debtForPayee(Charly) == None
 *
 * @param debtByPayee a map containing each payee along with their debt
 */
class DebtByPayee private (val debtByPayee: Map[Person, Money]) {

  /**
    * TODO #12: Return the debt for this payee
    */
  def debtForPayee(person: Person): Option[Money] = debtByPayee.get(person)

  /**
    * TODO #13: Return all the payees as a list
    */
  def allPayees(): List[Person] = debtByPayee.keySet.toList
}

object DebtByPayee {
  /**
   * Creates a DebtByPayee instance using the information contained in the map.
   * Should be only used in tests.
   *
   * @param debtByPayee a map containing each payee along with their debt
   */
  def unsafeCreate(debtByPayee: Map[Person, Money]): DebtByPayee =
    new DebtByPayee(debtByPayee)

  /**
    * TODO #14: Create a DebtByPayee instance using the information from this Expense.
    * Each participant should get the same debt to the payer.
    * For simplicity we don't care about losing cents. For example, dividing 1 dollar
    * among 3 participants should yield 33 cents of debt for each participant.
    */
  def fromExpense(expense: Expense): DebtByPayee = new DebtByPayee(
    expense.participants.toList
      .map(p => Map(p -> expense.amountByParticipant))
      .combineAll
  )

  /**
   * Creates an instance of DebtByPayee with exactly one payee and their debt.
   *
   * @param person the payee (person who owes)
   * @param money the amount owed by the payee
   */
  def singleton(person: Person, money: Money): DebtByPayee = new DebtByPayee(Map(person -> money))

  /**
    * TODO #15: Implement an eq instance.
    * Two values are equal iff their debtByPayee maps are equal.
    */
  implicit def eqDebtByPayee(implicit
      eqMap: Eq[Map[Person, Money]]
  ): Eq[DebtByPayee] = Eq.instance { (d1, d2) =>
    d1.debtByPayee eqv d2.debtByPayee
  }

  /**
    * TODO #16: Implement a monoid instance.
    *
    * Hint: Use the monoidMap instance and a suitable method to convert it
    * to the instance you need.
    */
  implicit def monoidDebtByPayee(implicit
      monoidMap: Monoid[Map[Person, Money]]
  ): Monoid[DebtByPayee] =
    monoidMap.imap(m => new DebtByPayee(m))(_.debtByPayee)

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
