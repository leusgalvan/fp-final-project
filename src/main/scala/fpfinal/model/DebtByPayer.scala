package fpfinal.model

import cats._
import cats.implicits._

/**
 * This class holds information about how much each person owns each particular payer.
 *
 * For example, suppose Bob owes Alice 10 dollars, and Charly owes Alice 20 dollars. In this
 * case Alice would be the payer, whereas Bob and Charly would be the payees.
 *
 * So if we want to know how much people owe to Alice, we can call debtForPayer(Alice) and get
 * a DebtByPayee object with this information. This object should contain an entry for Bob for
 * 10 dollars, and an entry for Charly for 20 dollars.
 *
 * We can also query all the payers included in this object.
 *
 * @param debtByPerson a map containing each payer along with information about people who owe them
 *                     (this information is in a DebtByPayee object)
 */
class DebtByPayer private (val debtByPerson: Map[Person, DebtByPayee]) {

  /**
    * TODO #18: Get the debt summary by payee for this payer
    */
  def debtForPayer(person: Person): Option[DebtByPayee] = debtByPerson.get(person)

  /**
    * TODO #19: Get all the payers in a list
    */
  def allPayers(): List[Person] = debtByPerson.keySet.toList

  /**
   * Return a simplified version of this DebtByPayer object which does not contain mutual
   * debts.
   *
   * For example, if Alice owes Bob 30 dollars and Bob owes Alice 20 dollars, in the simplified version
   * Alice owes Bob 10 dollars.
   */
  def simplified: DebtByPayer = {
    def payeesFor(person: Person): List[Person] =
      debtByPerson.get(person).toList.flatMap(_.allPayees())

    def owes(p1: Person, p2: Person): Money =
      debtByPerson.get(p2).flatMap(_.debtForPayee(p1)).getOrElse(Money.zero)

    def createDebt(p1: Person, p2: Person, p1OwesP2: Money, p2OwesP1: Money): Option[DebtByPayer] = {
      if(p2OwesP1 > p1OwesP2) {
        val debtByPayer = new DebtByPayer(Map(p1 -> DebtByPayee.singleton(p2, p2OwesP1 minus p1OwesP2)))
        Some(debtByPayer)
      } else None
    }

    val debtByPayes = for {
      p1 <- allPayers()
      p2 <- payeesFor(p1)
      p1OwesP2 = owes(p1, p2)
      p2OwesP1 = owes(p2, p1)
      d <- createDebt(p1, p2, p1OwesP2, p2OwesP1).toList
    } yield d

    debtByPayes.foldMap(identity)
  }
}

object DebtByPayer {
  /**
   * Creates a DebtByPayer instance using the information contained in the map.
   * Should be only used in tests.
   *
   * @param debtByPerson a map containing each payer along with how much people owe them
   */
  def unsafeCreate(debtByPerson: Map[Person, DebtByPayee]): DebtByPayer =
    new DebtByPayer(debtByPerson)

  /**
   * Creates a DebtByPayer from a single expense.
   *
   * The only payer is the payer from the expense. The payees are the participants
   * in the expense, and each owes the same amount (the total amount of the expense
   * divided evenly among them).
   */
  def fromExpense(expense: Expense): DebtByPayer =
    new DebtByPayer(Map(expense.payer -> DebtByPayee.fromExpense(expense)))

  implicit def eqDebtByPayer(implicit
      eqMap: Eq[Map[Person, DebtByPayee]]
  ): Eq[DebtByPayer] =
    Eq.instance((d1, d2) => d1.debtByPerson === d2.debtByPerson)

  /**
    * TODO #20: Implement a monoid instance.
    *
    * Hint: Use the monoidMap instance and a suitable method to convert it
    * to the instance you need.
    */
  implicit def monoidDebtByPayer(implicit
      monoidMap: Monoid[Map[Person, DebtByPayee]]
  ): Monoid[DebtByPayer] =
    monoidMap.imap(m => new DebtByPayer(m))(_.debtByPerson)

  implicit def showDebtByPayer(implicit
      showPerson: Show[Person],
      showDebtByPayee: Show[DebtByPayee]
  ): Show[DebtByPayer] =
    Show.show { debtByPayer =>
      s"""Debt by payer:
       |
       |${debtByPayer
        .allPayers()
        .toNel
        .fold("  No debts found")(
          _.foldMap(payer =>
            s"${payer.show}:\n" + debtByPayer
              .debtForPayer(payer)
              .foldMap(_.show)
          )
        )}""".stripMargin
    }
}
