package fpfinal.service

import cats._
import cats.data._
import cats.implicits._
import fpfinal.model.{DebtByPayer, Expense}
import fpfinal.service.ExpenseService.ExpenseOp

/**
 * A trait for computing the debts of all the people involved in the expenses.
 *
 * It uses the State monad to keep track of the expenses that the user of the application
 * has added so far.
 */
trait ExpenseService {
  import ExpenseService._

  val expenseService: Service

  trait Service {
    /**
     * Adds an expense to the state.
     */
    def addExpense(expense: Expense): ExpenseOp[Expense]

    /**
     * Computes the debt for all the people involved, based on the expenses
     * there are in the state.
     */
    def computeDebt(): ExpenseOp[DebtByPayer]
  }
}

object ExpenseService {
  type ExpenseOp[A] = State[ExpenseState, A]

  /**
   * Represents a state containing a list of expenses.
   */
  case class ExpenseState(expenses: List[Expense]) {
    /**
     * @return a new state with the given expense added
     */
    def addExpense(expense: Expense): ExpenseState = copy(expenses = expense :: expenses)
  }

  object ExpenseState {
    /**
     * A state with no expenses.
     */
    def empty: ExpenseState = ExpenseState(Nil)

    implicit def eqExpenseState(implicit eqExpense: Eq[Expense]): Eq[ExpenseState] =
      Eq.instance((es1, es2) => es1.expenses === es2.expenses)
  }
}

/**
  * TODO #23: Implement a LiveExpenseService to implement the ExpenseService trait.
  *
  * The addExpense method should add the given expense to the ExpenseState, and return it
  * as it is.
  *
  * The computeDebt method should use the expenses stored in the state and compute
  * a DebtByPayer object. This object contains a summary of how much is owed to each person.
  * Please don't forget to call 'simplified' in the DebtByPayer instance to remove cases where
  * two people owe each other.
  *
  * For example, if A made an expense of 6 dollars where B and C were included, then each owe A 2 dollars.
  * This means the DebtByPayer object will contain an entry for A as a payer. If we call
  * debtForPayer(A) we'll get back a DebtForPayee object with one entry for B (who owes 2 dollars)
  * and one entry for C (who also owes 2 dollars).
  *
  * Check the tests for a concrete example.
  */
trait LiveExpenseService extends ExpenseService {
  override val expenseService: Service = new Service {
    /**
     * Adds an expense to the state.
     */
    override def addExpense(expense: Expense): ExpenseOp[Expense] =
      State(s => (s.addExpense(expense), expense))

    /**
     * Computes the debt for all the people involved, based on the expenses
     * there are in the state.
     */
    override def computeDebt(): ExpenseOp[DebtByPayer] = {
      State.inspect(_.expenses.foldMap(DebtByPayer.fromExpense).simplified)
    }
  }
}
