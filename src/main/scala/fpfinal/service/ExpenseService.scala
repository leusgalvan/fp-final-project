package fpfinal.service

import cats._
import cats.data._
import cats.implicits._
import fpfinal.model.{DebtByPayer, Expense}

trait ExpenseService {
  import ExpenseService._

  val expenseService: Service

  trait Service {
    def addExpense(expense: Expense): ExpenseOp[Expense]
    def computeDebt(): ExpenseOp[DebtByPayer]
  }
}

object ExpenseService {
  type ExpenseOp[A] = State[ExpenseState, A]
  case class ExpenseState(
      expenses: List[Expense]
  ) {
    def addExpense(expense: Expense): ExpenseState =
      copy(expenses = expense :: expenses)
  }
  object ExpenseState {
    def empty: ExpenseState = ExpenseState(Nil)

    implicit def eqExpenseState(implicit
        eqExpense: Eq[Expense]
    ): Eq[ExpenseState] =
      Eq.instance((es1, es2) => es1.expenses === es2.expenses)
  }
}

/**
  * TODO: Implement a LiveExpenseService to implement the ExpenseService trait.
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
// trait LiveExpenseService...
