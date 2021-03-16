package fpfinal.service

import cats.data._
import fpfinal.model.Expense

trait ExpenseService {
  import ExpenseService._

  val expenseService: Service

  trait Service {
    def addExpense(expense: Expense): ExpenseOp[Expense]
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
}

trait LiveExpenseService extends ExpenseService {
  import ExpenseService._
  override val expenseService: Service = new Service {
    override def addExpense(
        expense: Expense
    ): ExpenseOp[Expense] = {
      State(s => (s.addExpense(expense), expense))
    }
  }
}
