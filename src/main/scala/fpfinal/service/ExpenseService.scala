package fpfinal.service

import cats.data._
import fpfinal.model.Expense

trait ExpenseService {
  type ExpenseOp[A] = State[ExpenseState, A]

  case class ExpenseState(
      expenses: List[Expense]
  ) {
    def addExpense(expense: Expense): ExpenseState =
      copy(expenses = expense :: expenses)
  }

  val expenseService: Service

  trait Service {
    def addExpense(expense: Expense): ExpenseOp[Expense]
  }
}

trait LiveExpenseService extends ExpenseService {
  override val expenseService: Service = new Service {
    override def addExpense(
        expense: Expense
    ): ExpenseOp[Expense] = {
      State(s => (s.addExpense(expense), expense))
    }
  }
}
