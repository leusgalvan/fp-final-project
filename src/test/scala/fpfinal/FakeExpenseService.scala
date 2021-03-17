package fpfinal

import cats.implicits._
import fpfinal.model.Expense
import fpfinal.service.ExpenseService
import fpfinal.service.ExpenseService.ExpenseOp

trait FakeExpenseService extends ExpenseService {

  var callsToAddExpense = 0

  override val expenseService: Service = new Service {
    override def addExpense(expense: Expense): ExpenseOp[Expense] = {
      callsToAddExpense += 1
      expense.pure[ExpenseOp]
    }
  }

}
