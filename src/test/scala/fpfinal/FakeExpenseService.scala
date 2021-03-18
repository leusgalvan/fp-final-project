package fpfinal

import cats.implicits._
import cats._
import fpfinal.model.{Expense, PayerDebt}
import fpfinal.service.ExpenseService
import fpfinal.service.ExpenseService.ExpenseOp

trait FakeExpenseService extends ExpenseService {

  var callsToAddExpense = 0
  var callsToComputeDebt = 0

  override val expenseService: Service = new Service {
    override def addExpense(expense: Expense): ExpenseOp[Expense] = {
      callsToAddExpense += 1
      expense.pure[ExpenseOp]
    }

    override def computeDebt(): ExpenseOp[PayerDebt] = {
      callsToComputeDebt += 1
      Monoid[PayerDebt].empty.pure[ExpenseOp]
    }
  }

}
