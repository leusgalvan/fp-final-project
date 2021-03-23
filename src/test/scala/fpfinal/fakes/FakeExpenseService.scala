package fpfinal.fakes

import cats.Monoid
import cats.implicits._
import fpfinal.model.{DebtByPayer, Expense}
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

    override def computeDebt(): ExpenseOp[DebtByPayer] = {
      callsToComputeDebt += 1
      Monoid[DebtByPayer].empty.pure[ExpenseOp]
    }
  }

}
