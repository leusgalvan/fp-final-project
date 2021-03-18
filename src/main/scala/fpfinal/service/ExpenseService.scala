package fpfinal.service

import cats.data._
import cats._
import cats.implicits._
import fpfinal.model.{Expense, PayerDebt}

trait ExpenseService {
  import ExpenseService._

  val expenseService: Service

  trait Service {
    def addExpense(expense: Expense): ExpenseOp[Expense]
    def computeDebt(): ExpenseOp[PayerDebt]
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

    override def computeDebt(): ExpenseOp[PayerDebt] =
      State.inspect(_.expenses.foldMap(PayerDebt.fromExpense))
  }
}
