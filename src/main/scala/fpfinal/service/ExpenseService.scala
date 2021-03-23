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

trait LiveExpenseService extends ExpenseService {
  import ExpenseService._
  override val expenseService: Service = new Service {
    override def addExpense(
        expense: Expense
    ): ExpenseOp[Expense] = {
      State(s => (s.addExpense(expense), expense))
    }

    override def computeDebt(): ExpenseOp[DebtByPayer] =
      State.inspect(_.expenses.foldMap(DebtByPayer.fromExpense))
  }
}
