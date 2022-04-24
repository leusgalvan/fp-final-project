package fpfinal.services

import cats._
import cats.implicits._
import fpfinal.FpFinalSpec
import fpfinal.model._
import fpfinal.service.ExpenseService.ExpenseState
import fpfinal.service.LiveExpenseService

class ExpenseServiceSpec extends FpFinalSpec {
  val service: LiveExpenseService#Service =
    new LiveExpenseService {}.expenseService

  test("add expense adds the expense to the state") {
    forAll { (expense: Expense, currentExpenses: List[Expense]) =>
      assert(
        service
          .addExpense(expense)
          .run(ExpenseState(currentExpenses))
          .value eqv (ExpenseState(expense :: currentExpenses), expense)
      )
    }
  }

  test("computeDebt for no expenses") {
    val initialState = ExpenseState(Nil)
    val expectedDebt = Monoid[DebtByPayer].empty
    val result = service.computeDebt().run(initialState).value
    assert(result eqv (initialState, expectedDebt))
  }

  test("computeDebt for some expenses") {
    val martin = Person.unsafeCreate("Martin")
    val leandro = Person.unsafeCreate("Leandro")
    val eugenia = Person.unsafeCreate("Eugenia")
    val expense1 = Expense.unsafeCreate(
      martin,
      Money.unsafeCreate(1500),
      List(leandro, eugenia)
    )
    val expense2 = Expense.unsafeCreate(
      leandro,
      Money.unsafeCreate(2100),
      List(martin, eugenia)
    )
    val initialState = ExpenseState(List(expense1, expense2))
    val expectedDebt = DebtByPayer.unsafeCreate(
      Map(
        leandro -> DebtByPayee.unsafeCreate(
          Map(
            eugenia -> Money.unsafeCreate(700),
            martin -> Money.unsafeCreate(200)
          )
        ),
        martin -> DebtByPayee.unsafeCreate(
          Map(
            eugenia -> Money.unsafeCreate(500)
          )
        )
      )
    )

    val result = service.computeDebt().run(initialState).value
    assert(result eqv (initialState, expectedDebt))
  }
}
