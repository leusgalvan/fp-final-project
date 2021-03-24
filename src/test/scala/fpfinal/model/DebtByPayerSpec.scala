package fpfinal.model

import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import fpfinal.FpFinalSpec

class DebtByPayerSpec extends FpFinalSpec {
  test("payer is the payer from the expense") {
    forAll { (expense: Expense) =>
      assert(
        DebtByPayer
          .fromExpense(expense)
          .allPayers() eqv List(expense.payer)
      )
    }
  }

  test("combining yields the union of the payers") {
    forAll { (e1: Expense, e2: Expense) =>
      assert(
        (DebtByPayer.fromExpense(e1) |+| DebtByPayer.fromExpense(e2))
          .allPayers()
          .toSet eqv
          List(e1.payer, e2.payer).toSet
      )
    }
  }

  test("the debt of the combination is the combination of the debts") {
    forAll { (e1: Expense, e2: Expense) =>
      val d1 = DebtByPayer.fromExpense(e1)
      val d2 = DebtByPayer.fromExpense(e2)
      val d = d1 |+| d2
      assert(d.allPayers().forall { p =>
        d.debtForPayer(p) eqv d1.debtForPayer(p) |+| d2.debtForPayer(p)
      })
    }
  }

  // Typeclass instances tests here...
  checkAll("Eq[DebtByPayer]", EqTests[DebtByPayer].eqv)
}
