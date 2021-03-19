package fpfinal.model

import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.implicits._
import org.scalacheck.Prop.forAll

class DebtByPayerSpec extends ModelSpec {
  test("payer is the payer from the expense") {
    forAll { (expense: Expense) =>
      DebtByPayer
        .fromExpense(expense)
        .allPayers() eqv List(expense.payer)
    }
  }

  test("combining yields the union of the payers") {
    forAll { (e1: Expense, e2: Expense) =>
      (DebtByPayer.fromExpense(e1) |+| DebtByPayer.fromExpense(e2))
        .allPayers()
        .toSet eqv
        List(e1.payer, e2.payer).toSet
    }
  }

  test("the debt of the combination is the combination of the debts") {
    forAll { (e1: Expense, e2: Expense) =>
      val d1 = DebtByPayer.fromExpense(e1)
      val d2 = DebtByPayer.fromExpense(e2)
      val d = d1 |+| d2
      d.allPayers().forall { p =>
        d.debtForPayer(p) eqv d1.debtForPayer(p) |+| d2.debtForPayer(p)
      }
    }
  }

  checkAll("Eq[DebtByPayer]", EqTests[DebtByPayer].eqv)
  checkAll("Monoid[DebtByPayer]", MonoidTests[DebtByPayer].monoid)
}
