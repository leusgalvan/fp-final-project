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

  test("simplify removes cases where A and B owe each other") {
    forAll { (p1: Person, p2: Person, m1: Money, m2: Money) =>
      val (p2OwesP1, p1OwesP2) = if(m1 < m2) (m2, m1) else (m1, m2)
      val d = DebtByPayer.unsafeCreate(
        Map(
          p1 -> DebtByPayee.unsafeCreate(Map(p2 -> p2OwesP1)),
          p2 -> DebtByPayee.unsafeCreate(Map(p1 -> p1OwesP2))
        )
      ).simplified
      assert(d.debtForPayer(p1).exists(_.debtForPayee(p2).exists(_ eqv (p2OwesP1 minus p1OwesP2))))
      assert(d.debtForPayer(p2).isEmpty)
    }
  }

  test("simplify works when there are no cases where A and B owe each other") {
    forAll { (p1: Person, p2: Person, m: Money) =>
      val d = DebtByPayer.unsafeCreate(Map(p1 -> DebtByPayee.unsafeCreate(Map(p2 -> m)))).simplified
      assert(d.debtForPayer(p1).exists(_.debtForPayee(p2).exists(_ eqv m)))
      assert(d.debtForPayer(p2).isEmpty)
    }
  }

  // TODO #21: Add the missing typeclass tests for Monoid
  checkAll("Eq[DebtByPayer]", EqTests[DebtByPayer].eqv)
  checkAll("Monoid[DebtByPayer]", MonoidTests[DebtByPayer].monoid)
}
