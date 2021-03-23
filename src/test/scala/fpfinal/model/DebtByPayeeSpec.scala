package fpfinal.model

import cats._
import cats.data.NonEmptySet
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import fpfinal.FpFinalSpec
import org.scalacheck.Prop.forAll

import scala.collection.immutable.SortedSet

class DebtByPayeeSpec extends FpFinalSpec {
  test("payees are the participants from the expense") {
    forAll { (expense: Expense) =>
      assert(
        DebtByPayee
          .fromExpense(expense)
          .allPayees()
          .toSet eqv expense.participants.toList.toSet
      )
    }
  }

  test("combining yields the union of the payees") {
    forAll { (e1: Expense, e2: Expense) =>
      val lhs = NonEmptySet.fromSetUnsafe(
        SortedSet.from(
          (DebtByPayee.fromExpense(e1) |+| DebtByPayee.fromExpense(e2))
            .allPayees()
        )
      )
      val rhs = e1.participants |+| e2.participants
      assert(lhs eqv rhs)
    }
  }

  test("combining sums the debts of payees") {
    forAll { (e1: Expense, e2: Expense) =>
      val d1 = DebtByPayee.fromExpense(e1)
      val d2 = DebtByPayee.fromExpense(e2)
      val d = d1 |+| d2
      assert(d.allPayees().forall { p =>
        d.debtForPayee(p) eqv d1.debtForPayee(p) <+> d2.debtForPayee(p)
      })
    }
  }

  test("every participant of the expense get the same debt") {
    forAll { (expense: Expense) =>
      val debtByPayee = DebtByPayee.fromExpense(expense)
      val firstPayee = expense.participants.head
      val firstPayeeDebt = debtByPayee.debtForPayee(firstPayee)
      assert(
        expense.participants.forall(p =>
          debtByPayee.debtForPayee(p) eqv firstPayeeDebt
        )
      )
    }
  }

  checkAll("Eq[DebtByPayee]", EqTests[DebtByPayee].eqv)
  checkAll("Monoid[DebtByPayee]", MonoidTests[DebtByPayee].monoid)
}
